{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The Expansion algorithm — JSON-LD 1.1 API §5.1.
--
-- Spec references in comments use the section numbering from
-- <https://www.w3.org/TR/json-ld11-api/#expansion-algorithms>.
--
-- Currently implemented:
--
--  * Null, scalar, array, and object expansion (§5.1.2)
--  * Top-level @\@graph@-only unwrap and free-floating drop (§5.1.1)
--  * Keywords: @\@id@, @\@type@, @\@value@, @\@language@, @\@index@,
--    @\@list@, @\@set@, @\@graph@, @\@reverse@
--  * Type-scoped (§5.1.2 step 8) and property-scoped (§5.1.2 step
--    13.5) contexts
--  * Type coercion: @\@id@, @\@vocab@, @\@json@, IRI-typed values
--  * Term-level @\@language@ \/ @\@direction@ slots with
--    explicit-null semantics, falling back to active context defaults
--  * Container maps: @\@language@, @\@index@, @\@id@
--
-- Still deferred:
--
--  * Container maps for @\@type@ and @\@graph@
--  * @\@nest@, @\@included@
--  * Frame-expansion mode
module Data.JsonLd.Expand
    ( expandDocument
    , expandElement
    , expandValue
    ) where

import           Control.Monad       (foldM)
import           Data.Aeson          (Value (..), object)
import qualified Data.Aeson.Key      as Key
import qualified Data.Aeson.KeyMap   as KM
import qualified Data.List           as List
import qualified Data.Map.Strict     as Map
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Vector         as V

import           Data.JsonLd.Context (ActiveContext (..), Container (..),
                                      CtxConfig (..), TermDefinition (..),
                                      TermSlot (..),
                                      defaultCtxConfig, emptyActiveContext,
                                      expandIriCtx, processContext)
import           Data.JsonLd.Error
import           Data.JsonLd.Keyword (isKeyword)
import           Data.JsonLd.Types   (Direction (..), Document (..),
                                      Options (..))

------------------------------------------------------------------------
-- Top-level §5.1.1

-- | Run expansion on a document. The result is always an array per
-- §5.1.1 step 8 (the closure that wraps the recursive expansion).
expandDocument :: Options -> Document -> Either JsonLdError Value
expandDocument opts (Document mIri body) = do
    -- Per §5.1.1 the active context's base IRI is the explicit
    -- 'optBase' option if present, otherwise the document's URL.
    let baseIri = case optBase opts of
            Just _  -> optBase opts
            Nothing -> mIri
        cfg  = defaultCtxConfig opts baseIri
        ctx0 = (emptyActiveContext (optProcessingMode opts))
                   { acBase         = baseIri
                   , acOriginalBase = baseIri
                   }

    -- Optional @expandContext from processor options.
    ctx1 <- case optExpandContext opts of
        Nothing -> Right ctx0
        Just c  -> processContext cfg ctx0 c

    expanded <- expandElement cfg ctx1 Nothing body
    Right (closeDocument expanded)

-- | §5.1.1 steps 8–9: a top-level @\@graph@-only object yields its
-- @\@graph@ value; @null@ yields an empty array; anything else gets
-- wrapped in a single-element array.
closeDocument :: Value -> Value
closeDocument = \case
    Null -> Array V.empty
    Object km
        | KM.size km == 1, Just g <- KM.lookup "@graph" km ->
            case g of
                Array _ -> g
                v       -> Array (V.singleton v)
        | otherwise -> Array (V.singleton (Object km))
    Array xs -> Array xs
    other    -> Array (V.singleton other)

------------------------------------------------------------------------
-- §5.1.2 Expansion

-- | Recursive expansion of a single element.
expandElement
    :: CtxConfig
    -> ActiveContext
    -> Maybe Text         -- ^ active property
    -> Value              -- ^ element to expand
    -> Either JsonLdError Value
expandElement cfg ctx mActiveProp element = case element of
    -- §5.1.2 step 1.
    Null -> Right Null

    -- §5.1.2 step 4: scalars.
    Bool   _ -> scalar element
    Number _ -> scalar element
    String _ -> scalar element

    -- §5.1.2 step 5: arrays.
    Array xs -> do
        items <- traverse (expandElement cfg ctx mActiveProp) (V.toList xs)
        let cleaned = concatMap unwrapArray items
            unwrapArray Null       = []
            unwrapArray (Array vs) = V.toList vs
            unwrapArray v          = [v]
        case mActiveProp of
            Just p
                | isListContainer ctx p, p /= "@list" ->
                    Right (object [("@list", Array (V.fromList cleaned))])
            _ -> Right (Array (V.fromList cleaned))

    -- §5.1.2 step 6 onward: objects.
    Object km -> expandObject cfg ctx mActiveProp km
  where
    -- A scalar at the top level (active property is null) or under
    -- @\@graph@ is dropped (free-floating literal).
    scalar v = case mActiveProp of
        Nothing       -> Right Null
        Just "@graph" -> Right Null
        Just p        -> expandValue cfg ctx p v

isListContainer :: ActiveContext -> Text -> Bool
isListContainer ctx p = case Map.lookup p (acTerms ctx) of
    Just td -> CList `elem` tdContainers td
    Nothing -> False

------------------------------------------------------------------------
-- §5.1.2 step 6+: object expansion

expandObject
    :: CtxConfig
    -> ActiveContext
    -> Maybe Text
    -> KM.KeyMap Value
    -> Either JsonLdError Value
expandObject cfg ctx0 mActiveProp km = do
    -- Step 6: process local @context.
    ctx1 <- case KM.lookup "@context" km of
        Just c  -> processContext cfg ctx0 c
        Nothing -> Right ctx0

    -- Step 8: type-scoped contexts. For each value of any @type-aliased
    -- entry (in alphabetical order) whose term def has a scoped
    -- @context, layer that context on top with propagate=False.
    ctx2 <- applyTypeScopedContexts cfg ctx1 km

    -- Step 9: iterate the remaining entries.
    let entries =
            [ (Key.toText k, v)
            | (k, v) <- KM.toList km
            , Key.toText k /= "@context"
            ]
    result <- foldM (foldEntry cfg ctx2 mActiveProp) KM.empty entries

    Right (finaliseObject mActiveProp result)

-- | §5.1.2 step 8: apply each type's scoped @context (if any) on top of
-- the current active context with propagation disabled.
applyTypeScopedContexts
    :: CtxConfig
    -> ActiveContext
    -> KM.KeyMap Value
    -> Either JsonLdError ActiveContext
applyTypeScopedContexts cfg ctx km = do
    typeNames <- collectTypeValues ctx km
    foldM applyOne ctx (List.sort typeNames)
  where
    applyOne acc t = case Map.lookup t (acTerms acc) >>= tdContext of
        Just c -> processContext
            (cfg { ccPropagate = False, ccOverrideProtected = True })
            acc c
        Nothing -> Right acc

-- | Collect every value of any entry whose key (after IRI expansion)
-- equals @\@type@. Each value must be a string or array of strings.
collectTypeValues
    :: ActiveContext
    -> KM.KeyMap Value
    -> Either JsonLdError [Text]
collectTypeValues ctx km =
    fmap concat (traverse extract (KM.toList km))
  where
    extract (k, v) = do
        expanded <- expandIriCtx ctx True False (Key.toText k)
        if expanded == "@type"
            then case v of
                String s -> Right [s]
                Array xs -> traverse asString (V.toList xs)
                _ -> Left $ JsonLdError InvalidTypeValue
                    "@type must be a string or array of strings"
            else Right []

    asString (String s) = Right s
    asString _ = Left $ JsonLdError InvalidTypeValue
        "@type array elements must be strings"

-- | Process one (key, value) pair from a context-update-applied object.
foldEntry
    :: CtxConfig
    -> ActiveContext
    -> Maybe Text
    -> KM.KeyMap Value
    -> (Text, Value)
    -> Either JsonLdError (KM.KeyMap Value)
foldEntry cfg ctx mActiveProp acc (key, value) = do
    expandedKey <- expandIriCtx ctx True False key
    if T.null expandedKey
        then Right acc
        else if isKeyword expandedKey
            then handleKeyword cfg ctx mActiveProp acc expandedKey value
            else if T.elem ':' expandedKey
                then handleProperty cfg ctx acc expandedKey key value
                else Right acc      -- unmapped term: drop

-- | Property entry. Applies any property-scoped @\@context@ on the
-- term, then dispatches to a container-aware expansion (currently
-- only @\@language@-map handling is done specially) or the normal
-- recursive expansion.
handleProperty
    :: CtxConfig
    -> ActiveContext
    -> KM.KeyMap Value
    -> Text                -- ^ expanded key (the IRI)
    -> Text                -- ^ original term name (used as active property)
    -> Value
    -> Either JsonLdError (KM.KeyMap Value)
handleProperty cfg ctx acc expandedKey origKey value = do
    let mTd        = Map.lookup origKey (acTerms ctx)
        containers = maybe [] tdContainers mTd

    -- §5.1.2 step 13.5: property-scoped @context.
    ctx' <- case mTd >>= tdContext of
        Just c -> processContext (cfg { ccOverrideProtected = True }) ctx c
        Nothing -> Right ctx

    case (containers, value) of
        (cs, Object km) | CLanguage `elem` cs ->
            expandLanguageMap acc expandedKey km
        (cs, Object km) | CIndex `elem` cs ->
            expandIndexMap cfg ctx' acc expandedKey origKey km
        (cs, Object km) | CId `elem` cs ->
            expandIdMap cfg ctx' acc expandedKey origKey km
        _ -> do
            expanded <- expandElement cfg ctx' (Just origKey) value
            let arr = case expanded of
                    Null     -> Array V.empty
                    Array xs -> Array xs
                    v        -> Array (V.singleton v)
            Right (KM.insertWith mergeArrays (Key.fromText expandedKey) arr acc)

-- | Expand a value used with @\@container: @\@language@ — a map keyed
-- by language tag whose values are strings (or arrays of strings).
-- The special key @\@none@ produces a value object with no language.
expandLanguageMap
    :: KM.KeyMap Value
    -> Text
    -> KM.KeyMap Value
    -> Either JsonLdError (KM.KeyMap Value)
expandLanguageMap acc expandedKey langMap = do
    pairs <- concat <$> traverse expandPair (KM.toList langMap)
    let arr = Array (V.fromList pairs)
    Right (KM.insertWith mergeArrays (Key.fromText expandedKey) arr acc)
  where
    expandPair (langKey, val) = do
        let langText = Key.toText langKey
            mLang | langText == "@none" = Nothing
                  | otherwise           = Just (T.toLower langText)
        case val of
            String s -> Right [valueObj s mLang]
            Array xs -> traverse (asString mLang) (V.toList xs)
            _ -> Left $ JsonLdError InvalidLanguageMapValue
                "@language map value must be a string or array of strings"

    asString mLang (String s) = Right (valueObj s mLang)
    asString _     _          = Left $ JsonLdError InvalidLanguageMapValue
        "@language map array elements must be strings"

    valueObj s (Just l) = object [("@value", String s), ("@language", String l)]
    valueObj s Nothing  = object [("@value", String s)]

-- | Expand a value used with @\@container: @\@index@ — a map keyed by
-- arbitrary index strings whose values are expanded as if they were
-- the term's value, with @\@index@ added to each result. The special
-- key @\@none@ leaves @\@index@ off.
expandIndexMap
    :: CtxConfig
    -> ActiveContext
    -> KM.KeyMap Value
    -> Text         -- ^ expanded key (the IRI)
    -> Text         -- ^ original term name
    -> KM.KeyMap Value
    -> Either JsonLdError (KM.KeyMap Value)
expandIndexMap cfg ctx acc expandedKey origKey idxMap = do
    pairs <- concat <$> traverse expandPair (KM.toList idxMap)
    let arr = Array (V.fromList pairs)
    Right (KM.insertWith mergeArrays (Key.fromText expandedKey) arr acc)
  where
    expandPair (idxKey, val) = do
        let idxText = Key.toText idxKey
            mIdx | idxText == "@none" = Nothing
                 | otherwise          = Just idxText
            valArr = case val of
                Array vs -> V.toList vs
                v        -> [v]
        items <- traverse (expandElement cfg ctx (Just origKey)) valArr
        let unwrapped = concatMap unwrap items
        Right (case mIdx of
            Just idx -> map (addIndex idx) unwrapped
            Nothing  -> unwrapped)

    unwrap (Array vs) = V.toList vs
    unwrap Null       = []
    unwrap v          = [v]

    addIndex idx (Object km)
        | not (KM.member "@index" km) = Object (KM.insert "@index" (String idx) km)
        | otherwise                   = Object km
    addIndex _ v = v

-- | Expand a value used with @\@container: @\@id@ — a map keyed by IRIs
-- (or terms) whose values are expanded as if they were the term's value.
-- @\@id@ is added to each map-value result. The special key @\@none@
-- omits the @\@id@.
expandIdMap
    :: CtxConfig
    -> ActiveContext
    -> KM.KeyMap Value
    -> Text         -- ^ expanded key (the IRI)
    -> Text         -- ^ original term name
    -> KM.KeyMap Value
    -> Either JsonLdError (KM.KeyMap Value)
expandIdMap cfg ctx acc expandedKey origKey idMap = do
    pairs <- concat <$> traverse expandPair (KM.toList idMap)
    let arr = Array (V.fromList pairs)
    Right (KM.insertWith mergeArrays (Key.fromText expandedKey) arr acc)
  where
    expandPair (idKey, val) = do
        let keyText = Key.toText idKey
        mIri <- if keyText == "@none"
            then Right Nothing
            else Just <$> expandIriCtx ctx False True keyText
        let valArr = case val of
                Array vs -> V.toList vs
                v        -> [v]
        items <- traverse (expandElement cfg ctx (Just origKey)) valArr
        let unwrapped = concatMap unwrap items
        Right (map (addId mIri) unwrapped)

    unwrap (Array vs) = V.toList vs
    unwrap Null       = []
    unwrap v          = [v]

    addId Nothing v             = v
    addId (Just iri) (Object km)
        | not (KM.member "@id" km) = Object (KM.insert "@id" (String iri) km)
        | otherwise                = Object km
    addId _ v                   = v

-- | Keyword entry: each keyword has a specific handling rule.
handleKeyword
    :: CtxConfig
    -> ActiveContext
    -> Maybe Text
    -> KM.KeyMap Value
    -> Text
    -> Value
    -> Either JsonLdError (KM.KeyMap Value)
handleKeyword cfg ctx mActiveProp acc kw value = case kw of
    "@id" -> case value of
        String s -> do
            iri <- expandIriCtx ctx False True s
            Right (KM.insert "@id" (String iri) acc)
        _ -> Left $ JsonLdError InvalidIdValue "@id must be a string"

    "@type" -> case value of
        String s -> do
            iri <- expandIriCtx ctx True True s
            Right (KM.insertWith mergeArrays "@type" (Array (V.singleton (String iri))) acc)
        Array xs -> do
            iris <- traverse expandTypeItem (V.toList xs)
            Right (KM.insertWith mergeArrays "@type" (Array (V.fromList iris)) acc)
        _ -> Left $ JsonLdError InvalidTypeValue "@type must be a string or array of strings"
      where
        expandTypeItem (String s) = String <$> expandIriCtx ctx True True s
        expandTypeItem _ = Left $ JsonLdError InvalidTypeValue
            "@type array elements must be strings"

    "@value" ->
        Right (KM.insert "@value" value acc)

    "@language" -> case value of
        String s -> Right (KM.insert "@language" (String (T.toLower s)) acc)
        _ -> Left $ JsonLdError InvalidLanguageTaggedString
            "@language value must be a string"

    "@index" -> case value of
        String _ -> Right (KM.insert "@index" value acc)
        _ -> Left $ JsonLdError InvalidIndexValue "@index must be a string"

    "@list" -> do
        expanded <- expandElement cfg ctx (Just "@list") value
        Right (KM.insert "@list" (asArrayValue expanded) acc)

    "@set" -> do
        expanded <- expandElement cfg ctx mActiveProp value
        Right (KM.insert "@set" (asArrayValue expanded) acc)

    "@graph" -> do
        expanded <- expandElement cfg ctx (Just "@graph") value
        Right (KM.insert "@graph" (asArrayValue expanded) acc)

    "@reverse" -> case value of
        Object km -> do
            -- §5.1.2 step 13.4.13: expand the @reverse value as a
            -- regular object (its entries become reverse properties),
            -- then merge into the parent under @reverse.
            expanded <- expandObject cfg ctx (Just "@reverse") km
            case expanded of
                Object expandedKm | not (KM.null expandedKm) ->
                    Right (KM.insertWith mergeReverse "@reverse"
                              (Object expandedKm) acc)
                _ -> Right acc
        _ -> Left $ JsonLdError InvalidReverseValue
            "@reverse value must be a map"

    _ -> Left $ JsonLdError NotImplemented
        ("expansion of keyword " <> kw <> " is not yet implemented")

-- | Merge two @\@reverse@ sub-objects by unioning their property maps,
-- concatenating the value arrays where keys collide.
mergeReverse :: Value -> Value -> Value
mergeReverse (Object new) (Object old) =
    Object (KM.unionWith mergeArrays new old)
mergeReverse new _ = new

asArrayValue :: Value -> Value
asArrayValue = \case
    Null     -> Array V.empty
    Array xs -> Array xs
    v        -> Array (V.singleton v)

-- | Concatenate two array values, preserving the existing-then-new
-- order. Used to merge multiple entries that map to the same expanded
-- IRI (which can happen when terms alias each other).
mergeArrays :: Value -> Value -> Value
mergeArrays new old = case (old, new) of
    (Array a, Array b) -> Array (a <> b)
    (Array a, b)       -> Array (V.snoc a b)
    (a, Array b)       -> Array (V.cons a b)
    (a, b)             -> Array (V.fromList [a, b])

------------------------------------------------------------------------
-- §5.1.2 step 19+ — object finalisation

-- | Final pass over a freshly-built expanded object. Implements the
-- subset of the §5.1.2 post-processing we need:
--
-- * @\@set@ unwrapping (step 21.2).
-- * Free-floating-node drop at the top level / under @\@graph@ (23).
finaliseObject :: Maybe Text -> KM.KeyMap Value -> Value
finaliseObject mActiveProp result
    -- Step 21.2: a @\@set@ object unwraps to its array.
    | KM.size result == 1
    , Just v <- KM.lookup "@set" result
        = v

    -- Step 23: free-floating drops.
    | freeFloating = Null

    | otherwise = Object result
  where
    freeFloating = case mActiveProp of
        Nothing       -> isFloating
        Just "@graph" -> isFloating
        _             -> False

    isFloating =
           KM.null result
        || (KM.size result == 1 && KM.member "@id" result)
        || KM.member "@value" result
        || (KM.size result == 1 && KM.member "@list" result)

------------------------------------------------------------------------
-- §5.3.1 Value Expansion

-- | Wrap a scalar value into a value- or node-object form. Applies
-- term-level type coercion (@\@id@, @\@vocab@, @\@json@, or an IRI),
-- and otherwise emits a value object whose @\@language@ and
-- @\@direction@ are sourced from the term's slots (or, if the term
-- doesn't override, the active context's defaults).
expandValue
    :: CtxConfig
    -> ActiveContext
    -> Text          -- ^ active property
    -> Value
    -> Either JsonLdError Value
expandValue _cfg ctx prop value =
    let mTd     = Map.lookup prop (acTerms ctx)
        typeMap = mTd >>= tdType
    in case typeMap of
        Just "@id"
            | String s <- value -> do
                iri <- expandIriCtx ctx False True s
                Right (object [("@id", String iri)])
        Just "@vocab"
            | String s <- value -> do
                iri <- expandIriCtx ctx True True s
                Right (object [("@id", String iri)])
        Just "@json" ->
            Right (object [("@value", value), ("@type", String "@json")])
        Just typeIri ->
            Right (object [("@value", value), ("@type", String typeIri)])
        Nothing -> case value of
            String _ ->
                let lang = resolveSlot (maybe SlotUnset tdLanguage mTd)
                                       (acLanguage ctx)
                    dir  = resolveSlot (maybe SlotUnset tdDirection mTd)
                                       (directionText <$> acDirection ctx)
                    base = KM.fromList [("@value", value)]
                    withLang = maybe base
                        (\l -> KM.insert "@language" (String l) base) lang
                    withDir  = maybe withLang
                        (\d -> KM.insert "@direction" (String d) withLang) dir
                in Right (Object withDir)
            _ -> Right (object [("@value", value)])

-- | Resolve a 'TermSlot' against a default. The semantics are: an
-- explicit @null@ slot ('SlotNull') overrides the default to nothing,
-- a value slot wins, and an unset slot falls through.
resolveSlot :: TermSlot -> Maybe Text -> Maybe Text
resolveSlot SlotUnset      def = def
resolveSlot SlotNull       _   = Nothing
resolveSlot (SlotValue v) _    = Just v

directionText :: Direction -> Text
directionText DirLtr = "ltr"
directionText DirRtl = "rtl"
