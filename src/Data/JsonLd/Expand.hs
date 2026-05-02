{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | The Expansion algorithm — JSON-LD 1.1 API §5.1.
--
-- Spec references in comments use the section numbering from
-- <https://www.w3.org/TR/json-ld11-api/#expansion-algorithms>. As with
-- the context-processing module, several spec features are deferred:
--
--  * Container expansion for @\@language@, @\@index@, @\@id@,
--    @\@type@, @\@graph@ maps
--  * @\@reverse@, @\@nest@, @\@included@
--  * Type-scoped and property-scoped contexts at expand time
--  * @\@direction@ on values
--  * Frame-expansion mode
--
-- These return 'NotImplemented' or simply pass values through unchanged.
module Data.JsonLd.Expand
    ( expandDocument
    , expandElement
    , expandValue
    ) where

import           Control.Monad       (foldM)
import           Data.Aeson          (Value (..), object)
import qualified Data.Aeson.Key      as Key
import qualified Data.Aeson.KeyMap   as KM
import qualified Data.Map.Strict     as Map
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Vector         as V

import           Data.JsonLd.Context (ActiveContext (..), Container (..),
                                      CtxConfig, TermDefinition (..),
                                      defaultCtxConfig, emptyActiveContext,
                                      expandIriCtx, processContext)
import           Data.JsonLd.Error
import           Data.JsonLd.Keyword (isKeyword)
import           Data.JsonLd.Types   (Document (..), Options (..))

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
    -- Step 6: process @context. Type-scoped and property-scoped contexts
    -- are not yet honoured — see Phase 2 part 3 / Phase 3 follow-up.
    ctx <- case KM.lookup "@context" km of
        Just c  -> processContext cfg ctx0 c
        Nothing -> Right ctx0

    -- Step 9: iterate the remaining entries.
    let entries =
            [ (Key.toText k, v)
            | (k, v) <- KM.toList km
            , Key.toText k /= "@context"
            ]
    result <- foldM (foldEntry cfg ctx mActiveProp) KM.empty entries

    Right (finaliseObject mActiveProp result)

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

-- | Property entry: expand the value with this property as active, wrap
-- in an array, and merge into the accumulator.
handleProperty
    :: CtxConfig
    -> ActiveContext
    -> KM.KeyMap Value
    -> Text                -- ^ expanded key (the IRI)
    -> Text                -- ^ original term name (used as active property)
    -> Value
    -> Either JsonLdError (KM.KeyMap Value)
handleProperty cfg ctx acc expandedKey origKey value = do
    expanded <- expandElement cfg ctx (Just origKey) value
    let arr = case expanded of
            Null     -> Array V.empty
            Array xs -> Array xs
            v        -> Array (V.singleton v)
    Right (KM.insertWith mergeArrays (Key.fromText expandedKey) arr acc)

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

    _ -> Left $ JsonLdError NotImplemented
        ("expansion of keyword " <> kw <> " is not yet implemented")

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

-- | Wrap a scalar value into a value- or node-object form, applying
-- the term's type coercion and falling back to the default language
-- when applicable. Direction handling and the full @\@language@-map
-- container case are deferred.
expandValue
    :: CtxConfig
    -> ActiveContext
    -> Text          -- ^ active property
    -> Value
    -> Either JsonLdError Value
expandValue _cfg ctx prop value =
    let mTd        = Map.lookup prop (acTerms ctx)
        typeMap    = mTd >>= tdType
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
                let withLang = case acLanguage ctx of
                        Just l  -> object [("@value", value), ("@language", String l)]
                        Nothing -> object [("@value", value)]
                in Right withLang
            _ -> Right (object [("@value", value)])
