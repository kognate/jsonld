{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Context processing — JSON-LD 1.1 API §4.1.2 and §4.2.2.
--
-- Implements two intertwined algorithms:
--
-- * 'processContext' — the Context Processing algorithm: takes a
--   pre-existing active context and a local context (a JSON value), and
--   returns a new active context that has all of the local context's
--   declarations applied.
-- * 'createTermDefinition' — the Create Term Definition algorithm:
--   processes a single term entry inside a local context.
--
-- Step numbers in comments refer to the algorithm sections in the
-- 1.1-Rec API spec at <https://www.w3.org/TR/json-ld11-api/>.
--
-- Deferred to Phase 4 (document loader): remote @\@context@ references
-- (Strings) and @\@import@. Both currently return 'NotImplemented'.
module Data.JsonLd.Context
    ( -- * Active context
      ActiveContext (..)
    , emptyActiveContext
    , TermDefinition (..)
    , defaultTermDefinition
    , Container (..)
    , containerText
    , parseContainer
    , TermSlot (..)
      -- * Configuration
    , CtxConfig (..)
    , defaultCtxConfig
      -- * Algorithms
    , processContext
    , createTermDefinition
    , expandIriCtx
    ) where

import           Control.Monad       (foldM, when)
import           Data.Aeson          (Value (..))
import qualified Data.Aeson.Key      as Key
import qualified Data.Aeson.KeyMap   as KM
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as Map
import           Data.Maybe          (isJust)
import qualified Data.Set            as Set
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Vector         as V

import           Data.JsonLd.Error
import           Data.JsonLd.Iri     (Iri (..), isAbsoluteIri, isBlankNodeId,
                                      resolveRef)
import           Data.JsonLd.Keyword (isKeyword, isKeywordLike)
import           Data.JsonLd.Types   (Direction (..), Options (..),
                                      ProcessingMode (..))

------------------------------------------------------------------------
-- Container values for @container

data Container
    = CList
    | CSet
    | CIndex
    | CGraph
    | CId
    | CType
    | CLanguage
    deriving (Eq, Ord, Show, Read, Bounded, Enum)

containerText :: Container -> Text
containerText = \case
    CList     -> "@list"
    CSet      -> "@set"
    CIndex    -> "@index"
    CGraph    -> "@graph"
    CId       -> "@id"
    CType     -> "@type"
    CLanguage -> "@language"

parseContainer :: Text -> Maybe Container
parseContainer t =
    lookup t [(containerText c, c) | c <- [minBound .. maxBound]]

------------------------------------------------------------------------
-- Term definition

-- | A three-state slot for term-level overrides of language\/direction.
-- The spec distinguishes \"the term explicitly cleared this\" (writes a
-- @null@) from \"the term said nothing\" (the entry is absent), so we
-- can't collapse to a plain 'Maybe'.
data TermSlot
    = SlotUnset
    | SlotNull
    | SlotValue !Text
    deriving (Eq, Show)

data TermDefinition = TermDefinition
    { tdIri        :: !(Maybe Text)
      -- ^ The IRI mapping. May also be a keyword string (e.g. @\"\@type\"@)
      -- when the term aliases a keyword.
    , tdPrefix     :: !Bool
    , tdProtected  :: !Bool
    , tdReverse    :: !Bool
    , tdBaseUrl    :: !(Maybe Iri)
      -- ^ The base IRI of the context that defined this term — used by
      -- @\@import@ and scoped contexts.
    , tdContext    :: !(Maybe Value)
      -- ^ Scoped @\@context@, stored verbatim; re-processed at
      -- expand\/compact time.
    , tdContainers :: ![Container]
    , tdNest       :: !(Maybe Text)
    , tdLanguage   :: !TermSlot
    , tdDirection  :: !TermSlot
    , tdIndex      :: !(Maybe Text)
    , tdType       :: !(Maybe Text)
    }
    deriving (Eq, Show)

defaultTermDefinition :: TermDefinition
defaultTermDefinition = TermDefinition
    { tdIri        = Nothing
    , tdPrefix     = False
    , tdProtected  = False
    , tdReverse    = False
    , tdBaseUrl    = Nothing
    , tdContext    = Nothing
    , tdContainers = []
    , tdNest       = Nothing
    , tdLanguage   = SlotUnset
    , tdDirection  = SlotUnset
    , tdIndex      = Nothing
    , tdType       = Nothing
    }

------------------------------------------------------------------------
-- Active context

data ActiveContext = ActiveContext
    { acTerms          :: !(Map Text TermDefinition)
    , acBase           :: !(Maybe Iri)
    , acOriginalBase   :: !(Maybe Iri)
    , acVocab          :: !(Maybe Text)
    , acLanguage       :: !(Maybe Text)
    , acDirection      :: !(Maybe Direction)
    , acPrevious       :: !(Maybe ActiveContext)
    , acProcessingMode :: !ProcessingMode
    }
    deriving (Eq, Show)

emptyActiveContext :: ProcessingMode -> ActiveContext
emptyActiveContext pm = ActiveContext
    { acTerms          = Map.empty
    , acBase           = Nothing
    , acOriginalBase   = Nothing
    , acVocab          = Nothing
    , acLanguage       = Nothing
    , acDirection      = Nothing
    , acPrevious       = Nothing
    , acProcessingMode = pm
    }

------------------------------------------------------------------------
-- Configuration carried through processContext / createTermDefinition

data CtxConfig = CtxConfig
    { ccOptions           :: !Options
    , ccBaseUrl           :: !(Maybe Iri)
    , ccOverrideProtected :: !Bool
    , ccPropagate         :: !Bool
    , ccValidateScoped    :: !Bool
    , ccRemoteContexts    :: ![Iri]
    }
    deriving (Eq, Show)

defaultCtxConfig :: Options -> Maybe Iri -> CtxConfig
defaultCtxConfig opts mBase = CtxConfig
    { ccOptions           = opts
    , ccBaseUrl           = mBase
    , ccOverrideProtected = False
    , ccPropagate         = True
    , ccValidateScoped    = True
    , ccRemoteContexts    = []
    }

------------------------------------------------------------------------
-- §4.4.2 IRI Expansion (context-mode subset)

-- | Expand a string value to an IRI in the same way @\@id@ values are
-- expanded inside a term definition (§4.4.2). This is a smaller subset
-- of the full IRI expansion algorithm — it doesn't recurse into
-- 'createTermDefinition' (the spec's wider expansion does, for ordering
-- robustness). Good enough for the common case where context entries
-- are listed before their dependents.
expandIriCtx
    :: ActiveContext
    -> Bool          -- ^ vocab flag
    -> Bool          -- ^ document-relative flag
    -> Text          -- ^ value to expand
    -> Either JsonLdError Text
expandIriCtx ctx vocab docRel value
    | T.null value                            = Right value
    | isKeyword value || isKeywordLike value  = Right value
    | Just td <- Map.lookup value (acTerms ctx)
    , Just iri <- tdIri td
        = Right iri
    | T.elem ':' value = expandColon
    | vocab, Just v <- acVocab ctx          = Right (v <> value)
    | docRel, Just (Iri b) <- acBase ctx    = Right (resolveRef b value)
    | otherwise                               = Right value
  where
    -- Compact-IRI expansion. Looks up the prefix in the term map and
    -- substitutes its IRI mapping; otherwise treats the value as an
    -- absolute IRI. The full §4.4.2 algorithm consults the @\@prefix@
    -- flag and recursively defines the prefix on demand — both deferred.
    expandColon =
        let (prefix, rest0) = T.break (== ':') value
            suffix          = T.drop 1 rest0
        in if "//" `T.isPrefixOf` suffix || prefix == "_"
            then Right value
            else case Map.lookup prefix (acTerms ctx) of
                Just td | Just iri <- tdIri td -> Right (iri <> suffix)
                _ -> Right value

------------------------------------------------------------------------
-- §4.1.2 Context Processing Algorithm

-- | Run the Context Processing algorithm. The resulting active context
-- has @local@'s declarations layered on top of @active@.
processContext
    :: CtxConfig
    -> ActiveContext
    -> Value          -- ^ local context (object, string, null, or array of these)
    -> Either JsonLdError ActiveContext
processContext cfg0 active0 local0 = do
    -- Step 1: clone active context (we already have value semantics).
    let result0 = active0

    -- Step 2: if propagate=False and result has no previous_context,
    -- record the prior active context.
    let result1
            | not (ccPropagate cfg0)
            , Nothing <- acPrevious result0
                = result0 { acPrevious = Just active0 }
            | otherwise = result0

    -- Step 3: extract @propagate from a top-level local-context map.
    cfg1 <- propagateFromLocal cfg0 local0

    -- Step 4: ensure local context is a list.
    let locals = toContextArray local0

    -- Step 5: fold each context in.
    foldM (applyOne cfg1) result1 locals

-- | If local context is an object that contains @\@propagate@, fold
-- that into the configuration before the array dance below.
propagateFromLocal :: CtxConfig -> Value -> Either JsonLdError CtxConfig
propagateFromLocal cfg (Object o)
    | Just (Bool b) <- KM.lookup "@propagate" o = Right cfg { ccPropagate = b }
    | Just _        <- KM.lookup "@propagate" o
        = Left (JsonLdError InvalidPropagateValue "@propagate must be a boolean")
propagateFromLocal cfg _ = Right cfg

toContextArray :: Value -> [Value]
toContextArray (Array xs) = V.toList xs
toContextArray v          = [v]

applyOne :: CtxConfig -> ActiveContext -> Value -> Either JsonLdError ActiveContext
applyOne cfg active = \case
    -- Step 5.1: null context = reset.
    Null -> applyNull cfg active

    -- Step 5.2: a string is a remote context reference. Loader is not
    -- yet wired up; defer.
    String _ -> Left $ JsonLdError NotImplemented
        "remote @context loading is not implemented yet (Phase 4)"

    -- Step 5.3: anything else must be a context-definition map.
    Object km -> applyObject cfg active km

    -- Step 5.3 — anything else is invalid.
    other -> Left $ JsonLdError InvalidLocalContext
        ("local context must be a map, IRI, or null; got " <> T.pack (show other))

applyNull :: CtxConfig -> ActiveContext -> Either JsonLdError ActiveContext
applyNull cfg active = do
    -- Step 5.1.1: protected terms cannot be cleared without override.
    when (not (ccOverrideProtected cfg) && hasProtectedTerm active) $
        Left $ JsonLdError InvalidContextNullification
            "context nullification rejected: protected term definitions present"
    -- Step 5.1.2: re-initialise but keep the original base IRI.
    let pm   = acProcessingMode active
        prev = if ccPropagate cfg then Nothing else acPrevious active
    Right (emptyActiveContext pm)
        { acBase         = acOriginalBase active
        , acOriginalBase = acOriginalBase active
        , acPrevious     = prev
        }

hasProtectedTerm :: ActiveContext -> Bool
hasProtectedTerm = any tdProtected . Map.elems . acTerms

applyObject
    :: CtxConfig
    -> ActiveContext
    -> KM.KeyMap Value
    -> Either JsonLdError ActiveContext
applyObject cfg active0 ctxMap = do
    -- Step 5.5 @version
    active1 <- handleVersion (ccOptions cfg) active0 ctxMap

    -- Step 5.6 @import — defer to Phase 4
    when (KM.member "@import" ctxMap) $
        Left $ JsonLdError NotImplemented
            "@import is not implemented yet (Phase 4)"

    -- Step 5.7 @base (only when remote contexts is empty)
    active2 <- handleBase active1 ctxMap (null (ccRemoteContexts cfg))

    -- Step 5.8 @vocab
    active3 <- handleVocab active2 ctxMap

    -- Step 5.9 @language
    active4 <- handleLanguage active3 ctxMap

    -- Step 5.10 @direction (1.1)
    active5 <- handleDirection active4 ctxMap

    -- Step 5.11 @propagate is already taken care of in propagateFromLocal,
    -- but in 1.0 mode it's an error.
    case KM.lookup "@propagate" ctxMap of
        Just _ | acProcessingMode active5 == JsonLd10 ->
            Left $ JsonLdError InvalidContextEntry
                "@propagate is a 1.1 feature; processing mode is 1.0"
        _ -> Right ()

    -- Step 5.13: process each remaining key as a term definition.
    let protectedDefault = case KM.lookup "@protected" ctxMap of
            Just (Bool b) -> b
            _             -> False

    let termKeys =
            [ Key.toText k
            | k <- KM.keys ctxMap
            , Key.toText k `Set.notMember` reservedKeys
            ]

    let definedInit = Map.empty :: Map Text Bool
    (final, _) <- foldM
        (\(ac, defined) key ->
            createTermDefinition cfg ctxMap ac key defined protectedDefault)
        (active5, definedInit)
        termKeys
    Right final

reservedKeys :: Set.Set Text
reservedKeys = Set.fromList
    [ "@base", "@direction", "@import", "@language"
    , "@propagate", "@protected", "@version", "@vocab"
    ]

handleVersion
    :: Options
    -> ActiveContext
    -> KM.KeyMap Value
    -> Either JsonLdError ActiveContext
handleVersion opts active ctxMap = case KM.lookup "@version" ctxMap of
    Nothing -> Right active
    Just (Number n)
        | n /= 1.1
            -> Left $ JsonLdError InvalidVersionValue "@version must be 1.1"
        | optProcessingMode opts == JsonLd10
            -> Left $ JsonLdError ProcessingModeConflict
                "context declares @version 1.1 but processor is in 1.0 mode"
        | otherwise -> Right active { acProcessingMode = JsonLd11 }
    Just _ -> Left $ JsonLdError InvalidVersionValue "@version must be the number 1.1"

handleBase
    :: ActiveContext
    -> KM.KeyMap Value
    -> Bool          -- ^ apply (true at the top level only)
    -> Either JsonLdError ActiveContext
handleBase active ctxMap apply
    | not apply = Right active
    | otherwise = case KM.lookup "@base" ctxMap of
        Nothing                   -> Right active
        Just Null                 -> Right active { acBase = Nothing }
        Just (String s)
            | isAbsoluteIri s     -> Right active { acBase = Just (Iri s) }
            | Just (Iri b) <- acBase active
                                  -> Right active { acBase = Just (Iri (resolveRef b s)) }
            | otherwise           -> Left $ JsonLdError InvalidBaseIri
                ("relative @base " <> s <> " has no base to resolve against")
        Just _                    -> Left $ JsonLdError InvalidBaseIri
            "@base must be a string or null"

handleVocab :: ActiveContext -> KM.KeyMap Value -> Either JsonLdError ActiveContext
handleVocab active ctxMap = case KM.lookup "@vocab" ctxMap of
    Nothing                  -> Right active
    Just Null                -> Right active { acVocab = Nothing }
    Just (String s)
        | T.null s           -> Right active { acVocab = Just s }
        | isAbsoluteIri s    -> Right active { acVocab = Just s }
        | isBlankNodeId s    -> Right active { acVocab = Just s }
        | otherwise          -> Left $ JsonLdError InvalidVocabMapping
            ("@vocab must be an IRI or blank node identifier; got " <> s)
    Just _                   -> Left $ JsonLdError InvalidVocabMapping
        "@vocab must be a string or null"

handleLanguage :: ActiveContext -> KM.KeyMap Value -> Either JsonLdError ActiveContext
handleLanguage active ctxMap = case KM.lookup "@language" ctxMap of
    Nothing       -> Right active
    Just Null     -> Right active { acLanguage = Nothing }
    Just (String s) -> Right active { acLanguage = Just (T.toLower s) }
    Just _        -> Left $ JsonLdError InvalidDefaultLanguage
        "@language must be a string or null"

handleDirection :: ActiveContext -> KM.KeyMap Value -> Either JsonLdError ActiveContext
handleDirection active ctxMap = case KM.lookup "@direction" ctxMap of
    Nothing -> Right active
    Just _ | acProcessingMode active == JsonLd10 ->
        Left $ JsonLdError InvalidContextEntry
            "@direction is a 1.1 feature; processing mode is 1.0"
    Just Null              -> Right active { acDirection = Nothing }
    Just (String "ltr")    -> Right active { acDirection = Just DirLtr }
    Just (String "rtl")    -> Right active { acDirection = Just DirRtl }
    Just _                 -> Left $ JsonLdError InvalidBaseDirection
        "@direction must be \"ltr\", \"rtl\", or null"

------------------------------------------------------------------------
-- §4.2.2 Create Term Definition

-- | Add a single term definition (or remove one) from @active@.
--
-- Returns the new active context and the updated @defined@ map. The
-- @defined@ map carries cycle-detection state across recursive calls.
createTermDefinition
    :: CtxConfig
    -> KM.KeyMap Value     -- ^ the local context (passed by 'applyObject')
    -> ActiveContext       -- ^ the active context being built
    -> Text                -- ^ the term being defined
    -> Map Text Bool       -- ^ defined map: True=fully defined, False=in progress
    -> Bool                -- ^ context-level @protected default
    -> Either JsonLdError (ActiveContext, Map Text Bool)
createTermDefinition cfg ctxMap active term defined protectedDefault = do
    -- Step 1: cycle / already-done check.
    case Map.lookup term defined of
        Just True  -> Right (active, defined)
        Just False -> Left $ JsonLdError CyclicIriMapping
            ("cyclic term definition involving " <> term)
        Nothing -> goDefine

  where
    goDefine = do
        -- Step 3: empty term is invalid.
        when (T.null term) $
            Left $ JsonLdError InvalidTermDefinition "empty term"

        -- Steps 5-7: keyword and keyword-like checks.
        if isKeyword term
            then Left $ JsonLdError KeywordRedefinition
                    ("cannot redefine keyword " <> term)
            else if isKeywordLike term
                -- §4.2.2 step 7: terms with the form "@x" but not actual
                -- keywords are reserved and silently ignored.
                then Right (active, Map.insert term True defined)
                else dispatchValue

    dispatchValue = do
        let defined1 = Map.insert term False defined
            value    = KM.lookup (Key.fromText term) ctxMap
        case value of
            Just Null -> finalise (active { acTerms = Map.delete term (acTerms active) }) defined1
            Just (String s) -> defineWithIri active defined1 s
            Just (Object def) -> defineFromObject active defined1 def
            Just _other -> Left $ JsonLdError InvalidTermDefinition
                ("term definition for " <> term <> " must be string, map, or null")
            Nothing -> Left $ JsonLdError InvalidTermDefinition
                ("term " <> term <> " has no entry in the local context")

    finalise ac d = Right (ac, Map.insert term True d)

    -- Insert a freshly-built term definition, performing the §4.2.2
    -- step 31 protected-redefinition check.
    commit :: ActiveContext -> Map Text Bool -> TermDefinition
           -> Either JsonLdError (ActiveContext, Map Text Bool)
    commit ac defined1 newTd = do
        resolved <-
            if ccOverrideProtected cfg
                then Right newTd
                else case Map.lookup term (acTerms ac) of
                    Just prev
                        | tdProtected prev
                        , unprotect prev /= unprotect newTd
                            -> Left $ JsonLdError ProtectedTermRedefinition
                                ("attempt to redefine protected term " <> term)
                        | tdProtected prev
                            -> Right prev
                    _ -> Right newTd
        finalise (ac { acTerms = Map.insert term resolved (acTerms ac) }) defined1

    -- A term whose value is just a string is shorthand for {"@id": s}.
    defineWithIri :: ActiveContext -> Map Text Bool -> Text
                  -> Either JsonLdError (ActiveContext, Map Text Bool)
    defineWithIri ac defined1 s = do
        iri <- expandIriCtx ac True False s
        commit ac defined1 defaultTermDefinition
            { tdIri       = Just iri
            , tdProtected = protectedDefault
            , tdBaseUrl   = ccBaseUrl cfg
            }

    -- {"@id": ..., "@type": ..., "@container": ..., ...}
    defineFromObject :: ActiveContext -> Map Text Bool -> KM.KeyMap Value
                     -> Either JsonLdError (ActiveContext, Map Text Bool)
    defineFromObject ac defined1 def = do
        let getKey k = KM.lookup k def
            keys = Set.fromList (map Key.toText (KM.keys def))

        -- Reject unknown keys.
        let allowed = Set.fromList
                [ "@id", "@reverse", "@type", "@container", "@language"
                , "@direction", "@index", "@prefix", "@protected"
                , "@nest", "@context"
                ]
        case Set.toList (keys `Set.difference` allowed) of
            (extra : _) -> Left $ JsonLdError InvalidTermDefinition
                ("unknown key " <> extra <> " in term definition for " <> term)
            [] -> Right ()

        -- @protected (Step 14)
        protectedFlag <- case getKey "@protected" of
            Nothing             -> Right protectedDefault
            Just (Bool b)       -> Right b
            Just _              -> Left $ JsonLdError InvalidProtectedValue
                ("@protected on " <> term <> " must be a boolean")

        -- @reverse vs @id mutually-exclusive setup.
        let reverseEntry = getKey "@reverse"
            idEntry      = getKey "@id"

        when (isJust reverseEntry && isJust idEntry) $
            Left $ JsonLdError InvalidReverseProperty
                ("term " <> term <> " has both @id and @reverse")

        -- Resolve IRI mapping.
        (iri, isReverse) <- case (reverseEntry, idEntry) of
            (Just (String r), _) -> do
                expanded <- expandIriCtx ac True False r
                pure (Just expanded, True)
            (Just _, _) -> Left $ JsonLdError InvalidIriMapping
                ("@reverse on " <> term <> " must be a string")
            (Nothing, Just Null) -> pure (Nothing, False)
            (Nothing, Just (String i)) -> do
                expanded <- expandIriCtx ac True False i
                pure (Just expanded, False)
            (Nothing, Just _) -> Left $ JsonLdError InvalidIriMapping
                ("@id on " <> term <> " must be a string or null")
            (Nothing, Nothing)
                -- No @id: term is itself a compact IRI or uses @vocab.
                | T.elem ':' term -> do
                    expanded <- expandIriCtx ac True False term
                    pure (Just expanded, False)
                | Just _ <- acVocab ac -> do
                    expanded <- expandIriCtx ac True False term
                    pure (Just expanded, False)
                | otherwise -> Left $ JsonLdError InvalidIriMapping
                    ("term " <> term <> " has no @id and no @vocab to resolve against")

        -- @type
        typeIri <- case getKey "@type" of
            Nothing                 -> Right Nothing
            Just (String "@id")     -> Right (Just "@id")
            Just (String "@vocab")  -> Right (Just "@vocab")
            Just (String "@json")   -> Right (Just "@json")
            Just (String "@none")   -> Right (Just "@none")
            Just (String t)         -> Just <$> expandIriCtx ac True False t
            Just _                  -> Left $ JsonLdError InvalidTypeMapping
                ("@type on " <> term <> " must be a string")

        -- @container
        containers <- case getKey "@container" of
            Nothing -> Right []
            Just v  -> parseContainerValue term v

        -- @prefix
        prefixFlag <- case getKey "@prefix" of
            Nothing       -> Right False
            Just (Bool b) -> Right b
            Just _        -> Left $ JsonLdError InvalidPrefixValue
                ("@prefix on " <> term <> " must be a boolean")

        -- @nest
        nest <- case getKey "@nest" of
            Nothing               -> Right Nothing
            Just (String "@nest") -> Right (Just "@nest")
            Just (String s)
                | not (isKeyword s) -> Right (Just s)
            Just _ -> Left $ JsonLdError InvalidNestValue
                ("@nest on " <> term <> " must be a non-keyword string or \"@nest\"")

        -- @index
        indexVal <- case getKey "@index" of
            Nothing         -> Right Nothing
            Just (String s) -> Right (Just s)
            Just _          -> Left $ JsonLdError InvalidTermDefinition
                ("@index on " <> term <> " must be a string")

        -- @language (term-level)
        langSlot <- case getKey "@language" of
            Nothing         -> Right SlotUnset
            Just Null       -> Right SlotNull
            Just (String s) -> Right (SlotValue (T.toLower s))
            Just _ -> Left $ JsonLdError InvalidLanguageMapping
                ("@language on " <> term <> " must be a string or null")

        -- @direction (term-level)
        dirSlot <- case getKey "@direction" of
            Nothing            -> Right SlotUnset
            Just Null          -> Right SlotNull
            Just (String "ltr") -> Right (SlotValue "ltr")
            Just (String "rtl") -> Right (SlotValue "rtl")
            Just _ -> Left $ JsonLdError InvalidBaseDirection
                ("@direction on " <> term <> " must be \"ltr\", \"rtl\", or null")

        -- @context (scoped) — stored verbatim; recursive validation deferred.
        let scopedCtx = getKey "@context"

        commit ac defined1 defaultTermDefinition
            { tdIri        = iri
            , tdReverse    = isReverse
            , tdProtected  = protectedFlag
            , tdType       = typeIri
            , tdContainers = containers
            , tdPrefix     = prefixFlag
            , tdNest       = nest
            , tdIndex      = indexVal
            , tdLanguage   = langSlot
            , tdDirection  = dirSlot
            , tdContext    = scopedCtx
            , tdBaseUrl    = ccBaseUrl cfg
            }

-- | A term definition with the @\@protected@ flag stripped. Used by
-- the protected-redefinition check, which considers two definitions
-- equivalent if they differ only in protection.
unprotect :: TermDefinition -> TermDefinition
unprotect t = t { tdProtected = False }

parseContainerValue :: Text -> Value -> Either JsonLdError [Container]
parseContainerValue term = \case
    String s -> single s
    Array xs -> traverse one (V.toList xs)
    _        -> bad
  where
    bad = Left $ JsonLdError InvalidContainerMapping
        ("@container on " <> term <> " must be a string or array of strings")
    single s = case parseContainer s of
        Just c  -> Right [c]
        Nothing -> Left $ JsonLdError InvalidContainerMapping
            ("@container on " <> term <> " has unknown value " <> s)
    one (String s) = case parseContainer s of
        Just c  -> Right c
        Nothing -> Left $ JsonLdError InvalidContainerMapping
            ("@container on " <> term <> " has unknown value " <> s)
    one _ = Left $ JsonLdError InvalidContainerMapping
        ("@container on " <> term <> " array must contain only strings")
