-- | Error codes defined by the JSON-LD 1.1 API and Framing specifications.
--
-- Each constructor corresponds to one of the strings the spec uses to
-- identify an error. The textual form (returned by 'errorCodeText') is what
-- the W3C test suite compares against in negative-evaluation tests.
module Data.JsonLd.Error
    ( JsonLdError (..)
    , JsonLdErrorCode (..)
    , errorCodeText
    , parseErrorCode
    ) where

import           Control.Exception (Exception)
import           Data.Text         (Text)

data JsonLdError = JsonLdError
    { errorCode    :: !JsonLdErrorCode
    , errorMessage :: !Text
    }
    deriving (Eq, Show)

instance Exception JsonLdError

-- | The fixed set of JSON-LD 1.1 error codes (API + Framing).
data JsonLdErrorCode
    = CollidingKeywords
    | ConflictingIndexes
    | ContextOverflow
    | CyclicIriMapping
    | InvalidIdValue
    | InvalidImportValue
    | InvalidIncludedValue
    | InvalidIndexValue
    | InvalidNestValue
    | InvalidPrefixValue
    | InvalidPropagateValue
    | InvalidProtectedValue
    | InvalidReverseValue
    | InvalidVersionValue
    | InvalidBaseDirection
    | InvalidBaseIri
    | InvalidContainerMapping
    | InvalidContextEntry
    | InvalidContextNullification
    | InvalidDefaultLanguage
    | InvalidIriMapping
    | InvalidJsonLiteral
    | InvalidKeywordAlias
    | InvalidLanguageMapValue
    | InvalidLanguageMapping
    | InvalidLanguageTaggedString
    | InvalidLanguageTaggedValue
    | InvalidLocalContext
    | InvalidRemoteContext
    | InvalidReverseProperty
    | InvalidReversePropertyMap
    | InvalidReversePropertyValue
    | InvalidScopedContext
    | InvalidScriptElement
    | InvalidSetOrListObject
    | InvalidTermDefinition
    | InvalidTypeMapping
    | InvalidTypeValue
    | InvalidTypedValue
    | InvalidValueObject
    | InvalidValueObjectValue
    | InvalidVocabMapping
    | IriConfusedWithPrefix
    | KeywordRedefinition
    | LoadingDocumentFailed
    | LoadingRemoteContextFailed
    | MultipleContextLinkHeaders
    | ProcessingModeConflict
    | ProtectedTermRedefinition
      -- Framing
    | InvalidFrame
    | InvalidEmbedValue
      -- Internal: not in the spec, used for stub algorithms
    | NotImplemented
    deriving (Eq, Ord, Show, Read, Bounded, Enum)

errorCodeText :: JsonLdErrorCode -> Text
errorCodeText c = case c of
    CollidingKeywords             -> "colliding keywords"
    ConflictingIndexes            -> "conflicting indexes"
    ContextOverflow               -> "context overflow"
    CyclicIriMapping              -> "cyclic IRI mapping"
    InvalidIdValue                -> "invalid @id value"
    InvalidImportValue            -> "invalid @import value"
    InvalidIncludedValue          -> "invalid @included value"
    InvalidIndexValue             -> "invalid @index value"
    InvalidNestValue              -> "invalid @nest value"
    InvalidPrefixValue            -> "invalid @prefix value"
    InvalidPropagateValue         -> "invalid @propagate value"
    InvalidProtectedValue         -> "invalid @protected value"
    InvalidReverseValue           -> "invalid @reverse value"
    InvalidVersionValue           -> "invalid @version value"
    InvalidBaseDirection          -> "invalid base direction"
    InvalidBaseIri                -> "invalid base IRI"
    InvalidContainerMapping       -> "invalid container mapping"
    InvalidContextEntry           -> "invalid context entry"
    InvalidContextNullification   -> "invalid context nullification"
    InvalidDefaultLanguage        -> "invalid default language"
    InvalidIriMapping             -> "invalid IRI mapping"
    InvalidJsonLiteral            -> "invalid JSON literal"
    InvalidKeywordAlias           -> "invalid keyword alias"
    InvalidLanguageMapValue       -> "invalid language map value"
    InvalidLanguageMapping        -> "invalid language mapping"
    InvalidLanguageTaggedString   -> "invalid language-tagged string"
    InvalidLanguageTaggedValue    -> "invalid language-tagged value"
    InvalidLocalContext           -> "invalid local context"
    InvalidRemoteContext          -> "invalid remote context"
    InvalidReverseProperty        -> "invalid reverse property"
    InvalidReversePropertyMap     -> "invalid reverse property map"
    InvalidReversePropertyValue   -> "invalid reverse property value"
    InvalidScopedContext          -> "invalid scoped context"
    InvalidScriptElement          -> "invalid script element"
    InvalidSetOrListObject        -> "invalid set or list object"
    InvalidTermDefinition         -> "invalid term definition"
    InvalidTypeMapping            -> "invalid type mapping"
    InvalidTypeValue              -> "invalid type value"
    InvalidTypedValue             -> "invalid typed value"
    InvalidValueObject            -> "invalid value object"
    InvalidValueObjectValue       -> "invalid value object value"
    InvalidVocabMapping           -> "invalid vocab mapping"
    IriConfusedWithPrefix         -> "IRI confused with prefix"
    KeywordRedefinition           -> "keyword redefinition"
    LoadingDocumentFailed         -> "loading document failed"
    LoadingRemoteContextFailed    -> "loading remote context failed"
    MultipleContextLinkHeaders    -> "multiple context link headers"
    ProcessingModeConflict        -> "processing mode conflict"
    ProtectedTermRedefinition     -> "protected term redefinition"
    InvalidFrame                  -> "invalid frame"
    InvalidEmbedValue             -> "invalid embed value"
    NotImplemented                -> "not implemented"

parseErrorCode :: Text -> Maybe JsonLdErrorCode
parseErrorCode t =
    lookup t [(errorCodeText c, c) | c <- [minBound .. maxBound]]
