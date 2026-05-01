-- | JSON-LD 1.1 reserved keywords.
--
-- See <https://www.w3.org/TR/json-ld11/#keywords>.
module Data.JsonLd.Keyword
    ( Keyword (..)
    , keywordText
    , parseKeyword
    , allKeywords
    , isKeyword
    , isKeywordLike
    ) where

import           Data.Text (Text)
import qualified Data.Text as T

-- | Every keyword defined by JSON-LD 1.1.
data Keyword
    = KBase
    | KContainer
    | KContext
    | KDirection
    | KGraph
    | KId
    | KImport
    | KIncluded
    | KIndex
    | KJson
    | KLanguage
    | KList
    | KNest
    | KNone
    | KPrefix
    | KPropagate
    | KProtected
    | KReverse
    | KSet
    | KType
    | KValue
    | KVersion
    | KVocab
    deriving (Eq, Ord, Show, Read, Bounded, Enum)

keywordText :: Keyword -> Text
keywordText k = case k of
    KBase      -> "@base"
    KContainer -> "@container"
    KContext   -> "@context"
    KDirection -> "@direction"
    KGraph     -> "@graph"
    KId        -> "@id"
    KImport    -> "@import"
    KIncluded  -> "@included"
    KIndex     -> "@index"
    KJson      -> "@json"
    KLanguage  -> "@language"
    KList      -> "@list"
    KNest      -> "@nest"
    KNone      -> "@none"
    KPrefix    -> "@prefix"
    KPropagate -> "@propagate"
    KProtected -> "@protected"
    KReverse   -> "@reverse"
    KSet       -> "@set"
    KType      -> "@type"
    KValue     -> "@value"
    KVersion   -> "@version"
    KVocab     -> "@vocab"

parseKeyword :: Text -> Maybe Keyword
parseKeyword t = lookup t [(keywordText k, k) | k <- allKeywords]

allKeywords :: [Keyword]
allKeywords = [minBound .. maxBound]

isKeyword :: Text -> Bool
isKeyword t = case parseKeyword t of
    Just _  -> True
    Nothing -> False

-- | A string that starts with @\@@ and is followed by one or more ASCII
-- letters. Such strings are treated as keywords by the spec even when not
-- defined; they generate processor errors rather than being interpreted as
-- IRIs.
isKeywordLike :: Text -> Bool
isKeywordLike t = case T.uncons t of
    Just ('@', rest) | not (T.null rest) -> T.all isAlpha rest
    _                                    -> False
  where
    isAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
