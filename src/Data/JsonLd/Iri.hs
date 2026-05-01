-- | IRI primitives.
--
-- The full set of IRI handling rules required by JSON-LD (relative
-- resolution per RFC 3986, base resolution, distinguishing keywords from
-- blank nodes from compact IRIs) lives here. Only the type wrappers and
-- the predicates the rest of the library needs to typecheck against are
-- defined for now; the resolution algorithms will be filled in alongside
-- expansion.
module Data.JsonLd.Iri
    ( Iri (..)
    , BlankNodeId (..)
    , isAbsoluteIri
    , isBlankNodeId
    ) where

import           Data.Text (Text)
import qualified Data.Text as T

newtype Iri = Iri { unIri :: Text }
    deriving (Eq, Ord, Show)

newtype BlankNodeId = BlankNodeId { unBlankNodeId :: Text }
    deriving (Eq, Ord, Show)

-- | True if @t@ contains an ASCII colon preceded by at least one
-- scheme-legal character. This is the cheap check the spec uses in many
-- places to distinguish absolute IRIs from terms.
isAbsoluteIri :: Text -> Bool
isAbsoluteIri t = case T.break (== ':') t of
    (scheme, rest)
        | T.null scheme -> False
        | T.null rest   -> False
        | otherwise     -> T.all schemeChar scheme
  where
    schemeChar c =
        (c >= 'a' && c <= 'z')
            || (c >= 'A' && c <= 'Z')
            || (c >= '0' && c <= '9')
            || c == '+'
            || c == '-'
            || c == '.'

isBlankNodeId :: Text -> Bool
isBlankNodeId = T.isPrefixOf "_:"
