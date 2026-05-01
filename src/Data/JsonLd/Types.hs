-- | Internal AST shared by the expansion, compaction, and flattening
-- algorithms. The shape mirrors §3 (\"Data Model\") of the JSON-LD 1.1
-- specification.
--
-- This is the seam between 'Data.Aeson.Value' and the algorithms: parsers
-- convert from 'Aeson.Value' into these types so the algorithms can
-- pattern-match on the categories the spec is written in terms of (node
-- objects, value objects, list/set/graph objects).
module Data.JsonLd.Types
    ( -- * Documents
      Document (..)
      -- * Processing options
    , ProcessingMode (..)
    , Options (..)
    , defaultOptions
    ) where

import           Data.Aeson      (Value)
import           Data.Text       (Text)

import           Data.JsonLd.Iri (Iri)

-- | A JSON-LD document, paired with the IRI it was loaded from. The IRI
-- is needed by the expansion algorithm whenever the document or one of
-- its contexts uses a relative IRI.
data Document = Document
    { documentIri  :: !(Maybe Iri)
    , documentBody :: !Value
    }
    deriving (Eq, Show)

-- | The @processingMode@ option from the API spec. JSON-LD 1.1 introduced
-- behaviors that conflict with 1.0 documents, so processors must choose
-- which mode to operate in.
data ProcessingMode
    = JsonLd10
    | JsonLd11
    deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Options = Options
    { optProcessingMode :: !ProcessingMode
    , optBase           :: !(Maybe Iri)
    , optExpandContext  :: !(Maybe Value)
    , optProduceGenericRdf :: !Bool
    , optRdfDirection   :: !(Maybe Text)
    }
    deriving (Eq, Show)

defaultOptions :: Options
defaultOptions = Options
    { optProcessingMode    = JsonLd11
    , optBase              = Nothing
    , optExpandContext     = Nothing
    , optProduceGenericRdf = True
    , optRdfDirection      = Nothing
    }
