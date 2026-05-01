{-# LANGUAGE OverloadedStrings #-}

-- | Internal AST shared by the expansion, compaction, and flattening
-- algorithms.
--
-- The shape mirrors §3 (\"Data Model\") and §4 of the JSON-LD 1.1
-- specification. After expansion, every value is one of three shapes:
--
--   * a /node object/  — has an @\@id@ and\/or properties keyed by IRIs
--   * a /value object/ — has @\@value@ plus optional type\/language\/direction
--   * a /list object/  — has @\@list@
--
-- Set objects exist only in source JSON-LD; expansion strips them. Graph
-- objects are modelled as node objects with a non-'Nothing' 'nodeGraph'.
module Data.JsonLd.Types
    ( -- * Documents
      Document (..)
      -- * Processing options
    , ProcessingMode (..)
    , Options (..)
    , defaultOptions
      -- * AST
    , Subject (..)
    , Direction (..)
    , LangTag (..)
    , mkLangTag
    , JsonLd (..)
    , NodeObject (..)
    , emptyNodeObject
    , ValueObject (..)
    , LiteralPayload (..)
    , ListObject (..)
      -- * Property maps
    , PropertyMap
    , insertProperty
    ) where

import           Data.Aeson      (Value)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text       (Text)
import qualified Data.Text       as T

import           Data.JsonLd.Iri (BlankNodeId, Iri)

------------------------------------------------------------------------
-- Documents and options

data Document = Document
    { documentIri  :: !(Maybe Iri)
    , documentBody :: !Value
    }
    deriving (Eq, Show)

data ProcessingMode = JsonLd10 | JsonLd11
    deriving (Eq, Ord, Show, Read, Bounded, Enum)

data Options = Options
    { optProcessingMode    :: !ProcessingMode
    , optBase              :: !(Maybe Iri)
    , optExpandContext     :: !(Maybe Value)
    , optProduceGenericRdf :: !Bool
    , optRdfDirection      :: !(Maybe Text)
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

------------------------------------------------------------------------
-- Identifiers and literal annotations

-- | The thing an @\@id@ refers to: an absolute IRI, a blank node, or
-- (rarely) nothing at all (used for unidentified node objects).
data Subject
    = SubjectIri   !Iri
    | SubjectBlank !BlankNodeId
    deriving (Eq, Ord, Show)

data Direction = DirLtr | DirRtl
    deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- | A BCP 47 language tag, canonicalised to ASCII lowercase.
newtype LangTag = LangTag { unLangTag :: Text }
    deriving (Eq, Ord, Show)

-- | Smart constructor that lower-cases the tag. Use this rather than the
-- 'LangTag' constructor directly; downstream comparisons assume tags are
-- in canonical form.
mkLangTag :: Text -> LangTag
mkLangTag = LangTag . T.toLower

------------------------------------------------------------------------
-- Property maps

-- | A node's properties: each IRI maps to an ordered list of values.
-- The order is significant in expanded form (it mirrors the source array
-- order) but the IRI keys themselves are kept sorted for determinism.
type PropertyMap = Map Iri [JsonLd]

-- | Append a value to the list for an IRI, preserving insertion order.
insertProperty :: Iri -> JsonLd -> PropertyMap -> PropertyMap
insertProperty k v = Map.insertWith (\new old -> old ++ new) k [v]

------------------------------------------------------------------------
-- Expanded-form AST

-- | The three shapes that survive expansion.
data JsonLd
    = JNode  !NodeObject
    | JValue !ValueObject
    | JList  !ListObject
    deriving (Eq, Show)

data NodeObject = NodeObject
    { nodeId       :: !(Maybe Subject)
      -- ^ @\@id@.
    , nodeTypes    :: ![Iri]
      -- ^ @\@type@ as a list (the spec allows multiple types).
    , nodeGraph    :: !(Maybe [JsonLd])
      -- ^ @\@graph@ contents. 'Nothing' for a regular node, 'Just' for a
      -- named or default-graph object.
    , nodeIncluded :: ![JsonLd]
      -- ^ @\@included@ (JSON-LD 1.1).
    , nodeIndex    :: !(Maybe Text)
      -- ^ @\@index@.
    , nodeReverse  :: !PropertyMap
      -- ^ Properties listed under @\@reverse@.
    , nodeProperties :: !PropertyMap
      -- ^ Ordinary IRI-keyed properties.
    }
    deriving (Eq, Show)

emptyNodeObject :: NodeObject
emptyNodeObject = NodeObject
    { nodeId         = Nothing
    , nodeTypes      = []
    , nodeGraph      = Nothing
    , nodeIncluded   = []
    , nodeIndex      = Nothing
    , nodeReverse    = Map.empty
    , nodeProperties = Map.empty
    }

data ValueObject = ValueObject
    { valuePayload   :: !LiteralPayload
    , valueType      :: !(Maybe Iri)
      -- ^ Datatype IRI, or @\@json@ for a JSON literal.
    , valueLanguage  :: !(Maybe LangTag)
    , valueDirection :: !(Maybe Direction)
    , valueIndex     :: !(Maybe Text)
    }
    deriving (Eq, Show)

-- | The shape an @\@value@ may take.
--
-- The spec allows strings, booleans, numbers (with a distinction between
-- integer and double for typing), @null@, and — when the type is
-- @\@json@ — an arbitrary JSON value preserved verbatim.
data LiteralPayload
    = LitString  !Text
    | LitBool    !Bool
    | LitInteger !Integer
    | LitDouble  !Double
    | LitNull
    | LitJson    !Value
    deriving (Eq, Show)

data ListObject = ListObject
    { listItems :: ![JsonLd]
    , listIndex :: !(Maybe Text)
    }
    deriving (Eq, Show)
