-- | Top-level entry points to the JSON-LD 1.1 processor.
--
-- These are stubs at the moment: every algorithm returns 'NotImplemented'
-- so the test harness can still wire them up and report unimplemented
-- categories as such. Each algorithm will be filled in over the coming
-- phases (see the project plan).
module Data.JsonLd
    ( -- * Re-exports
      module Data.JsonLd.Error
    , module Data.JsonLd.Types
      -- * Algorithms
    , expand
    , compact
    , flatten
    , fromRdf
    , toRdf
    , frame
    ) where

import           Data.Aeson           (Value)

import           Data.JsonLd.Error
import           Data.JsonLd.Types

notImplemented :: Either JsonLdError a
notImplemented = Left (JsonLdError NotImplemented "algorithm not yet implemented")

expand :: Options -> Document -> Either JsonLdError Value
expand _ _ = notImplemented

compact :: Options -> Value -> Document -> Either JsonLdError Value
compact _ _ _ = notImplemented

flatten :: Options -> Maybe Value -> Document -> Either JsonLdError Value
flatten _ _ _ = notImplemented

-- | The RDF type is unspecified for now; will be replaced by the internal
-- dataset type in Phase 7.
fromRdf :: Options -> rdfDataset -> Either JsonLdError Value
fromRdf _ _ = notImplemented

toRdf :: Options -> Document -> Either JsonLdError rdfDataset
toRdf _ _ = notImplemented

frame :: Options -> Value -> Document -> Either JsonLdError Value
frame _ _ _ = notImplemented
