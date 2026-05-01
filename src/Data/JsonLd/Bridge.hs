{-# LANGUAGE OverloadedStrings #-}

-- | Bridge between 'Data.Aeson.Value' and the internal JSON-LD AST.
--
-- The classifiers in this module inspect the literal @\@-key@ structure
-- of an Aeson value. They are intended for use /after/ expansion (or
-- during expansion, when the active context has already been used to
-- rewrite term aliases back to their canonical keyword form). Before
-- expansion, terms in the active context may alias keywords (so a member
-- key @\"@val\"@ in the source might mean @\@value@), and these
-- predicates will not recognise the alias.
module Data.JsonLd.Bridge
    ( ObjectShape (..)
    , classifyShape
    , isValueObject
    , isListObject
    , isSetObject
    , isGraphObject
    , isNodeObjectShape
    ) where

import           Data.Aeson         (Value (..))
import qualified Data.Aeson.Key     as Key
import qualified Data.Aeson.KeyMap  as KM

-- | Discriminator over the syntactic shape of a JSON object (in
-- expanded JSON-LD form).
data ObjectShape
    = ShapeValue
    | ShapeList
    | ShapeSet
    | ShapeGraph
    | ShapeNode
    | ShapeNotObject
      -- ^ The 'Value' wasn't an object at all.
    deriving (Eq, Show)

classifyShape :: Value -> ObjectShape
classifyShape v
    | not (isObject v)           = ShapeNotObject
    | hasMember "@value" v       = ShapeValue
    | hasMember "@list"  v       = ShapeList
    | hasMember "@set"   v       = ShapeSet
    | hasMember "@graph" v       = ShapeGraph
    | otherwise                  = ShapeNode

isValueObject, isListObject, isSetObject, isGraphObject :: Value -> Bool
isValueObject = (== ShapeValue) . classifyShape
isListObject  = (== ShapeList)  . classifyShape
isSetObject   = (== ShapeSet)   . classifyShape
isGraphObject = (== ShapeGraph) . classifyShape

-- | True for objects that don't match any of the special @\@-key@
-- shapes. Note this is a /shape/ test only — a 'ShapeNode' may still
-- turn out to be an @\@context@-only object (which expansion drops) or
-- otherwise non-conforming.
isNodeObjectShape :: Value -> Bool
isNodeObjectShape = (== ShapeNode) . classifyShape

------------------------------------------------------------------------
-- Helpers

isObject :: Value -> Bool
isObject (Object _) = True
isObject _          = False

hasMember :: Key.Key -> Value -> Bool
hasMember k (Object km) = KM.member k km
hasMember _ _           = False
