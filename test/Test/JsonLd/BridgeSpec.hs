{-# LANGUAGE OverloadedStrings #-}

module Test.JsonLd.BridgeSpec
    ( tests
    ) where

import           Data.Aeson         (Value, object, (.=))
import qualified Data.Aeson         as A
import           Test.Tasty         (TestTree, testGroup)
import           Test.Tasty.HUnit   (testCase, (@?=))

import           Data.JsonLd.Bridge

tests :: TestTree
tests = testGroup "Data.JsonLd.Bridge"
    [ classify "value object" valueObj  ShapeValue
    , classify "list object"  listObj   ShapeList
    , classify "set object"   setObj    ShapeSet
    , classify "graph object" graphObj  ShapeGraph
    , classify "node object"  nodeObj   ShapeNode
    , classify "string"       (A.String "x")    ShapeNotObject
    , classify "array"        (A.Array  mempty) ShapeNotObject
    , classify "null"         A.Null            ShapeNotObject

    , testCase "isValueObject" $ isValueObject valueObj @?= True
    , testCase "isListObject"  $ isListObject  listObj  @?= True
    , testCase "isNodeObjectShape on plain node"   $
        isNodeObjectShape nodeObj  @?= True
    , testCase "isNodeObjectShape on value object" $
        isNodeObjectShape valueObj @?= False
    ]

classify :: String -> Value -> ObjectShape -> TestTree
classify label v expected =
    testCase ("classifyShape: " <> label) $ classifyShape v @?= expected

valueObj, listObj, setObj, graphObj, nodeObj :: Value
valueObj = object [ "@value" .= ("hello" :: String), "@language" .= ("en" :: String) ]
listObj  = object [ "@list"  .= ([1, 2, 3] :: [Int]) ]
setObj   = object [ "@set"   .= ([1, 2, 3] :: [Int]) ]
graphObj = object [ "@graph" .= ([object []] :: [Value]), "@id" .= ("g" :: String) ]
nodeObj  = object [ "@id" .= ("n" :: String), "http://x/p" .= ([] :: [Value]) ]
