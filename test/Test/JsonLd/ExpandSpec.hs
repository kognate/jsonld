{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the Expansion algorithm. Independent of the W3C
-- suite — we feed a small input map in, run 'expand', and assert on
-- the resulting JSON.
module Test.JsonLd.ExpandSpec
    ( tests
    ) where

import           Data.Aeson         (Value (..), object, (.=))
import qualified Data.Aeson         as A
import qualified Data.Vector        as V
import           Test.Tasty         (TestTree, testGroup)
import           Test.Tasty.HUnit   (Assertion, assertFailure, testCase, (@?=))

import           Data.JsonLd
import           Data.JsonLd.Expand (expandDocument)

tests :: TestTree
tests = testGroup "Data.JsonLd.Expand"
    [ testCase "null expands to []" $
        run Null @?= Right (Array V.empty)

    , testCase "free-floating scalar drops to []" $
        run (A.String "hello") @?= Right (Array V.empty)

    , testCase "free-floating @id-only object drops" $ do
        let doc = object
                [ "@context" .= object
                    [ "name" .= ("http://xmlns.com/foaf/0.1/name" :: String) ]
                , "@id" .= ("http://example.org/test#example" :: String)
                ]
        run doc @?= Right (Array V.empty)

    , testCase "term + value object basic expansion" $ do
        let doc = object
                [ "@context" .= object
                    [ "name" .= ("http://xmlns.com/foaf/0.1/name" :: String) ]
                , "name" .= ("Manu" :: String)
                ]
            expected = Array $ V.singleton $ object
                [ "http://xmlns.com/foaf/0.1/name" .= [ object ["@value" .= ("Manu" :: String)] ]
                ]
        run doc @?= Right expected

    , testCase "@type @id coercion expands string to node ref" $ do
        let doc = object
                [ "@context" .= object
                    [ "homepage" .= object
                        [ "@id"   .= ("http://xmlns.com/foaf/0.1/homepage" :: String)
                        , "@type" .= ("@id" :: String)
                        ]
                    ]
                , "homepage" .= ("http://manu.sporny.org/" :: String)
                ]
            expected = Array $ V.singleton $ object
                [ "http://xmlns.com/foaf/0.1/homepage" .=
                    [ object ["@id" .= ("http://manu.sporny.org/" :: String)] ]
                ]
        run doc @?= Right expected

    , testCase "typed value (xsd:integer)" $ do
        let doc = object
                [ "@context" .= object
                    [ "age" .= object
                        [ "@id"   .= ("http://example/age" :: String)
                        , "@type" .= ("http://www.w3.org/2001/XMLSchema#integer" :: String)
                        ]
                    ]
                , "age" .= (42 :: Int)
                ]
            expected = Array $ V.singleton $ object
                [ "http://example/age" .=
                    [ object
                        [ "@value" .= (42 :: Int)
                        , "@type"  .= ("http://www.w3.org/2001/XMLSchema#integer" :: String)
                        ]
                    ]
                ]
        run doc @?= Right expected

    , testCase "default @language adds @language to plain strings" $ do
        let doc = object
                [ "@context" .= object
                    [ "@language" .= ("en" :: String)
                    , "name"      .= ("http://example/name" :: String)
                    ]
                , "name" .= ("hello" :: String)
                ]
            expected = Array $ V.singleton $ object
                [ "http://example/name" .=
                    [ object
                        [ "@value"    .= ("hello" :: String)
                        , "@language" .= ("en" :: String)
                        ]
                    ]
                ]
        run doc @?= Right expected

    , testCase "@list value preserved" $ do
        let doc = object
                [ "@context" .= object
                    [ "items" .= ("http://example/items" :: String) ]
                , "items" .= object
                    [ "@list" .= ([1, 2] :: [Int]) ]
                ]
        case run doc of
            Right (Array vs) ->
                length vs @?= 1
            other -> assertFailure $ "unexpected: " <> show other

    , testCase "@graph-only top-level unwraps" $ do
        let doc = object
                [ "@context" .= object
                    [ "name" .= ("http://example/name" :: String) ]
                , "@graph" .=
                    [ object [ "name" .= ("A" :: String) ]
                    , object [ "name" .= ("B" :: String) ]
                    ]
                ]
        case run doc of
            Right (Array vs) ->
                length vs @?= 2
            other -> assertFailure $ "unexpected: " <> show other

    , testCase "unmapped term is dropped" $ do
        let doc = object
                [ "@context" .= object
                    [ "name" .= ("http://example/name" :: String) ]
                , "name"     .= ("kept" :: String)
                , "unmapped" .= ("dropped" :: String)
                ]
            expected = Array $ V.singleton $ object
                [ "http://example/name" .=
                    [ object ["@value" .= ("kept" :: String)] ]
                ]
        run doc @?= Right expected
    ]

run :: Value -> Either JsonLdError Value
run v = expandDocument defaultOptions (Document Nothing v)
