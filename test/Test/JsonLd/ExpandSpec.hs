{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the Expansion algorithm. Independent of the W3C
-- suite — we feed a small input map in, run 'expand', and assert on
-- the resulting JSON.
module Test.JsonLd.ExpandSpec
    ( tests
    ) where

import           Data.Aeson         (Value (..), object, (.=))
import qualified Data.Aeson         as A
import qualified Data.Aeson.KeyMap  as KM
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

    , testCase "property-scoped @context applies during recursive expand" $ do
        -- The "owner" property has its own @context that defines "name".
        -- The outer context doesn't, so "name" only resolves under owner.
        let doc = object
                [ "@context" .= object
                    [ "owner" .= object
                        [ "@id"      .= ("http://example/owner" :: String)
                        , "@context" .= object
                            [ "name" .= ("http://example/name" :: String) ]
                        ]
                    ]
                , "owner" .= object [ "name" .= ("Manu" :: String) ]
                ]
            expected = Array $ V.singleton $ object
                [ "http://example/owner" .=
                    [ object
                        [ "http://example/name" .=
                            [ object ["@value" .= ("Manu" :: String)] ]
                        ]
                    ]
                ]
        run doc @?= Right expected

    , testCase "type-scoped @context applies after @type" $ do
        -- @type Person triggers a Person-scoped context that defines "name".
        let doc = object
                [ "@context" .= object
                    [ "Person" .= object
                        [ "@id"      .= ("http://example/Person" :: String)
                        , "@context" .= object
                            [ "name" .= ("http://example/name" :: String) ]
                        ]
                    ]
                , "@type" .= ("Person" :: String)
                , "name"  .= ("Manu" :: String)
                ]
            expected = Array $ V.singleton $ object
                [ "@type" .= [ "http://example/Person" :: String ]
                , "http://example/name" .=
                    [ object ["@value" .= ("Manu" :: String)] ]
                ]
        run doc @?= Right expected

    , testCase "@direction applied via active context" $ do
        let doc = object
                [ "@context" .= object
                    [ "@version"   .= (1.1 :: Double)
                    , "@language"  .= ("ar"  :: String)
                    , "@direction" .= ("rtl" :: String)
                    , "name"       .= ("http://example/name" :: String)
                    ]
                , "name" .= ("سلام" :: String)
                ]
            expected = Array $ V.singleton $ object
                [ "http://example/name" .=
                    [ object
                        [ "@value"     .= ("سلام" :: String)
                        , "@language"  .= ("ar"   :: String)
                        , "@direction" .= ("rtl"  :: String)
                        ]
                    ]
                ]
        run doc @?= Right expected

    , testCase "@direction term slot overrides active context" $ do
        let doc = object
                [ "@context" .= object
                    [ "@version"   .= (1.1 :: Double)
                    , "@direction" .= ("rtl" :: String)
                    , "name" .= object
                        [ "@id"        .= ("http://example/name" :: String)
                        , "@direction" .= Null
                        ]
                    ]
                , "name" .= ("hello" :: String)
                ]
            expected = Array $ V.singleton $ object
                [ "http://example/name" .=
                    [ object [ "@value" .= ("hello" :: String) ] ]
                ]
        run doc @?= Right expected

    , testCase "@language container map expands to language-tagged values" $ do
        let doc = object
                [ "@context" .= object
                    [ "label" .= object
                        [ "@id"        .= ("http://example/label" :: String)
                        , "@container" .= ("@language" :: String)
                        ]
                    ]
                , "label" .= object
                    [ "en" .= ("Hello" :: String)
                    , "fr" .= ("Bonjour" :: String)
                    ]
                ]
        case run doc of
            Right (Array arr)
                | [Object km] <- V.toList arr
                , Just (Array vs) <- KM.lookup "http://example/label" km
                , length vs == 2
                    -> pure ()
            other -> assertFailure $ "unexpected: " <> show other

    , testCase "@language map with @none key omits @language" $ do
        let doc = object
                [ "@context" .= object
                    [ "label" .= object
                        [ "@id"        .= ("http://example/label" :: String)
                        , "@container" .= ("@language" :: String)
                        ]
                    ]
                , "label" .= object
                    [ "@none" .= ("Hello" :: String) ]
                ]
            expected = Array $ V.singleton $ object
                [ "http://example/label" .=
                    [ object [ "@value" .= ("Hello" :: String) ] ]
                ]
        run doc @?= Right expected
    ]

run :: Value -> Either JsonLdError Value
run v = expandDocument defaultOptions (Document Nothing v)
