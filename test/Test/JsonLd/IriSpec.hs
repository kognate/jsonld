{-# LANGUAGE OverloadedStrings #-}

-- | Reference resolution conformance, driven by the RFC 3986 §5.4 test
-- vectors plus a handful of structural checks on the parser.
module Test.JsonLd.IriSpec
    ( tests
    ) where

import           Data.Text          (Text)
import qualified Data.Text          as T
import           Test.Tasty         (TestTree, testGroup)
import           Test.Tasty.HUnit   (testCase, (@?=))

import           Data.JsonLd.Iri

tests :: TestTree
tests = testGroup "Data.JsonLd.Iri"
    [ testGroup "parseIriRef"
        [ testCase "absolute http"   $ parseIriRef "http://a/b?c#d" @?= IriRef
            (Just "http") (Just "a") "/b" (Just "c") (Just "d")
        , testCase "scheme only"     $ parseIriRef "g:h" @?= IriRef
            (Just "g") Nothing "h" Nothing Nothing
        , testCase "network-path ref" $ parseIriRef "//g" @?= IriRef
            Nothing (Just "g") "" Nothing Nothing
        , testCase "query only"      $ parseIriRef "?y" @?= IriRef
            Nothing Nothing "" (Just "y") Nothing
        , testCase "fragment only"   $ parseIriRef "#s" @?= IriRef
            Nothing Nothing "" Nothing (Just "s")
        , testCase "empty"           $ parseIriRef "" @?= IriRef
            Nothing Nothing "" Nothing Nothing
        , testCase "trailing empty query" $
            irQuery (parseIriRef "http://a/?") @?= Just ""
        , testCase "no query absent" $
            irQuery (parseIriRef "http://a/") @?= Nothing
        ]

    , testGroup "removeDotSegments §5.2.4"
        [ testCase "spec example 1" $
            removeDotSegments "/a/b/c/./../../g" @?= "/a/g"
        , testCase "spec example 2" $
            removeDotSegments "mid/content=5/../6" @?= "mid/6"
        ]

    , testGroup "isAbsoluteIri / isBlankNodeId"
        [ testCase "http URL"          $ isAbsoluteIri "http://example/x" @?= True
        , testCase "urn"               $ isAbsoluteIri "urn:isbn:0451450523" @?= True
        , testCase "term (no scheme)"  $ isAbsoluteIri "name"            @?= False
        , testCase "leading colon"     $ isAbsoluteIri ":foo"            @?= False
        , testCase "scheme starts num" $ isAbsoluteIri "1http:foo"       @?= False
        , testCase "blank node"        $ isBlankNodeId "_:b0"            @?= True
        , testCase "not a blank node"  $ isBlankNodeId "ex:foo"          @?= False
        ]

    , testGroup "RFC 3986 §5.4.1 normal examples"
        [ resolveCase ref expected | (ref, expected) <- normalExamples ]

    , testGroup "RFC 3986 §5.4.2 abnormal examples"
        [ resolveCase ref expected | (ref, expected) <- abnormalExamples ]
    ]

base :: Text
base = "http://a/b/c/d;p?q"

resolveCase :: Text -> Text -> TestTree
resolveCase ref expected =
    testCase (T.unpack ref <> " -> " <> T.unpack expected) $
        resolveRef base ref @?= expected

normalExamples :: [(Text, Text)]
normalExamples =
    [ ("g:h"      , "g:h")
    , ("g"        , "http://a/b/c/g")
    , ("./g"      , "http://a/b/c/g")
    , ("g/"       , "http://a/b/c/g/")
    , ("/g"       , "http://a/g")
    , ("//g"      , "http://g")
    , ("?y"       , "http://a/b/c/d;p?y")
    , ("g?y"      , "http://a/b/c/g?y")
    , ("#s"       , "http://a/b/c/d;p?q#s")
    , ("g#s"      , "http://a/b/c/g#s")
    , ("g?y#s"    , "http://a/b/c/g?y#s")
    , (";x"       , "http://a/b/c/;x")
    , ("g;x"      , "http://a/b/c/g;x")
    , ("g;x?y#s"  , "http://a/b/c/g;x?y#s")
    , (""         , "http://a/b/c/d;p?q")
    , ("."        , "http://a/b/c/")
    , ("./"       , "http://a/b/c/")
    , (".."       , "http://a/b/")
    , ("../"      , "http://a/b/")
    , ("../g"     , "http://a/b/g")
    , ("../.."    , "http://a/")
    , ("../../"   , "http://a/")
    , ("../../g"  , "http://a/g")
    ]

abnormalExamples :: [(Text, Text)]
abnormalExamples =
    [ ("../../../g"   , "http://a/g")
    , ("../../../../g", "http://a/g")
    , ("/./g"         , "http://a/g")
    , ("/../g"        , "http://a/g")
    , ("g."           , "http://a/b/c/g.")
    , (".g"           , "http://a/b/c/.g")
    , ("g.."          , "http://a/b/c/g..")
    , ("..g"          , "http://a/b/c/..g")
    , ("./../g"       , "http://a/b/g")
    , ("./g/."        , "http://a/b/c/g/")
    , ("g/./h"        , "http://a/b/c/g/h")
    , ("g/../h"       , "http://a/b/c/h")
    , ("g;x=1/./y"    , "http://a/b/c/g;x=1/y")
    , ("g;x=1/../y"   , "http://a/b/c/y")
    , ("g?y/./x"      , "http://a/b/c/g?y/./x")
    , ("g?y/../x"     , "http://a/b/c/g?y/../x")
    , ("g#s/./x"      , "http://a/b/c/g#s/./x")
    , ("g#s/../x"     , "http://a/b/c/g#s/../x")
    ]
