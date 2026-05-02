{-# LANGUAGE OverloadedStrings #-}

-- | Unit tests for the Context Processing and Create Term Definition
-- algorithms. The strategy: feed in a tiny @\@context@ as JSON, run
-- 'processContext', and assert on the resulting 'ActiveContext'.
module Test.JsonLd.ContextSpec
    ( tests
    ) where

import           Data.Aeson         (Value (Null, String), object, (.=))
import qualified Data.Map.Strict    as Map
import           Test.Tasty         (TestTree, testGroup)
import           Test.Tasty.HUnit   (Assertion, assertFailure, testCase, (@?=))

import           Data.JsonLd.Context
import           Data.JsonLd.Error
import           Data.JsonLd.Iri
import           Data.JsonLd.Types

tests :: TestTree
tests = testGroup "Data.JsonLd.Context"
    [ testGroup "@-keys"
        [ testCase "empty context is a no-op" $ do
            ac <- expectOk (object [])
            ac @?= startCtx

        , testCase "null resets to empty" $ do
            -- Build a context with terms, then nullify.
            ac1 <- expectOk (object ["x" .= ("http://x/" :: String)])
            ac2 <- runWith ac1 Null
            acTerms ac2 @?= Map.empty

        , testCase "@vocab sets vocabulary mapping" $ do
            ac <- expectOk (object ["@vocab" .= ("http://example/" :: String)])
            acVocab ac @?= Just "http://example/"

        , testCase "@vocab null clears" $ do
            ac1 <- expectOk (object ["@vocab" .= ("http://example/" :: String)])
            ac2 <- runWith ac1 (object ["@vocab" .= Null])
            acVocab ac2 @?= Nothing

        , testCase "@base sets base IRI" $ do
            ac <- expectOk (object ["@base" .= ("http://example/" :: String)])
            acBase ac @?= Just (Iri "http://example/")

        , testCase "@language is lower-cased" $ do
            ac <- expectOk (object ["@language" .= ("EN-US" :: String)])
            acLanguage ac @?= Just "en-us"

        , testCase "@direction ltr in 1.1" $ do
            ac <- expectOk (object ["@direction" .= ("ltr" :: String)])
            acDirection ac @?= Just DirLtr

        , testCase "@direction invalid value rejected" $
            expectErr InvalidBaseDirection
                (object ["@direction" .= ("sideways" :: String)])

        , testCase "@version 1.1 keeps mode" $ do
            ac <- expectOk (object ["@version" .= (1.1 :: Double)])
            acProcessingMode ac @?= JsonLd11

        , testCase "@version 1.0 rejected" $
            expectErr InvalidVersionValue
                (object ["@version" .= (1.0 :: Double)])

        , testCase "@import is deferred to Phase 4" $
            expectErr NotImplemented
                (object ["@import" .= ("foo.jsonld" :: String)])

        , testCase "remote string context is deferred" $
            expectErr NotImplemented (String "http://example.com/ctx")
        ]

    , testGroup "Create Term Definition"
        [ testCase "string-form term" $ do
            ac <- expectOk (object ["name" .= ("http://example/name" :: String)])
            tdIri <$> Map.lookup "name" (acTerms ac) @?= Just (Just "http://example/name")

        , testCase "object-form term with @id" $ do
            ac <- expectOk (object
                [ "name" .= object ["@id" .= ("http://example/name" :: String)] ])
            tdIri <$> Map.lookup "name" (acTerms ac) @?= Just (Just "http://example/name")

        , testCase "@type coercion" $ do
            ac <- expectOk (object
                [ "age" .= object
                    [ "@id" .= ("http://example/age" :: String)
                    , "@type" .= ("http://www.w3.org/2001/XMLSchema#integer" :: String)
                    ]
                ])
            (tdType =<< Map.lookup "age" (acTerms ac))
                @?= Just "http://www.w3.org/2001/XMLSchema#integer"

        , testCase "@type @id keyword is preserved" $ do
            ac <- expectOk (object
                [ "ref" .= object
                    [ "@id"   .= ("http://example/ref" :: String)
                    , "@type" .= ("@id" :: String)
                    ]
                ])
            (tdType =<< Map.lookup "ref" (acTerms ac)) @?= Just "@id"

        , testCase "@container @list" $ do
            ac <- expectOk (object
                [ "items" .= object
                    [ "@id" .= ("http://example/items" :: String)
                    , "@container" .= ("@list" :: String)
                    ]
                ])
            (tdContainers <$> Map.lookup "items" (acTerms ac))
                @?= Just [CList]

        , testCase "@container [@graph, @id]" $ do
            ac <- expectOk (object
                [ "g" .= object
                    [ "@id" .= ("http://example/g" :: String)
                    , "@container" .= (["@graph", "@id"] :: [String])
                    ]
                ])
            (tdContainers <$> Map.lookup "g" (acTerms ac))
                @?= Just [CGraph, CId]

        , testCase "@prefix flag" $ do
            ac <- expectOk (object
                [ "ex" .= object
                    [ "@id" .= ("http://example/" :: String)
                    , "@prefix" .= True
                    ]
                ])
            (tdPrefix <$> Map.lookup "ex" (acTerms ac)) @?= Just True

        , testCase "@reverse flag" $ do
            ac <- expectOk (object
                [ "knownBy" .= object
                    [ "@reverse" .= ("http://example/knows" :: String) ]
                ])
            let mTd = Map.lookup "knownBy" (acTerms ac)
            (tdReverse <$> mTd) @?= Just True
            (tdIri     <$> mTd) @?= Just (Just "http://example/knows")

        , testCase "@reverse and @id together rejected" $
            expectErr InvalidReverseProperty
                (object
                    [ "x" .= object
                        [ "@id"      .= ("http://x/" :: String)
                        , "@reverse" .= ("http://y/" :: String)
                        ]
                    ])

        , testCase "term value is null = remove" $ do
            ac1 <- expectOk (object ["x" .= ("http://x/" :: String)])
            ac2 <- runWith ac1 (object ["x" .= Null])
            Map.lookup "x" (acTerms ac2) @?= Nothing

        , testCase "vocab-relative term IRI" $ do
            ac <- expectOk (object
                [ "@vocab" .= ("http://example/" :: String)
                , "name"   .= ("name" :: String)
                ])
            tdIri <$> Map.lookup "name" (acTerms ac)
                @?= Just (Just "http://example/name")

        , testCase "compact-IRI expansion uses prior term" $ do
            -- Aeson 2.x preserves insertion order with the default
            -- `ordered-keymap` flag, so "ex" is processed before "name".
            ac <- expectOk (object
                [ "ex"   .= ("http://example/" :: String)
                , "name" .= ("ex:name" :: String)
                ])
            tdIri <$> Map.lookup "name" (acTerms ac)
                @?= Just (Just "http://example/name")

        , testCase "unknown term-def key rejected" $
            expectErr InvalidTermDefinition
                (object
                    [ "x" .= object
                        [ "@id"     .= ("http://x/" :: String)
                        , "@bogus"  .= ("v" :: String)
                        ]
                    ])

        , testCase "redefining a protected term is rejected" $ do
            ac1 <- expectOk (object
                [ "@protected" .= True
                , "p" .= ("http://example/p" :: String)
                ])
            -- Now try to change p's IRI mapping in a follow-up context.
            let cfg = defaultCtxConfig defaultOptions Nothing
            case processContext cfg ac1 (object ["p" .= ("http://example/q" :: String)]) of
                Left e | errorCode e == ProtectedTermRedefinition -> pure ()
                Left e -> assertFailure $
                    "expected ProtectedTermRedefinition, got " <> show (errorCode e)
                Right _ -> assertFailure "expected redefinition error, got success"

        , testCase "redefining a protected term to the same value is OK" $ do
            ac1 <- expectOk (object
                [ "@protected" .= True
                , "p" .= ("http://example/p" :: String)
                ])
            ac2 <- runWith ac1 (object ["p" .= ("http://example/p" :: String)])
            tdIri <$> Map.lookup "p" (acTerms ac2)
                @?= Just (Just "http://example/p")
        ]

    , testGroup "IRI expansion helper"
        [ testCase "absolute IRI passes through" $
            expandIriCtx startCtx True False "http://x/" @?= Right "http://x/"

        , testCase "term lookup" $ do
            ac <- expectOk (object ["name" .= ("http://x/" :: String)])
            expandIriCtx ac True False "name" @?= Right "http://x/"

        , testCase "vocab fallback" $ do
            ac <- expectOk (object ["@vocab" .= ("http://x/" :: String)])
            expandIriCtx ac True False "name" @?= Right "http://x/name"

        , testCase "blank node passes through" $
            expandIriCtx startCtx True False "_:b0" @?= Right "_:b0"

        , testCase "keyword passes through" $
            expandIriCtx startCtx True False "@id" @?= Right "@id"
        ]

    , testGroup "forward references"
        [ testCase "compact IRI uses prefix defined later in same context" $ do
            -- "name" appears before "ex" so processing it must trigger
            -- the recursive Create Term Definition for "ex".
            ac <- expectOk (object
                [ "name" .= ("ex:name" :: String)
                , "ex"   .= ("http://example/" :: String)
                ])
            tdIri <$> Map.lookup "name" (acTerms ac)
                @?= Just (Just "http://example/name")

        , testCase "object @id uses forward-referenced prefix" $ do
            ac <- expectOk (object
                [ "p"  .= object ["@id" .= ("ex:p" :: String)]
                , "ex" .= ("http://example/" :: String)
                ])
            tdIri <$> Map.lookup "p" (acTerms ac)
                @?= Just (Just "http://example/p")

        , testCase "@type uses forward-referenced prefix" $ do
            ac <- expectOk (object
                [ "age" .= object
                    [ "@id"   .= ("http://example/age" :: String)
                    , "@type" .= ("xsd:integer" :: String)
                    ]
                , "xsd" .= ("http://www.w3.org/2001/XMLSchema#" :: String)
                ])
            (tdType =<< Map.lookup "age" (acTerms ac))
                @?= Just "http://www.w3.org/2001/XMLSchema#integer"

        , testCase "circular term mappings detected" $
            expectErr CyclicIriMapping
                (object
                    [ "a" .= ("b:foo" :: String)
                    , "b" .= ("a:bar" :: String)
                    ])
        ]

    , testGroup "@container combination validation"
        [ testCase "@list + @set rejected" $
            expectErr InvalidContainerMapping
                (object
                    [ "x" .= object
                        [ "@id"        .= ("http://x/" :: String)
                        , "@container" .= (["@list", "@set"] :: [String])
                        ]
                    ])

        , testCase "@graph + @id is valid" $ do
            ac <- expectOk (object
                [ "g" .= object
                    [ "@id"        .= ("http://example/g" :: String)
                    , "@container" .= (["@graph", "@id"] :: [String])
                    ]
                ])
            (tdContainers <$> Map.lookup "g" (acTerms ac))
                @?= Just [CGraph, CId]

        , testCase "@graph + @id + @set is valid" $ do
            ac <- expectOk (object
                [ "g" .= object
                    [ "@id"        .= ("http://example/g" :: String)
                    , "@container" .= (["@graph", "@id", "@set"] :: [String])
                    ]
                ])
            -- Stored in source order; validation accepts the combo.
            (tdContainers <$> Map.lookup "g" (acTerms ac))
                @?= Just [CGraph, CId, CSet]

        , testCase "validContainerCombination matrix" $ do
            validContainerCombination [CList]               @?= True
            validContainerCombination [CSet, CIndex]        @?= True
            validContainerCombination [CGraph, CId]         @?= True
            validContainerCombination [CSet, CGraph, CId]   @?= True
            validContainerCombination [CList, CSet]         @?= False
            validContainerCombination [CType, CLanguage]    @?= False
            validContainerCombination []                    @?= False
        ]

    , testGroup "scoped @context"
        [ testCase "valid scoped context is stored verbatim" $ do
            let ctx = object
                    [ "p" .= object
                        [ "@id" .= ("http://example/p" :: String)
                        , "@context" .= object
                            [ "inner" .= ("http://example/inner" :: String) ]
                        ]
                    ]
            ac <- expectOk ctx
            -- @context is stored on the term as a verbatim Value.
            case Map.lookup "p" (acTerms ac) >>= tdContext of
                Just _  -> pure ()
                Nothing -> assertFailure "expected scoped @context to be stored"

        , testCase "scoped context with invalid local context propagates error" $
            -- Inner @context is a number (not a valid local context).
            expectErr InvalidLocalContext
                (object
                    [ "p" .= object
                        [ "@id" .= ("http://example/p" :: String)
                        , "@context" .= (42 :: Int)
                        ]
                    ])
        ]
    ]

------------------------------------------------------------------------
-- Helpers

startCtx :: ActiveContext
startCtx = emptyActiveContext JsonLd11

run :: Value -> Either JsonLdError ActiveContext
run = runWithEither startCtx

runWithEither :: ActiveContext -> Value -> Either JsonLdError ActiveContext
runWithEither ctx = processContext (defaultCtxConfig defaultOptions Nothing) ctx

runWith :: ActiveContext -> Value -> IO ActiveContext
runWith ctx v = case runWithEither ctx v of
    Right ac -> pure ac
    Left  e  -> assertFailure ("unexpected error: " <> show e)

expectOk :: Value -> IO ActiveContext
expectOk v = case run v of
    Right ac -> pure ac
    Left  e  -> assertFailure ("unexpected error: " <> show e)

expectErr :: JsonLdErrorCode -> Value -> Assertion
expectErr expected v = case run v of
    Left e | errorCode e == expected -> pure ()
    Left e -> assertFailure $
        "expected " <> show expected <> ", got " <> show (errorCode e)
    Right _ -> assertFailure $
        "expected " <> show expected <> ", got success"

