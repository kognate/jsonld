{-# LANGUAGE OverloadedStrings #-}

module Test.JsonLd.KeywordSpec
    ( tests
    ) where

import           Test.Tasty         (TestTree, testGroup)
import           Test.Tasty.HUnit   (testCase, (@?=))

import           Data.JsonLd.Keyword

tests :: TestTree
tests = testGroup "Data.JsonLd.Keyword"
    [ testCase "round-trip every keyword" $
        let rt k = parseKeyword (keywordText k) @?= Just k
        in mapM_ rt allKeywords

    , testCase "isKeyword recognises @id"        $ isKeyword "@id"        @?= True
    , testCase "isKeyword rejects @unknown"      $ isKeyword "@unknown"   @?= False
    , testCase "isKeyword rejects bare id"       $ isKeyword "id"         @?= False

    , testCase "isKeywordLike accepts undefined" $ isKeywordLike "@xyz"   @?= True
    , testCase "isKeywordLike rejects digits"    $ isKeywordLike "@123"   @?= False
    , testCase "isKeywordLike rejects bare @"    $ isKeywordLike "@"      @?= False
    , testCase "isKeywordLike accepts mixed alpha" $
        isKeywordLike "@AbC" @?= True
    , testCase "isKeywordLike rejects punctuation" $
        isKeywordLike "@a-b" @?= False
    ]
