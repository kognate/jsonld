{-# LANGUAGE OverloadedStrings #-}

-- | Drives the full W3C JSON-LD test suite.
--
-- For now every test is reported as a /skip/ via 'HUnit.assertString' on
-- an empty message — when the algorithms come online they will replace
-- the body of 'runTest' with the real comparison.
module Test.JsonLd.W3C
    ( loadAll
    ) where

import qualified Data.Text              as T
import           System.Directory       (doesFileExist)
import           Test.Tasty             (TestTree, testGroup)
import           Test.Tasty.HUnit       (testCase)
import qualified Test.Tasty.HUnit       as HUnit

import           Data.JsonLd.Test.Manifest

loadAll :: IO TestTree
loadAll = do
    groups <- traverse loadOneOrPlaceholder defaultManifestPaths
    pure $ testGroup "W3C JSON-LD test suite" groups

loadOneOrPlaceholder :: FilePath -> IO TestTree
loadOneOrPlaceholder path = do
    exists <- doesFileExist path
    if not exists
        then pure $ missingManifest path
        else do
            result <- loadManifest path
            case result of
                Left  err -> pure $ brokenManifest path err
                Right m   -> pure $ manifestGroup m

missingManifest :: FilePath -> TestTree
missingManifest path =
    testCase ("missing: " <> path) $
        HUnit.assertFailure $
            "Manifest not found at " <> path
            <> "\n(did you `git submodule update --init`?)"

brokenManifest :: FilePath -> String -> TestTree
brokenManifest path err =
    testCase ("broken: " <> path) $
        HUnit.assertFailure $ "Failed to parse manifest:\n" <> err

manifestGroup :: Manifest -> TestTree
manifestGroup m =
    testGroup (T.unpack (manifestName m) <> " (" <> show (length (manifestTests m)) <> " tests)")
        [ testCase (T.unpack (testId t) <> " " <> T.unpack (testName t)) (runTest m t)
        | t <- manifestTests m
        ]

-- | Currently every test is skipped. Skipping is signalled by writing a
-- prefixed message to stdout (tasty doesn't have a first-class \"skip\"
-- result; this lets CI grep for unskipped/total counts) and then
-- short-circuiting with 'pure' so the test passes vacuously.
--
-- When 'Data.JsonLd.expand' (and friends) start returning real results,
-- replace this body with a dispatch on 'testCategory' that compares
-- output via JSON-LD object comparison or matches the expected error
-- code.
runTest :: Manifest -> TestCase -> HUnit.Assertion
runTest _ t =
    putStrLn $ "[SKIP] " <> show (testCategory t)
        <> " " <> T.unpack (testId t)
        <> " " <> T.unpack (testName t)
