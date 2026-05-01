module Main (main) where

import qualified Test.JsonLd.W3C as W3C
import           Test.Tasty      (defaultMain, testGroup)

main :: IO ()
main = do
    w3c <- W3C.loadAll
    defaultMain $ testGroup "jsonld" [w3c]
