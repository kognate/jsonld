module Main (main) where

import qualified Test.JsonLd.BridgeSpec  as BridgeSpec
import qualified Test.JsonLd.ContextSpec as ContextSpec
import qualified Test.JsonLd.ExpandSpec  as ExpandSpec
import qualified Test.JsonLd.IriSpec     as IriSpec
import qualified Test.JsonLd.KeywordSpec as KeywordSpec
import qualified Test.JsonLd.W3C         as W3C
import           Test.Tasty              (defaultMain, testGroup)

main :: IO ()
main = do
    w3c <- W3C.loadAll
    defaultMain $ testGroup "jsonld"
        [ testGroup "unit"
            [ IriSpec.tests
            , KeywordSpec.tests
            , BridgeSpec.tests
            , ContextSpec.tests
            , ExpandSpec.tests
            ]
        , w3c
        ]
