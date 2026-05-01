{-# LANGUAGE OverloadedStrings #-}

-- | Reader for the W3C JSON-LD test manifests.
--
-- The manifests live under @tests/json-ld-api/tests/@ and
-- @tests/json-ld-framing/tests/@. Each is itself a JSON-LD document, but
-- has a fixed shape that we can parse as plain JSON; we don't need the
-- processor to bootstrap the test harness.
--
-- See <https://w3c.github.io/json-ld-api/tests/> for the human-readable
-- index.
module Data.JsonLd.Test.Manifest
    ( -- * Manifest model
      Manifest (..)
    , TestCase (..)
    , TestCategory (..)
    , TestKind (..)
    , Expectation (..)
    , TestOption (..)
    , emptyOption
      -- * Loading
    , loadManifest
    , defaultManifestPaths
    ) where

import           Data.Aeson         (Value (..), eitherDecodeFileStrict', withObject, withText, (.!=), (.:), (.:?))
import           Data.Aeson.Types   (Parser, parseEither)
import qualified Data.Aeson.KeyMap  as KM
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Vector        as V
import           System.FilePath    ((</>))

-- | One W3C manifest file.
data Manifest = Manifest
    { manifestPath    :: !FilePath
      -- ^ Where the manifest was loaded from. Test fixture paths are
      -- resolved relative to its directory.
    , manifestName    :: !Text
    , manifestBaseIri :: !Text
      -- ^ The HTTP IRI under which W3C hosts the fixtures. Tests use this
      -- as the base for any document-loader interactions.
    , manifestTests   :: ![TestCase]
    }
    deriving (Eq, Show)

-- | The algorithm a test exercises.
data TestCategory
    = CExpand
    | CCompact
    | CFlatten
    | CFrame
    | CFromRdf
    | CToRdf
    | CHtml
    deriving (Eq, Ord, Show)

-- | The W3C test classification.
data TestKind
    = PositiveEvaluation
    | NegativeEvaluation
    | PositiveSyntax
    deriving (Eq, Ord, Show)

-- | What the test expects.
data Expectation
    = ExpectOutput !FilePath
      -- ^ A fixture file whose contents the algorithm output must match
      -- under JSON-LD object comparison.
    | ExpectError !Text
      -- ^ The exact error code string, e.g. @\"invalid \@id value\"@.
    | ExpectSyntaxOk
      -- ^ Positive syntax tests just require the algorithm to terminate.
    deriving (Eq, Show)

-- | Per-test options (subset; expand as needed). The full original object
-- is preserved in 'optionRaw' so the test runner can pull out fields we
-- don't yet model without re-parsing the manifest.
data TestOption = TestOption
    { optionProcessingMode :: !(Maybe Text)
    , optionSpecVersion    :: !(Maybe Text)
    , optionBase           :: !(Maybe Text)
    , optionExpandContext  :: !(Maybe FilePath)
    , optionCompactArrays  :: !(Maybe Bool)
    , optionUseNativeTypes :: !(Maybe Bool)
    , optionRaw            :: !(Maybe Value)
    }
    deriving (Eq, Show)

emptyOption :: TestOption
emptyOption = TestOption Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data TestCase = TestCase
    { testId          :: !Text
      -- ^ Manifest-local identifier, e.g. @"#t0001"@.
    , testName        :: !Text
    , testPurpose     :: !Text
    , testCategory    :: !TestCategory
    , testKind        :: !TestKind
    , testNormative   :: !Bool
    , testInput       :: !FilePath
    , testFrame       :: !(Maybe FilePath)
    , testContext     :: !(Maybe FilePath)
    , testExpectation :: !Expectation
    , testOption      :: !TestOption
    }
    deriving (Eq, Show)

------------------------------------------------------------------------
-- Loading

loadManifest :: FilePath -> IO (Either String Manifest)
loadManifest path = do
    raw <- eitherDecodeFileStrict' path
    pure $ raw >>= parseEither (manifestParser path)

manifestParser :: FilePath -> Value -> Parser Manifest
manifestParser path = withObject "Manifest" $ \o -> do
    name    <- o .:? "name"    .!= T.pack path
    baseIri <- o .:? "baseIri" .!= ""
    rawSeq  <- o .:? "sequence" .!= V.empty
    tests   <- traverse parseTestCase (V.toList rawSeq)
    pure Manifest
        { manifestPath    = path
        , manifestName    = name
        , manifestBaseIri = baseIri
        , manifestTests   = tests
        }

parseTestCase :: Value -> Parser TestCase
parseTestCase = withObject "TestCase" $ \o -> do
    tid    <- o .:  "@id"
    name   <- o .:? "name"      .!= ""
    purp   <- o .:? "purpose"   .!= ""
    norm   <- o .:? "normative" .!= True
    types  <- parseTypes =<< o .: "@type"
    kind     <- requireKind types
    category <- requireCategory types
    input  <- o .:  "input"
    mFrame <- o .:? "frame"
    mCtx   <- o .:? "context"
    expectation <- parseExpectation kind o
    opt    <- parseOption =<< o .:? "option" .!= Object KM.empty
    pure TestCase
        { testId          = tid
        , testName        = name
        , testPurpose     = purp
        , testCategory    = category
        , testKind        = kind
        , testNormative   = norm
        , testInput       = input
        , testFrame       = mFrame
        , testContext     = mCtx
        , testExpectation = expectation
        , testOption      = opt
        }

parseTypes :: Value -> Parser [Text]
parseTypes (String t) = pure [t]
parseTypes (Array xs) = traverse (withText "test @type element" pure) (V.toList xs)
parseTypes other      = fail $ "@type: expected string or array, got " ++ show other

requireKind :: [Text] -> Parser TestKind
requireKind ts
    | "jld:PositiveEvaluationTest" `elem` ts = pure PositiveEvaluation
    | "jld:NegativeEvaluationTest" `elem` ts = pure NegativeEvaluation
    | "jld:PositiveSyntaxTest"     `elem` ts = pure PositiveSyntax
    | otherwise = fail $ "no recognised test kind in @type: " ++ show ts

requireCategory :: [Text] -> Parser TestCategory
requireCategory ts =
    case [ c | (s, c) <- categoryTable, s `elem` ts ] of
        (c:_) -> pure c
        []    -> fail $ "no recognised test category in @type: " ++ show ts
  where
    categoryTable =
        [ ("jld:ExpandTest",  CExpand)
        , ("jld:CompactTest", CCompact)
        , ("jld:FlattenTest", CFlatten)
        , ("jld:FrameTest",   CFrame)
        , ("jld:FromRDFTest", CFromRdf)
        , ("jld:ToRDFTest",   CToRdf)
        , ("jld:HtmlTest",    CHtml)
        ]

parseExpectation :: TestKind -> KM.KeyMap Value -> Parser Expectation
parseExpectation kind o = case kind of
    PositiveSyntax     -> pure ExpectSyntaxOk
    NegativeEvaluation -> ExpectError  <$> o .: "expectErrorCode"
    PositiveEvaluation -> ExpectOutput <$> o .: "expect"

parseOption :: Value -> Parser TestOption
parseOption (Object o) = do
    pm   <- o .:? "processingMode"
    spec <- o .:? "specVersion"
    base <- o .:? "base"
    ec   <- o .:? "expandContext"
    ca   <- o .:? "compactArrays"
    nt   <- o .:? "useNativeTypes"
    pure $ TestOption pm spec base ec ca nt (Just (Object o))
parseOption Null       = pure emptyOption
parseOption other      = fail $ "option: expected object, got " ++ show other

-- | The manifests we ship with by default. Additional manifests in the
-- same tree (HTML, remote-doc) can be passed to 'loadManifest' directly.
defaultManifestPaths :: [FilePath]
defaultManifestPaths =
    [ "tests/json-ld-api/tests"     </> "expand-manifest.jsonld"
    , "tests/json-ld-api/tests"     </> "compact-manifest.jsonld"
    , "tests/json-ld-api/tests"     </> "flatten-manifest.jsonld"
    , "tests/json-ld-api/tests"     </> "fromRdf-manifest.jsonld"
    , "tests/json-ld-api/tests"     </> "toRdf-manifest.jsonld"
    , "tests/json-ld-api/tests"     </> "html-manifest.jsonld"
    , "tests/json-ld-api/tests"     </> "remote-doc-manifest.jsonld"
    , "tests/json-ld-framing/tests" </> "frame-manifest.jsonld"
    ]
