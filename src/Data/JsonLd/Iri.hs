{-# LANGUAGE OverloadedStrings #-}

-- | IRI primitives.
--
-- Implements the parts of RFC 3986 §5 (\"Reference Resolution\") that
-- JSON-LD relies on whenever a relative reference must be resolved
-- against a base IRI. The grammar is pure ASCII — full IRI (RFC 3987)
-- support is folded in at the 'Text' level, since nothing here
-- inspects the bytes of the path beyond the structural delimiters
-- (@:@, @\/@, @?@, @#@).
module Data.JsonLd.Iri
    ( -- * Types
      Iri (..)
    , BlankNodeId (..)
      -- * Predicates
    , isAbsoluteIri
    , isBlankNodeId
      -- * RFC 3986 reference resolution
    , IriRef (..)
    , parseIriRef
    , recompose
    , removeDotSegments
    , resolveRef
    , resolveIri
    ) where

import           Control.Applicative ((<|>))
import           Data.Maybe          (isJust)
import           Data.Text           (Text)
import qualified Data.Text           as T

newtype Iri = Iri { unIri :: Text }
    deriving (Eq, Ord, Show)

newtype BlankNodeId = BlankNodeId { unBlankNodeId :: Text }
    deriving (Eq, Ord, Show)

-- | True if @t@ has the structural shape of an absolute IRI: a scheme
-- (one alpha then alpha\/digit\/+\/-\/.) followed by @:@. This is the
-- cheap check the spec uses to distinguish absolute IRIs from terms.
isAbsoluteIri :: Text -> Bool
isAbsoluteIri t = case T.uncons t of
    Just (c, _) | isAsciiAlpha c ->
        case T.break (\x -> x == ':' || x == '/' || x == '?' || x == '#') t of
            (scheme, rest)
                | not (T.null rest), T.head rest == ':', T.all isSchemeChar (T.tail scheme)
                  -> True
            _   -> False
    _ -> False

isBlankNodeId :: Text -> Bool
isBlankNodeId = T.isPrefixOf "_:"

------------------------------------------------------------------------
-- RFC 3986 §3 components

-- | A parsed IRI reference. Each delimited component is 'Just' iff the
-- delimiter (@scheme:@, @\/\/@, @?@, @#@) was present in the input.
-- This distinction matters: @http:\/\/example\/?@ has @irQuery = Just \"\"@,
-- while @http:\/\/example\/@ has @irQuery = Nothing@.
data IriRef = IriRef
    { irScheme    :: !(Maybe Text)
    , irAuthority :: !(Maybe Text)
    , irPath      :: !Text
    , irQuery     :: !(Maybe Text)
    , irFragment  :: !(Maybe Text)
    }
    deriving (Eq, Show)

-- | Parser corresponding to the RFC 3986 Appendix B regex
-- @^(([^:\/?#]+):)?(\/\/([^\/?#]*))?([^?#]*)(\\?([^#]*))?(#(.*))?@.
parseIriRef :: Text -> IriRef
parseIriRef input =
    let (scheme, t1)       = takeScheme input
        (authority, t2)    = takeAuthority t1
        (path, t3)         = T.break (\c -> c == '?' || c == '#') t2
        (query, t4)        = takeQuery t3
        fragment           = takeFragment t4
    in IriRef
        { irScheme    = scheme
        , irAuthority = authority
        , irPath      = path
        , irQuery     = query
        , irFragment  = fragment
        }

takeScheme :: Text -> (Maybe Text, Text)
takeScheme t = case T.break (\c -> c == ':' || c == '/' || c == '?' || c == '#') t of
    (s, rest)
        | not (T.null rest)
        , T.head rest == ':'
        , validScheme s -> (Just s, T.tail rest)
    _ -> (Nothing, t)
  where
    validScheme s = case T.uncons s of
        Just (c, cs) -> isAsciiAlpha c && T.all isSchemeChar cs
        Nothing      -> False

takeAuthority :: Text -> (Maybe Text, Text)
takeAuthority t
    | "//" `T.isPrefixOf` t =
        let (a, rest) = T.break (\c -> c == '/' || c == '?' || c == '#') (T.drop 2 t)
        in (Just a, rest)
    | otherwise = (Nothing, t)

takeQuery :: Text -> (Maybe Text, Text)
takeQuery t = case T.uncons t of
    Just ('?', rest) ->
        let (q, more) = T.break (== '#') rest in (Just q, more)
    _ -> (Nothing, t)

takeFragment :: Text -> Maybe Text
takeFragment t = case T.uncons t of
    Just ('#', rest) -> Just rest
    _                -> Nothing

-- | Inverse of 'parseIriRef'.
recompose :: IriRef -> Text
recompose r = mconcat
    [ maybe T.empty (<> ":")    (irScheme r)
    , maybe T.empty ("//" <>)   (irAuthority r)
    , irPath r
    , maybe T.empty ("?" <>)    (irQuery r)
    , maybe T.empty ("#" <>)    (irFragment r)
    ]

------------------------------------------------------------------------
-- RFC 3986 §5.2.4 remove_dot_segments

-- | Strip @.@ and @..@ segments from a path per RFC 3986 §5.2.4.
removeDotSegments :: Text -> Text
removeDotSegments = go T.empty
  where
    go out inp
        | T.null inp                  = out
        | "../" `T.isPrefixOf` inp    = go out (T.drop 3 inp)
        | "./"  `T.isPrefixOf` inp    = go out (T.drop 2 inp)
        | "/./" `T.isPrefixOf` inp    = go out (T.cons '/' (T.drop 3 inp))
        | inp == "/."                 = go out (T.singleton '/')
        | "/../" `T.isPrefixOf` inp   = go (dropLastSeg out) (T.cons '/' (T.drop 4 inp))
        | inp == "/.."                = go (dropLastSeg out) (T.singleton '/')
        | inp == "." || inp == ".."   = go out T.empty
        | otherwise                   =
            let (seg, rest) = takeSeg inp in go (out <> seg) rest

    -- The first segment of @inp@ is the leading @\/@ (if any) plus
    -- everything up to the next @\/@.
    takeSeg t = case T.uncons t of
        Just (c, cs) ->
            let (seg', rest) = T.break (== '/') cs
            in (T.cons c seg', rest)
        Nothing -> (T.empty, T.empty)

    -- Drop the trailing @\/segment@ (or just the trailing @\/@ if the
    -- output ends in one). If there's no @\/@ at all, drop everything.
    dropLastSeg t = case T.breakOnEnd "/" t of
        ("", _)     -> T.empty
        (prefix, _) -> T.dropEnd 1 prefix

------------------------------------------------------------------------
-- RFC 3986 §5.3 transform_references

-- | Resolve a reference @ref@ against @base@. If @base@ does not have a
-- scheme the result is whatever the algorithm produces — RFC 3986
-- requires the base to be absolute, but we don't enforce it here so the
-- caller can decide what to do (some JSON-LD inputs deliberately have
-- no base).
resolveRef :: Text -> Text -> Text
resolveRef base ref = recompose (transform (parseIriRef base) (parseIriRef ref))

resolveIri :: Iri -> Text -> Iri
resolveIri (Iri base) ref = Iri (resolveRef base ref)

transform :: IriRef -> IriRef -> IriRef
transform baseRef rRef
    | isJust (irScheme rRef) =
        rRef { irPath = removeDotSegments (irPath rRef) }

    | isJust (irAuthority rRef) =
        rRef
            { irScheme = irScheme baseRef
            , irPath   = removeDotSegments (irPath rRef)
            }

    | T.null (irPath rRef) =
        baseRef
            { irQuery    = irQuery rRef <|> irQuery baseRef
            , irFragment = irFragment rRef
            }

    | "/" `T.isPrefixOf` irPath rRef =
        IriRef
            { irScheme    = irScheme baseRef
            , irAuthority = irAuthority baseRef
            , irPath      = removeDotSegments (irPath rRef)
            , irQuery     = irQuery rRef
            , irFragment  = irFragment rRef
            }

    | otherwise =
        IriRef
            { irScheme    = irScheme baseRef
            , irAuthority = irAuthority baseRef
            , irPath      = removeDotSegments (mergePaths baseRef (irPath rRef))
            , irQuery     = irQuery rRef
            , irFragment  = irFragment rRef
            }

-- | RFC 3986 §5.2.3 merge.
mergePaths :: IriRef -> Text -> Text
mergePaths base refPath
    | isJust (irAuthority base) && T.null (irPath base) = T.cons '/' refPath
    | otherwise = case T.breakOnEnd "/" (irPath base) of
        ("", _)     -> refPath
        (prefix, _) -> prefix <> refPath

------------------------------------------------------------------------
-- ASCII helpers

isAsciiAlpha :: Char -> Bool
isAsciiAlpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

isSchemeChar :: Char -> Bool
isSchemeChar c =
    isAsciiAlpha c
        || (c >= '0' && c <= '9')
        || c == '+'
        || c == '-'
        || c == '.'
