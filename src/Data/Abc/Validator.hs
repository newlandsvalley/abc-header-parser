{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Abc.Validator
  ( ValidatedHeaders(..)
  , buildHeaderMap
  , validateHeaders
  , validateTitle
  , validateReferenceNumber
  , validateKeySignature
  , validateRhythm
  , normaliseKeySignature
  , normaliseRhythm
  ,
  ) where

import Data.Validation
-- import Control.Lens
import Data.Text (Text, pack, unpack)
import qualified Data.Text as Text (null)
import Data.Char (toLower, toUpper)
import qualified Data.Map as Map (fromList, lookup)
import Data.Maybe (Maybe(..))
import Data.List (filter, head, reverse, take)
import Data.Abc
import Data.Genre


-- Datatype for validated headers
data ValidatedHeaders =
  ValidatedHeaders
    { title  :: Text
    , ref    :: Text
    , key    :: Text
    , rhythm :: Text
    }
    deriving (Eq, Show)

buildHeaderMap :: Headers -> HeaderMap
buildHeaderMap =
  Map.fromList . reverse

-- | validate all the essential headers
validateHeaders :: Genre -> HeaderMap -> Validation [String] ValidatedHeaders
validateHeaders genre hdrs =
  pure ValidatedHeaders <*> vTitle <*> vRef <*> vKey <*> vRhythm
  where
    vTitle = validateTitle hdrs
    vRef = validateReferenceNumber hdrs
    vKey = validateKeySignature hdrs
    vRhythm = validateRhythm genre hdrs

validateTitle :: HeaderMap -> Validation [String] Text
validateTitle hdrs =
  case Map.lookup Title hdrs of
    Just h  ->
       if (Text.null h) then
         Failure  ["Tile (T:) is empty."]
       else
         Success  h
    Nothing -> Failure  ["No title (T:) present."]

validateReferenceNumber :: HeaderMap -> Validation [String] Text
validateReferenceNumber hdrs =
  case Map.lookup ReferenceNumber hdrs of
    Just h  ->
       if (Text.null h) then
         Failure  ["Reference number (X:) is empty."]
       else
         Success  h
    Nothing -> Failure  ["No reference number (X:) present."]

-- | validate a key signature and also attempt to normalise it
validateKeySignature :: HeaderMap -> Validation [String] Text
validateKeySignature hdrs =
  case Map.lookup Key hdrs of
    Just h  ->
       if (Text.null h) then
         Failure  ["Key signatue (K:) is empty."]
       else
         Success $ (normaliseKeySignature h)
         -- keySignature (unpack h)
    Nothing -> Failure  ["No key signature (K:) present."]

validateRhythm :: Genre -> HeaderMap -> Validation [String] Text
validateRhythm genre hdrs =
  case Map.lookup Rhythm hdrs of
    Just h  ->
      let
        proposedRhythm = unpack h
      in
        case genre of
          Irish ->
            case generalisedCelticNormalisation proposedRhythm of
              Just rhythm ->
                Success rhythm
              Nothing ->
                specificallyIrish proposedRhythm
          Klezmer ->
            klezmer proposedRhythm
          Scandi ->
            scandi proposedRhythm
          Scottish ->
            case generalisedCelticNormalisation proposedRhythm of
              Just rhythm ->
                Success rhythm
              Nothing ->
                specificallyScottish proposedRhythm
    Nothing -> Failure  ["No rhythm (R:) present."]

normaliseRhythm :: Genre -> Text -> Text
normaliseRhythm genre proposedRhythm =
  case genre of
    Irish ->
      case generalisedCelticNormalisation (unpack proposedRhythm) of
        Just rhythm ->
          rhythm
        Nothing ->
          case specificallyIrishNormalisation (unpack proposedRhythm) of
            Just rhythm ->
              rhythm
            Nothing ->
              proposedRhythm

    Klezmer ->
      case klezmerNormalisation (unpack proposedRhythm) of
        Just rhythm ->
          rhythm
        Nothing ->
          proposedRhythm

    Scandi ->
      case scandiNormalisation (unpack proposedRhythm) of
        Just rhythm ->
          rhythm
        Nothing ->
          proposedRhythm

    Scottish ->
      case generalisedCelticNormalisation (unpack proposedRhythm) of
        Just rhythm ->
          rhythm
        Nothing ->
          case specificallyScottishNormalisation (unpack proposedRhythm) of
            Just rhythm ->
              rhythm
            Nothing ->
              proposedRhythm


specificallyIrishNormalisation :: String -> Maybe Text
specificallyIrishNormalisation r =
  case (fmap toLower r) of
    "highland" -> Just $ pack $ show Highland
    "mazurka"  -> Just $ pack $ show Mazurka
    "slide"    -> Just $ pack $ show Slide
    _ -> Nothing

specificallyIrish :: String -> Validation [String] Text
specificallyIrish r =
  case specificallyIrishNormalisation r of
    Just n ->
      Success n
    _ ->
      Failure ["unrecognized rhythm for the Irish genre: " <> r <> "."]

specificallyScottishNormalisation :: String -> Maybe Text
specificallyScottishNormalisation r =
  case (fmap toLower r) of
    "schottische" -> Just $ pack $ show Schottische
    "strathspey" -> Just $ pack $ show Strathspey
    _ -> Nothing

specificallyScottish :: String -> Validation [String] Text
specificallyScottish r =
  case specificallyScottishNormalisation r of
    Just n ->
      Success n
    _ ->
      Failure ["unrecognized rhythm for the Scottish genre: " <> r <> "."]

scandiNormalisation :: String -> Maybe Text
scandiNormalisation r =
  case (fmap toLower r) of
    "brudmarsch"   -> Just $ pack $ show Brudmarsch
    "engelska"     -> Just $ pack $ show Engelska
    "långdans"     -> Just $ pack $ show Långdans
    "marsch"       -> Just $ pack $ show Marsch
    "polka"        -> Just $ pack $ show Polka
    "polska"       -> Just $ pack $ show Polska
    "schottis"     -> Just $ pack $ show Schottis
    "sekstur"      -> Just $ pack $ show Sekstur
    "skänklåt"     -> Just $ pack $ show Skänklåt
    "slängpolska"  -> Just $ pack $ show Slängpolska
    "waltz"        -> Just $ pack $ show Waltz
    _ -> Nothing

scandi :: String -> Validation [String] Text
scandi r =
  case scandiNormalisation r of
    Just n ->
      Success n
    _ ->
      Failure ["unrecognized rhythm for the Scandi genre: " <> r <> "."]

klezmerNormalisation :: String -> Maybe Text
klezmerNormalisation r =
  case (fmap toLower r) of
    "bulgar"    -> Just $ pack $ show Bulgar
    "csardas"   -> Just $ pack $ show Csardas
    "doina"     -> Just $ pack $ show Doina
    "freylekhs" -> Just $ pack $ show Freylekhs
    "khosidl"   -> Just $ pack $ show Khosidl
    "hora"      -> Just $ pack $ show Hora
    "honga"     -> Just $ pack $ show Honga
    "hopak"     -> Just $ pack $ show Hopak
    "kasatchok" -> Just $ pack $ show Kasatchok
    "kolomeyke" -> Just $ pack $ show Kolomeyke
    "sher"      -> Just $ pack $ show Sher
    "sirba"     -> Just $ pack $ show Sirba
    "skotshne"  -> Just $ pack $ show Skotshne
    "taksim"    -> Just $ pack $ show Taksim
    "terkish"   -> Just $ pack $ show Terkish
    _ -> Nothing


klezmer :: String -> Validation [String] Text
klezmer r =
  case klezmerNormalisation r of
    Just n ->
      Success n
    _ ->
      Failure ["unrecognized rhythm for the klezmer genre: " <> r <> "."]

generalisedCelticNormalisation :: String -> Maybe Text
generalisedCelticNormalisation r =
  case (fmap toLower r) of
    "barndance"  -> Just $ pack $ show Barndance
    "barn dance" -> Just $ pack $ show Barndance
    "hornpipe"   -> Just $ pack $ show Hornpipe
    "jig"        -> Just $ pack $ show Jig
    "march"      -> Just $ pack $ show March
    "polka"      -> Just $ pack $ show CelticPolka
    "reel"       -> Just $ pack $ show Reel
    "slipjig"    -> Just $ pack $ show SlipJig
    "slip jig"   -> Just $ pack $ show SlipJig
    "waltz"      -> Just $ pack $ show CelticWaltz
    _            -> Nothing


-- | This is a weak form of validation.  The idea is that we want to support
-- | queries against ABC metadata and so we attempt, as far as possible, to
-- | normalise the form of a key signature in both the saved metadata and
-- | also in the query.  ABC key signatures in the wild are fairly free format
-- | and this approach increases the chance of finding matches.
normaliseKeySignature :: Text -> Text
normaliseKeySignature s =
  case (length keySig) of
    0 ->
      -- shouldn't happen because we already reject empty strings
      "unknown"
    1 ->
      -- simple major key
      pack $ (toUpper (head keySig)) : (show Major)
    2 ->
      pack $ simpleMinorKey (head keySig) (last keySig)
    _ ->
      pack $ otherMode (head keySig) (tail keySig)
    where
      keySig = map toLower $ filter ( /= ' ') (unpack s)

simpleMinorKey :: Char -> Char -> String
simpleMinorKey c marker =
  if (marker == 'm') then
    (toUpper c) : (show Minor)
  else
    [ (toUpper c), marker ]

otherMode :: Char -> String -> String
otherMode c mode =
  case (take 3 mode) of
    "maj" -> (toUpper c) : (show Major)
    "min" -> (toUpper c) : (show Minor)
    "ion" -> (toUpper c) : (show Major)
    "dor" -> (toUpper c) : (show Dorian)
    "phr" -> (toUpper c) : (show Phrygian)
    "lid" -> (toUpper c) : (show Lydian)
    "mix" -> (toUpper c) : (show Mixolydian)
    "aeo" -> (toUpper c) : (show Minor)
    "loc" -> (toUpper c) : (show Locrian)
    other -> (toUpper c) : other
