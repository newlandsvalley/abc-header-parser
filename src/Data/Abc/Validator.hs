{-# LANGUAGE NamedFieldPuns #-}

module Data.Abc.Validator
  ( ValidatedHeaders
  , buildHeaderMap
  , validateHeaders
  , validateTitle
  , validateReferenceNumber
  , validateKeySignature
  , validateRhythm
  , normaliseKeySignature
  ,
  ) where

import Data.Validation
-- import Control.Lens
import Data.Text (Text, pack, toLower, unpack)
import qualified Data.Text as Text (null)
import Data.Char (toUpper)
import qualified Data.Char as Char (toLower)
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
         Success $ pack (normaliseKeySignature (unpack h))
         -- keySignature (unpack h)
    Nothing -> Failure  ["No key signature (K:) present."]

validateRhythm :: Genre -> HeaderMap -> Validation [String] Text
validateRhythm genre hdrs =
  case Map.lookup Rhythm hdrs of
    Just h  ->
      let
        proposedRhythm = unpack $ toLower h
      in
        case genre of
          Irish ->
            case generalisedCeltic proposedRhythm of
              Just rhythm ->
                Success rhythm
              Nothing ->
                specificallyIrish proposedRhythm
          Klezmer ->
            klezmer proposedRhythm
          Scandi ->
            scandi proposedRhythm
          Scottish ->
            case generalisedCeltic proposedRhythm of
              Just rhythm ->
                Success rhythm
              Nothing ->
                specificallyScottish proposedRhythm
    Nothing -> Failure  ["No rhythm (R:) present."]

specificallyIrish :: String -> Validation [String] Text
specificallyIrish r =
  case r of
    "highland" -> Success $ pack $ show Highland
    "mazurka" -> Success $ pack $ show Mazurka
    "slide"    -> Success $ pack $ show Slide
    _ -> Failure ["unrecognized rhythm for the Irish genre: " <> r <> "."]

specificallyScottish :: String -> Validation [String] Text
specificallyScottish r =
  case r of
    "schottische" -> Success $ pack $ show Schottische
    "strathspey" -> Success $ pack $ show Strathspey
    _ -> Failure ["unrecognized rhythm for the Scottish genre: " <> r <> "."]

scandi :: String -> Validation [String] Text
scandi r =
  case r of
    "brudmarsch"   -> Success $ pack $ show Brudmarsch
    "engelska"     -> Success $ pack $ show Engelska
    "långdans"     -> Success $ pack $ show Långdans
    "marsch"       -> Success $ pack $ show Marsch
    "polka"        -> Success $ pack $ show Polka
    "polska"       -> Success $ pack $ show Polska
    "schottis"     -> Success $ pack $ show Schottis
    "sekstur"      -> Success $ pack $ show Sekstur
    "skänklåt"     -> Success $ pack $ show Skänklåt
    "slängpolska"  -> Success $ pack $ show Slängpolska
    "waltz"        -> Success $ pack $ show Waltz
    _ -> Failure ["unrecognized rhythm for the Scandi genre: " <> r <> "."]

klezmer :: String -> Validation [String] Text
klezmer r =
  case r of
    "bulgar"    -> Success $ pack $ show Bulgar
    "csardas"   -> Success $ pack $ show Csardas
    "doina"     -> Success $ pack $ show Doina
    "freylekhs" -> Success $ pack $ show Freylekhs
    "khosidl"   -> Success $ pack $ show Khosidl
    "hora"      -> Success $ pack $ show Hora
    "honga"     -> Success $ pack $ show Honga
    "hopak"     -> Success $ pack $ show Hopak
    "kasatchok" -> Success $ pack $ show Kasatchok
    "kolomeyke" -> Success $ pack $ show Kolomeyke
    "sher"      -> Success $ pack $ show Sher
    "sirba"     -> Success $ pack $ show Sirba
    "skotshne"  -> Success $ pack $ show Skotshne
    "taksim"    -> Success $ pack $ show Taksim
    "terkish"   -> Success $ pack $ show Terkish
    _ -> Failure ["unrecognized rhythm for the klezmer genre: " <> r <> "."]

generalisedCeltic :: String -> Maybe Text
generalisedCeltic r =
  case r of
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
normaliseKeySignature :: String -> String
normaliseKeySignature s =
  case (length keySig) of
    0 ->
      -- shouldn't happen because we already reject empty strings
      "unknown"
    1 ->
      -- simple major key
      (toUpper (head keySig)) : (show Major)
    2 ->
      simpleMinorKey (head keySig) (last keySig)
    _ ->
      otherMode (head keySig) (tail keySig)
    where
      keySig = map (Char.toLower) $ filter ( /= ' ') s

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
