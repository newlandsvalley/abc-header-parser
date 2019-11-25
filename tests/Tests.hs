{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Abc
import Data.Genre
import Data.Abc.Validator (ValidatedHeaders, buildHeaderMap, validateHeaders
                          , normaliseKeySignature)
import Data.Abc.Parser (abcParse, headersParse)
import Data.GenreParser (genreParse)
import Data.Text (pack)
import Data.List (length)
import Data.Bifunctor (second)
import Data.Either (isLeft, isRight, fromRight)
import Data.Validation (Validation(..), toEither)
import Test.Hspec

countAbcHeaders :: Abc -> Int
countAbcHeaders (Abc {headers, body}) =
  length headers

countHeaders ::  Headers -> Int
countHeaders headers =
  length headers

validateHeadersForGenre ::  Genre -> Headers -> Validation [String] ValidatedHeaders
validateHeadersForGenre genre  =
  (validateHeaders Scandi) . buildHeaderMap

main :: IO ()
main = hspec $ do

  describe "The ABC parser" $ do
    it "parses simple ABC headers" $ do
      let
        result :: Either String Int
        result = second countAbcHeaders $ abcParse (pack augustsson)
      result `shouldBe` (Right 9)
    it "rejects completely invalid ABC" $ do
      let
        result = abcParse (pack badAbc)
      (isLeft result) `shouldBe` True

  describe "The headers parser" $ do
    it "parses simple headers" $ do
      -- checkParses augustsson
      let
        result :: Either String Int
        result = second countHeaders $ headersParse (pack augustsson)
      result `shouldBe` (Right 9)
    it "rejects completely invalid ABC" $ do
      let
        result = headersParse (pack badAbc)
      (isLeft result) `shouldBe` True


  describe "The genre parser" $ do
    it "accepts valid genres" $ do
      (genreParse "scandi") `shouldBe` (Right Scandi)
    it "rejects invalid genres" $ do
      (genreParse "foo") `shouldBe` (Left "unknown genre foo")


  describe "The validator" $ do
    it "accepts correct headers" $ do
      let
        result :: Validation [String] ValidatedHeaders
        result = fromRight (Failure ["Parse fail."]) $
                   second (validateHeadersForGenre Scandi) $
                   headersParse (pack augustsson)
      ((isRight . toEither) result) `shouldBe` True
    it "validates missing reference number" $ do
      let
        result :: Validation [String] ValidatedHeaders
        result = fromRight (Failure ["Parse fail."]) $
                       second (validateHeadersForGenre Scandi) $
                       headersParse (pack norefno)
      (toEither result) `shouldBe` (Left ["No reference number (X:) present."])
    it "validates missing title" $ do
      let
        result :: Validation [String] ValidatedHeaders
        result = fromRight (Failure ["Parse fail."]) $
                       second (validateHeadersForGenre Scandi) $
                       headersParse (pack notitle)
      (toEither result) `shouldBe` (Left ["No title (T:) present."])
    it "validates incorrect rhythm for genre" $ do
      let
        result :: Validation [String] ValidatedHeaders
        result = fromRight (Failure ["Parse fail."]) $
                       second (validateHeadersForGenre Scandi) $
                       headersParse (pack badrhythm)
      (toEither result) `shouldBe` (Left ["unrecognized rhythm for the Scandi genre: reel."])

  describe "The key signature normaliser" $ do
    it "normalises simple major keys" $ do
      (normaliseKeySignature "G") `shouldBe` "GMajor"
    it "normalises simple minor keys" $ do
      (normaliseKeySignature "Gm") `shouldBe` "GMinor"
    it "expands shorthand modal keys" $ do
      (normaliseKeySignature "GMix") `shouldBe` "GMixolydian"
    it "ignores spaces in keys" $ do
      (normaliseKeySignature "G DOR") `shouldBe` "GDorian"
    it "leaves other key formats unchanged - albeit capitalised differently" $ do
      (normaliseKeySignature "gFoo") `shouldBe` "Gfoo"


badAbc :: String
badAbc =
  "foo\r\n"

augustsson :: String
augustsson =
  "X:1\r\n"
  <> "T:Engelska efter Albert Augustsson\r\n"
  <> "N:From the playing of Albert Augustsson, Bohuslän county.\r\n"
  <> "M:4/4\r\n"
  <> "R:Engelska\r\n"
  <> "S:Orust\r\n"
  <> "Z:John Watson 24/01/2015\r\n"
  <> "L:1/8\r\n"
  <> "K:A\r\n"
  <> "A>c|: e2f2 efed | c2a2 e3d | cedc BdcB | Aced cBAc |\r\n"
  <> "e2f2 efed | c2a2 e3d | cedc BdcB | A4 A>AA>B :|\r\n"
  <> "|: e2e2 e2de | f2ed B3c | d3c d2cd | e3d cdBc |\r\n"
  <> "A2a2 a2gf | e2f2 e3d | cedc BdcB |1 A4 A>AA>B :|2 [A4E4] [A4E4] |\r\n"

norefno :: String
norefno =
  "T: Fastan\r\n"
  <> "R: Polska\r\n"
  <> "M: 3/4\r\n"
  <> "K: F\r\n"
  <> "L: 1/16\r\n"
  <> "| (3A4F4G4 A2B2 | (3:4:3c2d2B4c4 A2F2 | (3F4E4D4 B,2D2 | EA3 A8- |\r\n"

notitle :: String
notitle =
  "X:1\r\n"
  <> "R: Polska\r\n"
  <> "M: 3/4\r\n"
  <> "K: F\r\n"
  <> "L: 1/16\r\n"
  <> "| (3A4F4G4 A2B2 | (3:4:3c2d2B4c4 A2F2 | (3F4E4D4 B,2D2 | EA3 A8- |\r\n"

badrhythm :: String
badrhythm =
  "X:1\r\n"
  <>  "T: Fastan\r\n"
  <> "R: Reel\r\n"
  <> "M: 3/4\r\n"
  <> "K: F\r\n"
  <> "L: 1/16\r\n"
  <> "| (3A4F4G4 A2B2 | (3:4:3c2d2B4c4 A2F2 | (3F4E4D4 B,2D2 | EA3 A8- |\r\n"
