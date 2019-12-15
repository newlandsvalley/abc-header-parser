{-# LANGUAGE NamedFieldPuns #-}

module Data.Abc.Parser (abcParse, headersParse) where

import Control.Applicative ((<|>))
import Data.Text (Text, concat, pack, strip)
import Data.Either (Either(..))
import Data.Attoparsec.Text
import Data.Abc
import Data.Char (isPrint)

-- | A simple parser for headers in the ABC notation
-- | providing just enough to identify each header and support
-- | string searches against their contents

-- | parse ABC tune text to the end of the input
abcParse :: Text -> Either String Abc
abcParse text =
  -- trace ("ABC parse: "  <> show text) $
    parseOnly abcTune text

-- | just parse the headers from ABC tune text
headersParse :: Text -> Either String Headers
headersParse text =
  -- trace ("headers parse: "  <> show text) $
    parseOnly tuneHeaders text

abcTune :: Parser Abc
abcTune =
  buildTune <$>
    tuneHeaders <*> tuneBody
    <?> "abcTune"

tuneHeaders :: Parser Headers
tuneHeaders =
  many1 header
  <?> "headers"

{-}
tracedHeader :: Parser (HeaderName, Text)
tracedHeader = do
  theHeader <- header
  return .
    trace ("header "  ++ show theHeader) $ theHeader
-}

header :: Parser (HeaderName, Text)
header =
  (,) <$>
    headerName <*> someHeaderText <* endOfLine

someHeaderText :: Parser Text
someHeaderText =
  -- takeTill isEndOfLine
  strip <$> pack <$>
    many1 (satisfy isPrint)

tuneBody :: Parser Text
tuneBody =
  Data.Text.concat <$>
    manyTill bodyLine endOfInput

bodyLine :: Parser Text
bodyLine =
  pack <$>  (\s -> s <> "\r\n") <$>
    many' (satisfy isPrint) <* endOfLine

headerName :: Parser HeaderName
headerName =
  choice
    [ area
    , book
    , composer
    , discography
    , fileUrl
    , group
    , history
    , instruction
    , key
    , unitNoteLength
    , meter
    , macro
    , notes
    , origin
    , parts
    , tempo
    , rhythm
    , remark
    , source
    , symbolLine
    , title
    , userDefined
    , voice
    , wordsAfter
    , wordsAligned
    , referenceNumber
    , transcription
    , fieldContinuation
    , comment
    , unsupportedHeader
    ]

area :: Parser HeaderName
area =
  Area
    <$ (headerCode 'A')
    <?> "area"

book :: Parser HeaderName
book =
  Book
    <$ (headerCode 'B')
    <?> "book"

composer :: Parser HeaderName
composer =
  Composer
    <$ (headerCode 'C')
    <?> "composer"

discography :: Parser HeaderName
discography =
  Discography
    <$ (headerCode 'D')
    <?> "discography"

fileUrl :: Parser HeaderName
fileUrl =
  FileUrl
    <$ (headerCode 'F')
    <?> "fileUrl"

group :: Parser HeaderName
group =
  Group
    <$ (headerCode 'G')
    <?> "group"

history :: Parser HeaderName
history =
  History
    <$ (headerCode 'H')
    <?> "history"

instruction :: Parser HeaderName
instruction =
  Instruction
    <$ (headerCode 'I')
    <?> "instruction"

key :: Parser HeaderName
key =
  Key
    <$ (headerCode 'K')
    <?> "key"

unitNoteLength :: Parser HeaderName
unitNoteLength =
  UnitNoteLength
    <$ (headerCode 'L')
    <?> "unitNoteLength"

meter :: Parser HeaderName
meter =
  Meter
    <$ (headerCode 'M')
    <?> "meter"

macro :: Parser HeaderName
macro =
  Macro
    <$ (headerCode 'm')
    <?> "macro"

notes :: Parser HeaderName
notes =
  Notes
    <$ (headerCode 'N')
    <?> "notes"

origin :: Parser HeaderName
origin =
  Origin
    <$ (headerCode 'O')
    <?> "origin"

parts :: Parser HeaderName
parts =
  Parts
    <$ (headerCode 'P')
    <?> "parts"

tempo :: Parser HeaderName
tempo =
  Tempo
    <$ (headerCode 'Q')
    <?> "tempo"

rhythm :: Parser HeaderName
rhythm =
  Rhythm
    <$ (headerCode 'R')
    <?> "rhythm"

remark :: Parser HeaderName
remark =
  Remark
    <$ (headerCode 'r')
    <?> "remark"

source :: Parser HeaderName
source =
  Source
    <$ (headerCode 'S')
    <?> "source"

symbolLine :: Parser HeaderName
symbolLine =
  SymbolLine
    <$ (headerCode 's')
    <?> "symbolLine"

title :: Parser HeaderName
title =
  Title
    <$ (headerCode 'T')
    <?> "title"

userDefined :: Parser HeaderName
userDefined =
  UserDefined
    <$ (headerCode 'U')
    <?> "userDefined"

voice :: Parser HeaderName
voice =
  Voice
    <$ (headerCode 'V')
    <?> "voice"

wordsAfter :: Parser HeaderName
wordsAfter =
  WordsAfter
    <$ (headerCode 'W')
    <?> "wordsAfter"

wordsAligned :: Parser HeaderName
wordsAligned =
  WordsAligned
    <$ (headerCode 'w')
    <?> "wordsAligned"

referenceNumber :: Parser HeaderName
referenceNumber =
  ReferenceNumber
    <$ (headerCode 'X')
    <?> "referenceNumber"

transcription :: Parser HeaderName
transcription =
  Transcription
    <$ (headerCode 'Z')
    <?> "transcription"

fieldContinuation :: Parser HeaderName
fieldContinuation =
  FieldContinuation
    <$ (headerCode '+')
    <?> "fieldContinuation"

comment :: Parser HeaderName
comment =
  Comment
    <$ (headerCode '%')
    <?> "comment"

unsupportedHeader :: Parser HeaderName
unsupportedHeader =
  UnsupportedHeader
    <$> unsupportedHeaderCode
    <?> "unsupportedHeader"


headerCode :: Char -> Parser Char
headerCode c =
  char c <* char ':' <* whiteSpace

unsupportedHeaderCode :: Parser Char
unsupportedHeaderCode =
  let
    isUnsupported :: Char -> Bool
    isUnsupported c =
        (c >= 'a' && c <= 'q')
     || (c >= 't' && c <= 'v')
     || (c >= 'x' && c <= 'z')
     || (c == 'J')
     || (c == 'E')
  in
    satisfy isUnsupported <* char ':' <* whiteSpace

whiteSpace :: Parser String
whiteSpace =
  many' scoreSpace

-- normal space within a line of the tune's score
scoreSpace :: Parser Char
scoreSpace =
  -- tab <|> space
  (char '\t') <|> (char ' ')

buildTune :: [(HeaderName, Text)] -> Text -> Abc
buildTune headers body =
  Abc { headers, body }
