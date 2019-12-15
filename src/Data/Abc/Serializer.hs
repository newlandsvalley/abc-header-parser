
{-# LANGUAGE OverloadedStrings #-}

module Data.Abc.Serializer (serializeHeaders) where

import Prelude hiding (concat)
import Data.Text (Text, concat, singleton)
import Data.Char (toUpper)
import Data.Abc

serializeHeaders :: Headers -> Text
serializeHeaders hs =
  concat $ map header hs

header :: (HeaderName, HeaderText) -> Text
header (hn, ht) =
  (headerName hn) <> ":" <> ht <> "\r\n"

headerName :: HeaderName -> Text
headerName hn =
  case hn of
    UnitNoteLength ->
      "L"
    Macro ->
      "m"
    Tempo ->
      "Q"
    Remark ->
      "r"
    SymbolLine ->
      "s"
    WordsAligned ->
      "w"
    ReferenceNumber ->
      "X"
    Transcription ->
      "Z"
    FieldContinuation ->
      "+"
    Comment ->
      "%"
    UnsupportedHeader c ->
      singleton c
    other ->
      (singleton . toUpper . head) $ show other
