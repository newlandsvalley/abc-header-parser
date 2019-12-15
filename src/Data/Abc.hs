module Data.Abc where

import Data.Map
import Data.Text (Text)

type HeaderText = Text
type Body = Text

type HeaderMap = Map HeaderName HeaderText
type Headers = [ (HeaderName, HeaderText) ]

data Abc = Abc { headers :: Headers
               , body :: Text
               } deriving (Eq, Show)

-- | An ABC Tune Header name.
data HeaderName
  = Area
  | Book
  | Composer
  | Discography
  | FileUrl
  | Group
  | History
  | Instruction
  | Key
  | UnitNoteLength
  | Meter
  | Macro
  | Notes
  | Origin
  | Parts
  | Tempo
  | Rhythm
  | Remark
  | Source
  | SymbolLine
  | Title
  | UserDefined
  | Voice
  -- voice properties
  | WordsAfter
  -- words after notes
  | WordsAligned
  -- words aligned with notes
  | ReferenceNumber   -- spec says number is optionally allowed to be blank but not recommended
  | Transcription
  | FieldContinuation
  | Comment
  | UnsupportedHeader Char
    deriving (Eq, Ord, Show)

-- | A Mode.
data Mode
  = Major
  | Minor
  | Ionian
  | Dorian
  | Phrygian
  | Lydian
  | Mixolydian
  | Aeolian
  | Locrian
    deriving (Eq, Ord, Show)
