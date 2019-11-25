module Data.GenreParser (genreParse) where

import Data.Text (toLower, pack)
import Data.Either (Either)
import Data.Attoparsec.Text
import Data.Genre
import Data.Bifunctor (first)

-- | A trivial parser for Genres

-- | parse a genre
genreParse :: String -> Either String Genre
genreParse s =
  first (const $ "unknown genre " <> s) $
    parseOnly genre (toLower $ pack s)

genre :: Parser Genre
genre =
  choice
   [ irish
   , klezmer
   , scandi
   , scottish
   ] <* endOfInput

irish :: Parser Genre
irish =
  Irish <$ string (pack "irish")

klezmer :: Parser Genre
klezmer =
  Klezmer <$ string (pack "klezmer")

scandi :: Parser Genre
scandi =
  Scandi <$ string (pack "scandi")

scottish :: Parser Genre
scottish =
  Scottish <$ string (pack "scottish")
