module Data.Genre where

-- | A Tune genre
data Genre
  = Irish
  | Klezmer
  | Scandi
  | Scottish
    deriving (Eq, Ord, Show, Enum)

data ScandiRhythm
  = Brudmarsch
  | Engelska
  | Långdans
  | Marsch
  | Polka
  | Polska
  | Schottis
  | Sekstur
  | Skänklåt
  | Slängpolska
  | Waltz
    deriving (Eq, Ord, Show, Enum)

data KlezmerRhythm
  = Bulgar
  | Csardas
  | Doina
  | Freylekhs
  | Khosidl
  | Hora
  | Honga
  | Hopak
  | Kasatchok
  | Kolomeyke
  | Sher
  | Sirba
  | Skotshne
  | Taksim
  | Terkish
    deriving (Eq, Ord, Show, Enum)

data CelticRhythm
  = Barndance
  | Hornpipe
  | Jig
  | March
  | CelticPolka
  | Reel
  | SlipJig
  | CelticWaltz
    deriving (Eq, Ord, Enum)

instance Show CelticRhythm where
  show Barndance = "Barndance"
  show Hornpipe = "Hornpipe"
  show Jig = "Jig"
  show March = "March"
  show CelticPolka = "Polka"
  show Reel = "Reel"
  show SlipJig = "Slip Jig"
  show CelticWaltz = "Waltz"

data IrishRhythm
  = Highland
  | Mazurka
  | Slide
    deriving (Eq, Ord, Show, Enum)

data ScottishRhythm
   = Schottische
   | Strathspey
     deriving (Eq, Ord, Show, Enum)
