module Music.Scale.Format.Anamark(
    Tun(..)
  , Info(..)
  , fromScale
  , toText
) where

import Data.Text (Text)
import Data.Time

import Music.Scale.Format.Types

import qualified Data.Text as T

data Tun = Tun
  { tun'info :: Info
  , tun'tuning :: Tuning
  , tun'exactTuning :: ExactTuning
  }

type Tuning = [Int]

data ExactTuning = ExactTuning
  { exactTuning'baseFreq :: !(Maybe Hz)
  , exactTuning'notes    :: ![Cent]
  }

data Info = Info
  { info'name           :: Text
  , info'id             :: Text
  , info'filename       :: Text
  , info'description    :: Text
  , info'date           :: UTCTime
  , info'editor         :: Text
  }

fromScale :: UTCTime -> Scale -> Tun
fromScale date sc = Tun
  { tun'info = getInfo date sc
  , tun'tuning = getTuning sc
  , tun'exactTuning = getExactTuning sc
  }

getInfo :: UTCTime -> Scale -> Info
getInfo date Scale{..} = Info
    { info'name           = name
    , info'id             = name
    , info'filename       = mappend name ".tun"
    , info'description    = name
    , info'date           = date
    , info'editor         = "hscale"
    }
  where
    name = scale'name

toText :: Tun -> Text
toText = renderLang . toLang

-------------------------------------------
-- note conversion

defBaseFreq :: Hz
defBaseFreq = 8.1757989156437073336

getTuning :: Scale -> Tuning
getTuning = fmap round . fromAbsFreq defBaseFreq . getAbsMidiFreqs

getExactTuning :: Scale -> ExactTuning
getExactTuning sc = ExactTuning
  { exactTuning'baseFreq = Nothing
  , exactTuning'notes = fromAbsFreq defBaseFreq $ getAbsMidiFreqs sc
  }

fromAbsFreq :: Hz -> [Hz] -> [Cent]
fromAbsFreq baseFreq notes = fmap (toCent baseFreq) notes

-------------------------------------------
-- simplified tune file syntax

toLang :: Tun -> Lang
toLang tun = mconcat
  [ tuningSection
  , annaMarkSection
  ]
  where
    tuningSection = mconcat
      [ Section "Tuning" : tuningNotes (tun'tuning tun)
      , pure EmptyLine]

    annaMarkSection = concat
      [ scaleBegin
      , infoSection $ tun'info tun
      , exactTuning $ tun'exactTuning tun
      , pure scaleEnd ]

    scaleBegin =
      [ Section "Scale Begin"
      , Key "Format" $ TextVal "AnaMark-TUN"
      , Key "FormatVersion" $ IntVal 200
      , Key "FormatSpecs" $ TextVal "http://www.mark-henning.de/eternity/tuningspecs.html"
      , EmptyLine
      ]

    infoSection Info{..} =
      [ Section "Info"
      , Key "Name" $ TextVal info'name
      , Key "ID"   $ TextVal info'id
      , Key "Filename" $ TextVal info'filename
      , Key "Description" $ TextVal info'description
      , Key "Date" $ TextVal $ dateToText info'date
      , Key "Editor" $ TextVal info'editor
      , EmptyLine
      ]

    exactTuning ExactTuning{..} = mconcat
      [ pure $ Section "Exact Tuning"
      , maybe [] (pure . Key "BaseFreq" . DoubleVal) exactTuning'baseFreq
      , exactTuningNotes exactTuning'notes
      , pure EmptyLine
      ]

    scaleEnd = Section "Scale End"

tuningNotes :: [Int] -> [Prim]
tuningNotes vals =
  zipWith (\n val -> Key (note n) (IntVal val)) [0..] vals

exactTuningNotes :: [Cent] -> [Prim]
exactTuningNotes vals =
  zipWith (\n val -> Key (note n) (DoubleVal val)) [0..] vals

note :: Int -> Text
note n = mconcat ["note ", T.pack $ show n]

dateToText :: UTCTime -> Text
dateToText = T.pack . formatTime defaultTimeLocale "%F"

type Lang = [Prim]

data Prim
  = Comment Text
  | Section Text
  | Key Text Val
  | EmptyLine

data Val = TextVal Text | IntVal Int | DoubleVal Double

renderLang :: Lang -> Text
renderLang = T.unlines . fmap renderPrim

renderPrim :: Prim -> Text
renderPrim = \case
  Comment txt -> mappend "; " txt
  Section txt -> mconcat ["[", txt, "]"]
  Key name value -> mconcat [name, "=", renderVal value]
  EmptyLine -> "\n"

renderVal :: Val -> Text
renderVal = \case
  TextVal txt -> mconcat ["\"", txt, "\""]
  IntVal n -> T.pack $ show n
  DoubleVal d -> T.pack $ show d
