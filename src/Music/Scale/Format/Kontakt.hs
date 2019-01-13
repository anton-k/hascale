module Music.Scale.Format.Kontakt(
    Kontakt(..)
  , fromScale
  , toText
) where

import Data.Text

import Music.Scale.Format.Types

data Kontakt = Kontakt

fromScale :: Scale -> Kontakt
fromScale = undefined

toText :: Kontakt -> Text
toText = undefined
