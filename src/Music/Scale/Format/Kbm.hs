module Music.Scale.Format.Kbm(
    Kbm(..)
  , fromScale
  , toText
) where

import Data.Text

import Music.Scale.Format.Types

data Kbm = Kbm

fromScale :: Scale -> Kbm
fromScale = undefined

toText :: Kbm -> Text
toText = undefined
