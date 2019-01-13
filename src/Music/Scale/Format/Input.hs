module Music.Scale.Format.Input(
  parseInput
) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Aeson.TH
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Vector (Vector)
import Safe

import Music.Scale.Format.Types

import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Vector as V
import qualified Data.Yaml as Y

parseInput :: ByteString -> Either Text Scale
parseInput = fromInput <=< (either (Left . T.pack . show) Right . Y.decodeEither')

{-
name: "Major:
description: "Just intonation major"
baseFreq: 440  -- optional
baseNote: 69   -- optional
ratios: "x 9/8 x 5/4 x x 3/2 x 5/3 x x 2/1"
tonic: 60
midiMap: auto
-}
data Input = Input
  { name        :: !Text
  , baseFreq    :: !(Maybe Hz)
  , baseNote    :: !(Maybe Int)
  , ratios      :: !(Maybe Text)
  , tonic       :: !(Maybe Int)
  }
  deriving (Show, Eq)

defBaseFreq :: Hz
defBaseFreq = 440

defBaseNote :: Int
defBaseNote = 69

-- | Note C
defTonic :: Int
defTonic = 60

defRatios :: Text
defRatios = T.unwords $ fmap (T.pack . show . f) [1 .. 12]
  where
    f :: Int -> Double
    f n = 2 ** (fromIntegral n / 2)

fromInput :: Input -> Either Text Scale
fromInput Input{..} = do
  (degrees, octave) <- fromRatioSign =<< (parseRatios $ maybe defRatios id ratios)
  return $ Scale
      { scale'name     = name
      , scale'octave   = octave
      , scale'degrees  = degrees
      , scale'baseFreq = maybe defBaseFreq id baseFreq
      , scale'baseNote = maybe defBaseNote id baseNote
      , scale'tonic    = maybe defTonic    id tonic
      }

fromRatioSign :: [RatioSign] -> Either Text (Vector Ratio, Ratio)
fromRatioSign rs = maybe (Left "Failed to parse scale ratios") Right $ join $ liftA2 withOctave (noZeroLength rs) mOctave
  where
    mOctave = fromSign =<< lastMay rs

    withOctave len octave = fmap (\xs -> (toVals xs, octave)) $ initMay rs
      where
        toVals xs = V.fromList $ 1 : zipWith (fromSignWithET len octave) [(1 :: Int) ..] xs

    getEqualSubst len octave ind = octave ** (fromIntegral ind / fromIntegral len)

    fromSignWithET len octave ind x = maybe (getEqualSubst len octave ind) id $ fromSign x

    fromSign = \case
      Slash n m -> Just $ fromIntegral n  / fromIntegral m
      Skip -> Nothing
      Dot x -> Just x

    noZeroLength xs = case xs of
      [] -> Nothing
      _  -> Just $ length xs


data RatioSign = Slash Int Int | Skip | Dot Double
  deriving (Show, Eq)

parseRatios :: Text -> Either Text [RatioSign]
parseRatios = maybe (Left "Failed to parse scale ratios") Right . mapM parseSign . T.words
  where
    parseSign txt =
            getSkip txt
        <|> getSlash txt
        <|> getDot txt

    getSkip txt
      | txt == "x" || txt == "X" = Just Skip
      | otherwise                = Nothing

    getSlash txt = case T.splitOn "/" txt of
      [a, b] -> either (const Nothing) Just $ do
        an <- fmap fst $ T.decimal a
        bn <- fmap fst $ T.decimal b
        return $ Slash an bn
      _ -> Nothing

    getDot txt = either (const Nothing) (Just . Dot) $ fmap fst $ T.rational txt

---------------------------

$(deriveJSON defaultOptions 'Input)
