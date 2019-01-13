module Music.Scale.Format.Types where

-- scala file description
-- http://www.huygens-fokker.org/scala/scl_format.html

-- kbm description:
-- http://www.huygens-fokker.org/scala/help.htm#mappings

-- tun
-- https://www.mark-henning.de/files/am/Tuning_File_V2_Doc.pdf
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Data.Vector as V

type Ratio = Double
type Hz = Double
type Cent = Double

data Scale = Scale
  { scale'name     :: !Text
  , scale'octave   :: !Ratio
  , scale'degrees  :: !(Vector Ratio)
  , scale'baseFreq :: !Hz
  , scale'baseNote :: !Int
  , scale'tonic    :: !Int
  }
  deriving (Show, Eq)

getAbsMidiFreqs :: Scale -> [Hz]
getAbsMidiFreqs sc = fmap (toFreq sc) [0 .. 127]

toCent :: Hz -> Hz -> Cent
toCent baseFreq x = 1200 * logBase 2 (x / baseFreq)

{-
    var offset = i - base_midi_note;
    var quotient = Math.floor( offset / (tuning.length-1) );
    var remainder = offset % (tuning.length-1);
    if ( remainder < 0 ) remainder += tuning.length-1;
    var period = tuning[ tuning.length-1 ];
    // "decimal" here means a frequency ratio, but stored in decimal format
    var decimal = tuning[ remainder ] * Math.pow( period, quotient );

    // store the data in the tuning_table object
    tuning_table['freq'][i] = base_frequency * decimal;
-}

toFreq :: Scale -> Int -> Hz
toFreq scale@Scale{..} n =
  baseFreq * (scale'degrees V.! remainder) * (scale'octave ^^ quotient)
  where
    len       = V.length scale'degrees
    offset    = n - baseNote
    quotient  = (floor ((fromIntegral offset :: Double) / fromIntegral len)) :: Int
    remainder = offset `mod` len

    (baseFreq, baseNote) = getTonicBase scale

getTonicBase :: Scale -> (Hz, Int)
getTonicBase Scale{..} = (scale'baseFreq / ratio, scale'baseNote - note)
  where
    len       = V.length scale'degrees
    ratio     = scale'degrees V.! note
    note      = mod (scale'baseNote - scale'tonic) len



