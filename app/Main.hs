module Main where

import Data.Time
import Options.Applicative

import Music.Scale.Format.Types
import Music.Scale.Format.Input

import qualified Music.Scale.Format.Anamark as T
import qualified Data.ByteString as BS
import qualified Data.Text.IO as T


main :: IO ()
main = runApp =<< execParser opts
  where
    opts = info parseArgs
      ( fullDesc
         <> progDesc "Creates various tuning files"
         <> header "hscale - creates tuning files" )

runApp :: Arg -> IO ()
runApp arg@Arg{..} = do
  eInp <- fmap parseInput $ BS.readFile arg'input
  case eInp of
    Right inp -> procScale arg inp
    Left err -> T.putStrLn $ mappend "Error: " err

procScale :: Arg -> Scale -> IO ()
procScale Arg{..} scale = do
  now <- getCurrentTime
  let tun = T.fromScale now scale
  T.writeFile arg'output $ T.toText tun

-- data Format = Tun | Kbm | Scl | Kontakt
--   deriving (Show, Eq)

data Arg = Arg
  { arg'output    :: !FilePath
  , arg'input     :: !FilePath
  }

---------------------
-- simple format

{-
name: "Major:
description: "Just intonation major"
baseFreq: 440  -- optional
baseNote: 69   -- optional
ratios: "x 9/8 x 5/4 x x 3/2 x 5/3 x x 2/1"
tonic: 60
midiMap: auto
-}


---------------------
-- parse args

parseArgs :: Parser Arg
parseArgs = Arg
  <$> strOption
          ( long "output"
         <> short 'o'
         <> metavar "TARGET"
         <> help "Output file" )
  <*> strOption
          ( long "input"
         <> short 'i'
         <> metavar "TARGET"
         <> help "Input file for tunning" )
