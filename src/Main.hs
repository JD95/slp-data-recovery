{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude ()
import Protolude

import Control.Monad
import Data.List
import Data.Semigroup ((<>))
import Options.Applicative
import Lens.Micro.Platform
import qualified Data.ByteString as B
import System.Directory

data Options = Options { message :: Text }

opts :: Parser Options
opts = Options <$> option auto
     ( long "message"
    <> short 'm'
    <> metavar "MESSAGE"
    <> help "The message to print"
    <> showDefault
    <> value "Hello, World!"
      )

optsInfo :: ParserInfo Options
optsInfo = info (opts <**> helper)
  ( fullDesc
  <> progDesc "Print a simple message"
  <> header "slp-data-recovery - a minimal application"
  )

specificFile = "data/070717-C10-F-76-PWS/070717-c10-F-76-PWS-7.wav"
specificFolder = "data/070717-C10-F-76-PWS"

extractData :: ByteString -> ByteString -> ByteString
extractData date = snd . B.breakSubstring date 

getWavFiles :: FilePath -> IO [[Char]]
getWavFiles = fmap (filter (isSuffixOf ".wav")) . listDirectory


main :: IO ()
main = do
  files <- getWavFiles specificFolder
  csv <- forM files $ \file -> do
    contents <- B.readFile (specificFolder <> "/" <> file) 
    pure (extractData "070717" contents)
  B.writeFile "time-data.csv" (B.concat csv)


