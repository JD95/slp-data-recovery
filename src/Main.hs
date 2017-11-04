{-# LANGUAGE OverloadedStrings #-}
module Main where

import Prelude ()
import Protolude

import Data.Semigroup ((<>))
import Options.Applicative
import Lens.Micro.Platform
import qualified Data.ByteString as B

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

extractData date = snd . B.breakSubstring date 

main :: IO ()
main = do
  options <- execParser optsInfo
  putStrLn (message options)

