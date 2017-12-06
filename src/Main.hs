{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Prelude             ()
import           Protolude

import           Control.Arrow
import           Control.Monad
import qualified Data.ByteString     as B
import           Data.List
import           Data.Semigroup      ((<>))
import           Lens.Micro.Platform
import           Options.Applicative
import           System.Directory

data Options = Options
  { message :: Text
  }

opts :: Parser Options
opts =
  Options <$>
  option
    auto
    (long "message" <> short 'm' <> metavar "MESSAGE" <>
     help "The message to print" <>
     showDefault <>
     value "Hello, World!")

optsInfo :: ParserInfo Options
optsInfo =
  info
    (opts <**> helper)
    (fullDesc <> progDesc "Print a simple message" <>
     header "slp-data-recovery - a minimal application")

specificFile = "data/070717-C10-F-76-PWS/070717-c10-F-76-PWS-7.wav"

specificFolder = "data/070717-C10-F-76-PWS"

extractData :: ByteString -> ByteString -> ByteString
extractData date b = snd . Data.List.head $ filter ((==) b . fst) $ fmap (\sub -> B.breakSubstring sub b) subs
  where subs =
          [ date
          , "piper"
          , "seashells"
          , "woodchuck"
          , "tutor"
          , "oyster"
          , "perkins"
          , "moses"
          , "blackbear"
          , "chester"
          , "betty"
          , "1"
          , "2"
          , "3"
          , "4"
          , "5"
          , "6"
          , "7"
          , "8"
          , "9"
          , "10"
          ]

getWavFiles :: FilePath -> IO [[Char]]
getWavFiles = fmap (filter (isSuffixOf ".wav")) . listDirectory

isTimed :: FilePath -> Bool
isTimed =
  or . (<*>) (fmap isSuffixOf ["0.9.wav", "0.85.wav", "0.95.wav"]) . pure

isBase :: FilePath -> Bool
isBase =
  and .
  (<*>) [isSuffixOf ".wav", not . isTimed, not . isSuffixOf "rainbow-text.wav"] .
  pure

getDateFromFolder :: ByteString -> ByteString
getDateFromFolder = B.drop 5 . fst . B.breakSubstring "-"

isCSV :: FilePath -> Bool
isCSV = isSuffixOf ".csv"

isTxt :: FilePath -> Bool
isTxt = isSuffixOf ".txt"

directoryHasData :: FilePath -> IO Bool
directoryHasData p = or . (<*>) [isCSV, isTxt] <$> getDirectoryContents p

feature :: (a -> b) -> a -> (a, b)
feature f a = (a, f a)

featureM :: Monad m => (a -> m b) -> a -> m (a, b)
featureM f = sequence . feature f

data ID = ID ByteString deriving (Show)

data NoData =
  NoData ID FilePath
  deriving (Show)

foldersWithoutData :: IO [NoData]
foldersWithoutData = do
  dirs <- listDirectory "data"
  noData <- filterM (fmap not . directoryHasData) . fmap ("data/" <>) $ dirs
  pure $ zipWith ($) (NoData . ID . (toS . drop 5) <$> noData) noData

data TimedSample = TimedSample ByteString
data BaseSample = BaseSample ByteString

recoverData :: NoData -> IO ()
recoverData (NoData (ID i) folder) = do
  files <- listDirectory folder
  let date = getDateFromFolder (toS folder)
  print date
  print folder
  let recover sample = do
        contents <- B.readFile (folder <> "/" <> sample)
        pure (extractData date contents)
  timed <- mapM recover (filter isTimed files)
  print timed
  B.writeFile (toS folder <> "/" <> toS i <> "-timed.csv") (B.concat timed)
  base <- mapM recover (filter isBase files)
  print base
  B.writeFile (toS folder <> "/" <> toS i <> "-base.csv") (B.concat base)

main :: IO ()
main = do
  files <- getWavFiles specificFolder
  csv <-
    forM files $ \file -> do
      contents <- B.readFile (specificFolder <> "/" <> file)
      pure (extractData "070717" contents)
  B.writeFile "time-data.csv" (B.concat csv)
