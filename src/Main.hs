{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

-- import Language.UHIM.Dictionary.SKK.SKKExtended (SKKDict)
import qualified Language.UHIM.Dictionary.SKK.SKKExtended as SKK
import Language.UHIM.Dictionary.Transform.TUTYomi as TTY
import Language.UHIM.Dictionary.Transform.Variants as Var
import Language.UHIM.Dictionary.Transform.Tcvime as TV
import Language.UHIM.Dictionary.Publish.LaTeX as LaTeX
import Language.UHIM.Dictionary.Yaml

import qualified Data.ByteString as BS
-- import qualified Data.ByteString.Lazy as BL
import qualified Data.Yaml as Y
-- import qualified Data.Text as T
-- import qualified Data.Map as M
-- import Data.Map (Map)
-- import qualified Data.List as L
import Data.Either
import Data.Maybe
import Data.Monoid

import Control.Arrow
import Control.Monad

import Options.Applicative
import System.FilePath

import Paths_uhim_dict

data SKKYomiTarget = Tcvime
                   | Uim
  deriving (Show, Read, Eq, Ord)

data SKKYomiOptions = SKKYomiOptions { skkYomiOutputFile :: Maybe FilePath
                                     , skkYomiOutputTarget :: Maybe SKKYomiTarget
                                     , skkYomiOutputComments :: Bool
                                     , skkYomiDictionaries :: [FilePath]
                                     }
  deriving (Show, Read, Eq, Ord)
skkYomiOption :: Parser SKKYomiOptions
skkYomiOption = SKKYomiOptions
                <$> optional (strOption $ long "output"
                                        <> short 'o'
                                        <> metavar "FILE"
                                        <> help "Output file"
                                        )
                <*> optional (option auto $ long "target"
                                        <> short 't'
                                        <> metavar "TARGET"
                                        <> help "Output target, one of Tcvime and Uim"
                                        )
                <*> flag False True ( long "comments"
                                   <> short 'c'
                                   <> help "Output comments for each elements."
                                    )
                <*> many (argument str (metavar "FILE..."))

data LaTeXOptions = LaTeXOptions { latexOutputFile :: Maybe FilePath
                                 , latexTemplate :: Maybe FilePath
                                 , latexDictionaries :: [FilePath]
                                 }
  deriving (Show, Read, Eq, Ord)
latexOption :: Parser LaTeXOptions
latexOption = LaTeXOptions
                <$> optional (strOption $ long "output"
                                        <> short 'o'
                                        <> metavar "FILE"
                                        <> help "Output file"
                                        )
                <*> optional (strOption $ long "template"
                                        <> metavar "FILE"
                                        <> help "Template file"
                                        )
                <*> many (argument str (metavar "FILE..."))

data TcvimeOptions = TcvimeOptions { tcvimeOutputDirectory :: Maybe FilePath
                                   , tcvimeLayoutName :: String
                                   , tcvimeDictionaries :: [FilePath]
                                   }
  deriving (Show, Read, Eq, Ord)
tcvimeOption :: Parser TcvimeOptions
tcvimeOption = TcvimeOptions
                <$> optional (strOption $ long "outputdir"
                                        <> short 'd'
                                        <> metavar "DIR"
                                        <> help "Output directory"
                                        )
                <*> strOption ( long "name"
                             <> metavar "NAME"
                             <> help "Layout name")
                <*> many (argument str (metavar "FILE..."))

data Command = CommandSKKYomi SKKYomiOptions
             | CommandLaTeX LaTeXOptions
             | CommandTcvime TcvimeOptions
  deriving (Show, Read, Eq, Ord)

execCommand :: Command -> IO ()
execCommand (CommandSKKYomi opt) = do
  yds <- readDictionary $ skkYomiDictionaries opt
  let yamlDict = either error id yds
  let ttyConfig = case skkYomiOutputTarget opt of
        (Just Uim) -> TTY.uimDefaultConfig
        (Just Tcvime) -> TTY.tcvimeDefaultConfig
        Nothing -> TTY.uimDefaultConfig
  let yomiDict = TTY.extractSKK ttyConfig yamlDict
  let variantDict = Var.extractSKK Var.defaultConfig yamlDict
  let config =  SKK.defaultConfig { SKK.outputFrequency = skkYomiOutputComments opt }
  let outputStr = SKK.emitSKKDictionary config $ SKK.union yomiDict variantDict
  case skkYomiOutputFile opt of
    Nothing -> putStrLn outputStr
    Just fp -> writeFile fp outputStr
execCommand (CommandLaTeX opt) = do
  yds <- readDictionary $ latexDictionaries opt
  templFile <- case latexTemplate opt of
                 Nothing -> getDataFileName "template/publish-latex.tex"
                 Just x -> return x
  templ <- readFile templFile
  let yamlDict = either error id yds
  let conf = LaTeX.defaultConfig { LaTeX.template = templ }
  let outputStr = unlines $ LaTeX.emitDict conf yamlDict
  case latexOutputFile opt of
    Nothing -> putStrLn outputStr
    Just fp -> writeFile fp outputStr
execCommand (CommandTcvime opt) = do
  yds <- readDictionary $ tcvimeDictionaries opt
  let yamlDict = either error id yds
  let conf = TV.defaultConfig { TV.name = tcvimeLayoutName opt }
  let dir = fromMaybe "." $ tcvimeOutputDirectory opt
  let fss = map (first (dir </>)) $ TV.extract conf yamlDict
  forM_ fss (uncurry writeFile)

readDictionary :: [FilePath] -> IO (Either String Dictionary)
readDictionary [] = do
  d <- readFromBS "-" <$> BS.getContents
  return . (Y.prettyPrintParseException +++ id) $ d
readDictionary fps = do
  ds <- mapM readFromFile fps :: IO [Either Y.ParseException Dictionary]
  let (errs, dicts) = partitionEithers ds
  if null errs
    then return . Right $ mconcat dicts
    else return . Left . unlines $ map Y.prettyPrintParseException errs

opts :: Parser Command
opts = subparser
  (  command "skk-yomi" (info (helper <*> (CommandSKKYomi <$> skkYomiOption)) (progDesc "Generate SKK yomi dictionary"))
  <> command "latex" (info (helper <*> (CommandLaTeX <$> latexOption)) (progDesc "Generate LaTeX file"))
  <> command "tcvime" (info (helper <*> (CommandTcvime <$> tcvimeOption)) (progDesc "Generate Tcvime files")))

main :: IO ()
main = execParser progOpts >>= execCommand
  where
    progOpts = info (helper <*> opts)
      ( fullDesc
     <> progDesc "Manipulate UIM dictionary files"
     <> header "uhim-dict - UIM Hanzi Input Methods' Dictionary Manipulator" )
