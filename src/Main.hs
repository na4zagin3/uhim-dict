{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

-- import Language.UHIM.Dictionary.SKK.SKKExtended (SKKDict)
import qualified Language.UHIM.Dictionary.SKK.SKKExtended as SKK
import Language.UHIM.Dictionary.Transform.TUTYomi as TTY
import Language.UHIM.Dictionary.Transform.Variants as Var
import Language.UHIM.Dictionary.Publish.LaTeX as LaTeX
import Language.UHIM.Dictionary.Yaml

import qualified Data.ByteString as BS
-- import qualified Data.ByteString.Lazy as BL
-- import qualified Data.Yaml as Y
-- import qualified Data.Text as T
-- import qualified Data.Map as M
-- import Data.Map (Map)
-- import qualified Data.List as L
import Data.Monoid
import Data.String
import Options.Applicative

import Paths_uhim_dict

data SKKYomiOptions = SKKYomiOptions { skkYomiOutputFile :: Maybe FilePath
                                     , skkYomiDictionary :: Maybe FilePath
                                     }
  deriving (Show, Read, Eq, Ord)
skkYomiOption :: Parser SKKYomiOptions
skkYomiOption = SKKYomiOptions
                <$> optional (strOption $ long "output"
                                        <> short 'o'
                                        <> metavar "FILE"
                                        <> help "Output file"
                                        )
                <*> optional (argument str (metavar "FILE"))

data LaTeXOptions = LaTeXOptions { latexOutputFile :: Maybe FilePath
                                 , latexTemplate :: Maybe FilePath
                                 , latexDictionary :: Maybe FilePath
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
                <*> optional (argument str (metavar "FILE"))

data Command = SKKYomi SKKYomiOptions
             | LaTeX LaTeXOptions
  deriving (Show, Read, Eq, Ord)

execCommand :: Command -> IO ()
execCommand (SKKYomi opt) = do
  yds <- case skkYomiDictionary opt of
           Nothing -> readFromBS "-" <$> BS.getContents
           Just fp -> readFromFile fp
  let yamlDict = either (error . show) id yds
  let yomiDict = TTY.extractSKK TTY.defaultConfig yamlDict
  let variantDict = Var.extractSKK Var.defaultConfig yamlDict
  let outputStr = SKK.emitSKKDictionary $ SKK.union yomiDict variantDict
  case skkYomiOutputFile opt of
    Nothing -> putStrLn outputStr
    Just fp -> writeFile fp outputStr
execCommand (LaTeX opt) = do
  yds <- case latexDictionary opt of
           Nothing -> readFromBS "-" <$> BS.getContents
           Just fp -> readFromFile fp
  templFile <- case latexTemplate opt of
                 Nothing -> getDataFileName "template/publish-latex.tex"
                 Just x -> return x
  templ <- readFile templFile
  let conf = LaTeX.Config { LaTeX.template = templ }
  let yamlDict = either (error . show) id yds
  let outputStr = unlines $ LaTeX.emitDict conf yamlDict
  case latexOutputFile opt of
    Nothing -> putStrLn outputStr
    Just fp -> writeFile fp outputStr

opts :: Parser Command
opts = subparser
  (  command "skk-yomi" (info (helper <*> (SKKYomi <$> skkYomiOption)) (progDesc "Generate SKK yomi dictionary"))
  <> command "latex" (info (helper <*> (LaTeX <$> latexOption)) (progDesc "Generate LaTeX file")))

main :: IO ()
main = execParser progOpts >>= execCommand
  where
    progOpts = info (helper <*> opts)
      ( fullDesc
     <> progDesc "Manipulate UIM dictionary files"
     <> header "uhim-dict - UIM Hanzi Input Methods' Dictionary Manipulator" )
