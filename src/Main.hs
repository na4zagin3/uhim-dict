{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Dictionary.SKK.SKKExtended (SKKDict)
import qualified Dictionary.SKK.SKKExtended as SKK
import Dictionary.Transform.TUTYomi as TTY

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Yaml as Y
import qualified Data.Text as T
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.List as L
import Data.Monoid
import Text.Parsec
import GHC.Generics
-- import Control.Lens
import Data.Maybe
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types

import Dictionary.Yaml


main :: IO ()
main = do
  str <- Y.decodeEither' <$> BS.getContents
  case str of
    Left e -> print e
    Right y -> putStrLn . SKK.emitSKKDictionary $ TTY.extractSKK TTY.defaultConfig y
