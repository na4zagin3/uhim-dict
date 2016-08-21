{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Language.UHIM.Dictionary.SKK.SKKExtended (SKKDict)
import qualified Language.UHIM.Dictionary.SKK.SKKExtended as SKK
import Language.UHIM.Dictionary.Transform.TUTYomi as TTY
import Language.UHIM.Dictionary.Transform.Variants as Var
import Language.UHIM.Dictionary.Yaml

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



main :: IO ()
main = do
  str <- either (error . show) id . Y.decodeEither' <$> BS.getContents
  let yomiDict = TTY.extractSKK TTY.defaultConfig str
  let variantDict = Var.extractSKK Var.defaultConfig str
  putStrLn . SKK.emitSKKDictionary $ SKK.union yomiDict variantDict
