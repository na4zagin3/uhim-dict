{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Dictionary.Yaml.Japanese.Prim where

import qualified Data.Text as T
import Data.Monoid
import Data.Aeson.Types
import GHC.Generics

type ShinKana = String
type KyuKana = String

type Kana = String
type Kanji = String

data JaYomi = NonChange String
            | Changed ShinKana KyuKana -- 新仮名遣 旧仮名遣
    deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON JaYomi where
    toEncoding (NonChange str) = toEncoding str
    toEncoding (Changed shin kyu) = pairs ("新" .= shin <> "舊" .= kyu)

instance FromJSON JaYomi where
    parseJSON (Object v) = Changed <$>
                           v .: "新" <*>
                           v .: "舊"
    parseJSON (String v) = pure $ NonChange . T.unpack $ v
    parseJSON v          = typeMismatch "JaYomi" v
