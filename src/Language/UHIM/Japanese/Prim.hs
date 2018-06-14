{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.UHIM.Japanese.Prim where

import Data.Aeson.Types
import Data.Monoid (mempty)
import Data.Semigroup as Sem
import qualified Data.Text as T

import GHC.Generics

type ShinKana = String
type KyuKana = String

type Kana = String
type Kanji = String

data JaYomi = NonChange String
            | Changed ShinKana KyuKana -- 新仮名遣 旧仮名遣
    deriving (Show, Read, Eq, Ord, Generic)

instance Sem.Semigroup JaYomi where
  NonChange a <> NonChange b = NonChange (a <> b)
  NonChange a <> Changed b1 b2 = Changed (a <> b1) (a <> b2)
  Changed a1 a2 <> NonChange b = Changed (a1 <> b) (a2 <> b)
  Changed a1 a2 <> Changed b1 b2 = Changed (a1 <> b1) (a2 <> b2)

instance Monoid JaYomi where
  mempty = NonChange ""

#if !(MIN_VERSION_base(4,11,0))
  mappend = (<>)
#endif

instance ToJSON JaYomi where
    toJSON (NonChange str) = String $ T.pack str
    toJSON (Changed shin kyu) = object ["新" .= shin, "舊" .= kyu]
    toEncoding (NonChange str) = toEncoding str
    toEncoding (Changed shin kyu) = pairs ("新" .= shin <> "舊" .= kyu)

instance FromJSON JaYomi where
    parseJSON (Object v) = Changed <$>
                           v .: "新" <*>
                           v .: "舊"
    parseJSON (String v) = pure $ NonChange . T.unpack $ v
    parseJSON v          = typeMismatch "JaYomi" v

data JaLanguageVariant = ModernStandardJapanese | MiddleJapanese
    deriving (Eq, Ord, Show, Read)
