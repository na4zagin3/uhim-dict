{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Dictionary.Yaml where

import qualified Data.Text as T
import qualified Data.Map as M
import Data.Map (Map)
-- import qualified Data.List as L
-- import qualified Data.Yaml as Y
-- import Data.Text (Text)
-- import Data.Vector (Vector)
-- import Data.HashMap
-- import Data.String
-- import Data.Char
-- import qualified Data.Aeson as J
import qualified Data.Yaml.Include as Y
-- import qualified Data.ByteString as BS
-- import qualified Data.ByteString.UTF8 as BSU
-- import qualified Data.ByteString.Lazy as BL
-- import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.Aeson.TH
import Data.Aeson.Types
import Data.Monoid
-- import Text.Parsec
import GHC.Generics

import Dictionary.Yaml.Japanese.Verb

type ShinKana = String
type KyuKana = String

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

data Pron = Pron { pron日漢 :: Maybe JaYomi
                 , pron日呉 :: Maybe JaYomi
                 , pron日訓 :: Maybe JaYomi
                 }
    deriving (Eq, Ord, Show, Read)
deriveJSON defaultOptions{fieldLabelModifier = drop 4} ''Pron

data KanjiShapes = KanjiShapes (Map String String)
    deriving (Eq, Ord, Show, Read, Generic)

instance ToJSON KanjiShapes where
    toEncoding (KanjiShapes vks) = pairs . mconcat . map (\(k, v)-> T.pack k .= v) . M.toList $ vks

instance FromJSON KanjiShapes where
    parseJSON v@(Object _) = do
      xs <- parseJSON v
      pure $ KanjiShapes xs
    parseJSON (String v) = pure . KanjiShapes . M.fromList $ [(commonKanjiKey, T.unpack v)]
    parseJSON v          = typeMismatch "JaYomi" v

commonKanjiKey, kyuKanjiKey, shinKanjiKey, jaKanjiKey :: String
commonKanjiKey = "共通"
kyuKanjiKey = "日舊"
shinKanjiKey = "日新"
jaKanjiKey = "日"

{-

deriveJSON defaultOptions ''KanjiShapes
-}

data KanjiDeclaration = KanjiDeclaration { kanji體 :: KanjiShapes
                                         , kanji音 :: [Pron]
                                         , kanji形 :: [String]
                                         , kanji義 :: Maybe [String]
                                         , kanji鍵 :: Maybe (Map String String)
                                         , kanji頻度 :: Maybe Double
                                         }
    deriving (Eq, Ord, Show, Read)
deriveJSON defaultOptions{fieldLabelModifier = drop 5} ''KanjiDeclaration

data WordConvPair = WordConvPair { word字 :: KanjiShapes
                                 , word讀 :: Pron
                                 }
    deriving (Eq, Ord, Show, Read)
deriveJSON defaultOptions{fieldLabelModifier = drop 4} ''WordConvPair

data WordDeclaration = WordDeclaration { word聯 :: [WordConvPair]
                                       , word義 :: Maybe [String]
                                       , word頻度 :: Maybe Double
                                       }
    deriving (Eq, Ord, Show, Read)
deriveJSON defaultOptions{fieldLabelModifier = drop 4} ''WordDeclaration

data JaVerbDeclaration = JaVerbDeclaration { jaVerb類 :: [JaVerbConjugation]
                                           , jaVerb聯 :: [WordConvPair]
                                           }
    deriving (Eq, Ord, Show, Read)
deriveJSON defaultOptions{fieldLabelModifier = drop 6} ''JaVerbDeclaration

data DictEntry = Entry字 KanjiDeclaration
               | Entry語 WordDeclaration
               | Entry日動詞 JaVerbDeclaration
    deriving (Show, Read, Eq, Ord)
deriveJSON defaultOptions{constructorTagModifier = drop 5, sumEncoding = ObjectWithSingleField} ''DictEntry
