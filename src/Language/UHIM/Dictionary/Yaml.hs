{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Language.UHIM.Dictionary.Yaml where

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
import Data.Maybe
-- import Text.Parsec
import GHC.Generics

import Language.UHIM.Japanese.Prim
import Language.UHIM.Japanese.Verb
import Language.UHIM.Japanese.Adjective

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
                                         , kanji形 :: Map String String
                                         , kanji義 :: Maybe [String]
                                         , kanji鍵 :: Maybe (Map String String)
                                         , kanji頻度 :: Maybe Double
                                         , kanji簽 :: Maybe [String]
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
                                       , word簽 :: Maybe [String]
                                       }
    deriving (Eq, Ord, Show, Read)
deriveJSON defaultOptions{fieldLabelModifier = drop 4} ''WordDeclaration

data JaVerbDeclaration = JaVerbDeclaration { jaVerb類 :: [JaVerbConjugation]
                                           , jaVerb聯 :: [WordConvPair]
                                           , jaVerb頻度 :: Maybe Double
                                           , jaVerb簽 :: Maybe [String]
                                           }
    deriving (Eq, Ord, Show, Read)
deriveJSON defaultOptions{fieldLabelModifier = drop 6} ''JaVerbDeclaration

data JaAdjDeclaration = JaAdjDeclaration { jaAdj類 :: [JaAdjConjugation]
                                         , jaAdj聯 :: [WordConvPair]
                                         , jaAdj頻度 :: Maybe Double
                                         , jaAdj簽 :: Maybe [String]
                                         }
    deriving (Eq, Ord, Show, Read)
deriveJSON defaultOptions{fieldLabelModifier = drop 5} ''JaAdjDeclaration

data DictEntry = Entry字 KanjiDeclaration
               | Entry語 WordDeclaration
               | Entry日動詞 JaVerbDeclaration
               | Entry日形容詞 JaAdjDeclaration
    deriving (Show, Read, Eq, Ord)
deriveJSON defaultOptions{constructorTagModifier = drop 5, sumEncoding = ObjectWithSingleField} ''DictEntry


entryLabel :: DictEntry -> Maybe [String]
entryLabel (Entry字 decl) = kanji簽 decl
entryLabel (Entry語 decl) = word簽 decl
entryLabel (Entry日動詞 decl) = jaVerb簽 decl
entryLabel (Entry日形容詞 decl) = jaAdj簽 decl

frequency :: DictEntry -> Maybe Double
frequency (Entry字 decl) = kanji頻度 decl
frequency (Entry語 decl) = word頻度 decl
frequency (Entry日動詞 decl) = jaVerb頻度 decl
frequency (Entry日形容詞 decl) = jaAdj頻度 decl

-- Utils
convExtToTrad :: Char -> Char
convExtToTrad '\x1b000' = 'エ'
convExtToTrad '\x1b001' = 'え'
convExtToTrad x = x

extractShinKana :: JaYomi -> Maybe Kana
extractShinKana (NonChange x) = Just x
extractShinKana (Changed "" _) = Nothing
extractShinKana (Changed x _) = Just x

extractKyuKana :: JaYomi -> Maybe Kana
extractKyuKana (NonChange x) = Just $ map convExtToTrad x
extractKyuKana (Changed _ "") = Nothing
extractKyuKana (Changed _ x) = Just $ map convExtToTrad x

extractExtKyuKana :: JaYomi -> Maybe Kana
extractExtKyuKana (NonChange x) = Just x
extractExtKyuKana (Changed _ "") = Nothing
extractExtKyuKana (Changed _ x) = Just x

extractShinKanji :: KanjiShapes -> Maybe Kanji
extractShinKanji (KanjiShapes vks) = mconcat $ map (`M.lookup` vks) [ shinKanjiKey
                                                                    , jaKanjiKey
                                                                    , commonKanjiKey
                                                                    ]

extractKyuKanji :: KanjiShapes -> Maybe Kanji
extractKyuKanji (KanjiShapes vks) = mconcat $ map (`M.lookup` vks) [ kyuKanjiKey
                                                                   , jaKanjiKey
                                                                   , commonKanjiKey
                                                                   ]


extractJaPron :: Pron -> JaYomi
extractJaPron (Pron (Just x) Nothing  Nothing)  = x
extractJaPron (Pron Nothing  (Just x) Nothing)  = x
extractJaPron (Pron Nothing  Nothing  (Just x)) = x
extractJaPron x = error $ "extractJaPron: More than one pronunciation in " ++ show x

extractJaProns :: Pron -> [JaYomi]
extractJaProns pron = mapMaybe (\f -> f pron) [pron日呉, pron日漢, pron日訓]
