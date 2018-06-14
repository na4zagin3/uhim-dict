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
import qualified Data.Yaml as Y
import qualified Data.Yaml.Include as YI
import qualified Data.ByteString as BS
-- import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Lazy as BL
-- import qualified Data.ByteString.Lazy.UTF8 as BLU
import Data.Aeson.TH
import Data.Aeson.Types hiding (defaultOptions)
import qualified Data.Aeson.Types as AE
-- import Data.Monoid
import Data.Maybe
-- import Text.Parsec
import GHC.Generics

import Language.UHIM.Japanese.Prim
import Language.UHIM.Japanese.Verb
import Language.UHIM.Japanese.Adjective
import Language.UHIM.Dictionary.Yaml.Prim

data Pron = Pron { pron日呉 :: Maybe JaYomi
                 , pron日漢 :: Maybe JaYomi
                 , pron日訓 :: Maybe JaYomi
                 , pron日慣用 :: Maybe JaYomi
                 , pron日送 :: Maybe JaYomi
                 , pron日迎 :: Maybe JaYomi
                 , pron日音 :: Maybe JaYomi
                 }
    deriving (Eq, Ord, Show, Read)
deriveJSON jsonOptions{fieldLabelModifier = drop 4} ''Pron

emptyPron :: Pron
emptyPron = Pron { pron日漢 = Nothing
                 , pron日呉 = Nothing
                 , pron日訓 = Nothing
                 , pron日慣用 = Nothing
                 , pron日送 = Nothing
                 , pron日迎 = Nothing
                 , pron日音 = Nothing
                 }

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

data ShapeClass = JaCommon
                | JaTrad String
                | JaSimp String
                | Other String
  deriving (Show, Read, Ord, Eq)

readShapeKey :: String -> ShapeClass
readShapeKey "日" = JaCommon
readShapeKey ('日':'舊':v) = JaTrad v
readShapeKey ('日':'新':v) = JaSimp v
readShapeKey v = Other v

defaultKanjiKeyOrder :: String -> String -> Ordering
defaultKanjiKeyOrder x y = compare (readShapeKey x) (readShapeKey y)

kanYomiKey, goYomiKey, touYomiKey :: String
kanyoYomiKey, okuriYomiKey, mukaeYomiKey :: String
kanYomiKey = "日漢"
goYomiKey = "日呉"
touYomiKey = "日唐"
kanyoYomiKey = "日慣用"
okuriYomiKey = "日送"
mukaeYomiKey = "日迎"

{-

deriveJSON defaultOptions ''KanjiShapes
-}

data KanjiDeclaration = KanjiDeclaration { kanji體 :: KanjiShapes
                                         , kanji音 :: [Pron]
                                         , kanji形 :: Maybe (Map String String)
                                         , kanji義 :: Maybe [String]
                                         , kanji鍵 :: Maybe (Map String String)
                                         , kanji頻度 :: Maybe Double
                                         , kanji簽 :: Maybe [String]
                                         }
    deriving (Eq, Ord, Show, Read)
deriveJSON jsonOptions{fieldLabelModifier = drop 5} ''KanjiDeclaration
emptyKanjiDeclaration :: KanjiDeclaration
emptyKanjiDeclaration = KanjiDeclaration { kanji體 = KanjiShapes M.empty
                                         , kanji音 = []
                                         , kanji形 = Nothing
                                         , kanji義 = Nothing
                                         , kanji鍵 = Nothing
                                         , kanji頻度 = Nothing
                                         , kanji簽 = Nothing
                                         }

data WordConvPair = WordConvPair { word字 :: KanjiShapes
                                 , word讀 :: Pron
                                 }
    deriving (Eq, Ord, Show, Read)
deriveJSON jsonOptions{fieldLabelModifier = drop 4} ''WordConvPair

data WordDeclaration = WordDeclaration { word聯 :: [WordConvPair]
                                       , word義 :: Maybe [String]
                                       , word頻度 :: Maybe Double
                                       , word簽 :: Maybe [String]
                                       }
    deriving (Eq, Ord, Show, Read)
deriveJSON jsonOptions{fieldLabelModifier = drop 4} ''WordDeclaration

data JaVerbDeclaration = JaVerbDeclaration { jaVerb類 :: [JaVerbConjugation]
                                           , jaVerb聯 :: [WordConvPair]
                                           , jaVerb義 :: Maybe [String]
                                           , jaVerb頻度 :: Maybe Double
                                           , jaVerb簽 :: Maybe [String]
                                           }
    deriving (Eq, Ord, Show, Read)
deriveJSON jsonOptions{fieldLabelModifier = drop 6} ''JaVerbDeclaration

data JaAdjDeclaration = JaAdjDeclaration { jaAdj類 :: [JaAdjConjugation]
                                         , jaAdj聯 :: [WordConvPair]
                                         , jaAdj義 :: Maybe [String]
                                         , jaAdj頻度 :: Maybe Double
                                         , jaAdj簽 :: Maybe [String]
                                         }
    deriving (Eq, Ord, Show, Read)
deriveJSON jsonOptions{fieldLabelModifier = drop 5} ''JaAdjDeclaration

data DictEntry = Entry字 KanjiDeclaration
               | Entry語 WordDeclaration
               | Entry日動詞 JaVerbDeclaration
               | Entry日形容詞 JaAdjDeclaration
               | Entry日副詞 WordDeclaration
    deriving (Show, Read, Eq, Ord)
deriveJSON jsonOptions{constructorTagModifier = drop 5, sumEncoding = ObjectWithSingleField} ''DictEntry

type Position = [(String, Integer)]

type Dictionary = [(Position, DictEntry)]

readFromFile :: FilePath -> IO (Either Y.ParseException Dictionary)
readFromFile fp = do
  es <- YI.decodeFileEither fp
  return $ fmap (map (\(i, x) -> ([(fp, i)], x)) . zip [0..]) es

readFromBS :: String -> BS.ByteString -> Either Y.ParseException Dictionary
readFromBS fp str = do
  es <- Y.decodeEither' str
  return $ map (\(i, x) -> ([(fp, i)], x)) $ zip [0..] es

writeToFile :: Dictionary -> FilePath -> IO ()
writeToFile d fp = Y.encodeFile fp $ map snd d

writeToBS :: Dictionary -> BS.ByteString
writeToBS = Y.encode . map snd

entryLabel :: DictEntry -> Maybe [String]
entryLabel (Entry字 decl) = kanji簽 decl
entryLabel (Entry語 decl) = word簽 decl
entryLabel (Entry日動詞 decl) = jaVerb簽 decl
entryLabel (Entry日形容詞 decl) = jaAdj簽 decl
entryLabel (Entry日副詞 decl) = word簽 decl

frequency :: DictEntry -> Maybe Double
frequency (Entry字 decl) = kanji頻度 decl
frequency (Entry語 decl) = word頻度 decl
frequency (Entry日動詞 decl) = jaVerb頻度 decl
frequency (Entry日形容詞 decl) = jaAdj頻度 decl
frequency (Entry日副詞 decl) = word頻度 decl

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


extractJaPronList :: Pron -> [(String, JaYomi)]
extractJaPronList p@Pron{pron日漢 = Just x}  =  ("日漢", x) : extractJaPronList (p {pron日漢 = Nothing})
extractJaPronList p@Pron{pron日呉 = Just x}  =  ("日呉", x) : extractJaPronList (p {pron日呉 = Nothing})
extractJaPronList p@Pron{pron日慣用 = Just x}  =  ("日慣用", x) : extractJaPronList (p {pron日慣用 = Nothing})
extractJaPronList p@Pron{pron日訓 = Just x}  =  ("日呉", x) : extractJaPronList (p {pron日訓 = Nothing})
extractJaPronList p@Pron{pron日送 = Just x}  =  ("日送", x) : extractJaPronList (p {pron日送 = Nothing})
extractJaPronList p@Pron{pron日迎 = Just x}  =  ("日迎", x) : extractJaPronList (p {pron日迎 = Nothing})
extractJaPronList p@Pron{pron日音 = Just x}  =  ("日音", x) : extractJaPronList (p {pron日音 = Nothing})
extractJaPronList _  =  []

extractJaPron :: Pron -> Maybe JaYomi
extractJaPron (Pron (Just x) Nothing  Nothing  Nothing  Nothing  Nothing Nothing)  = Just x
extractJaPron (Pron Nothing  (Just x) Nothing  Nothing  Nothing  Nothing Nothing)  = Just x
extractJaPron (Pron Nothing  Nothing  (Just x) Nothing  Nothing  Nothing Nothing)  = Just x
extractJaPron (Pron Nothing  Nothing  Nothing  (Just x) Nothing  Nothing Nothing)  = Just x
extractJaPron (Pron Nothing  Nothing  Nothing  Nothing  (Just x) Nothing Nothing)  = Just x
extractJaPron (Pron Nothing  Nothing  Nothing  Nothing  Nothing  (Just x) Nothing) = Just x
extractJaPron (Pron Nothing  Nothing  Nothing  Nothing  Nothing  Nothing (Just x)) = Just x
extractJaPron _ = Nothing

extractJaProns :: Pron -> [JaYomi]
extractJaProns = map snd . extractJaPronList

okurigana :: JaYomi -> WordConvPair
okurigana s = WordConvPair
  { word字 = KanjiShapes $ M.singleton jaKanjiKey "$$"
  , word讀 = emptyPron { pron日送 = Just s }
  }
