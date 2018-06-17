{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
module Language.UHIM.Dictionary.Yaml where

import Control.Lens
import Control.Lens.TH ()
import Data.Aeson.TH
import Data.Aeson.Types (FromJSON, ToJSON, Value(..), pairs, parseJSON, typeMismatch)
import qualified Data.Aeson.Types as AE
import qualified Data.ByteString as BS
import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.Text as T
import qualified Data.Yaml as Y
import qualified Data.Yaml.Include as YI
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
makeLenses ''Pron

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
    toEncoding (KanjiShapes vks) = pairs . mconcat . map (\(k, v)-> T.pack k AE..= v) . M.toList $ vks

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

data KanjiDeclaration = KanjiDeclaration { kanjiDeclarationDecl體 :: KanjiShapes
                                         , kanjiDeclarationDecl音 :: [Pron]
                                         , kanjiDeclarationDecl形 :: Maybe (Map String String)
                                         , kanjiDeclarationDecl義 :: Maybe [String]
                                         , kanjiDeclarationDecl鍵 :: Maybe (Map String String)
                                         , kanjiDeclarationDecl頻度 :: Maybe Double
                                         , kanjiDeclarationDecl簽 :: Maybe [String]
                                         }
    deriving (Eq, Ord, Show, Read)
deriveJSON jsonOptions{fieldLabelModifier = drop 20} ''KanjiDeclaration
makeFields ''KanjiDeclaration

emptyKanjiDeclaration :: KanjiDeclaration
emptyKanjiDeclaration = KanjiDeclaration { kanjiDeclarationDecl體 = KanjiShapes M.empty
                                         , kanjiDeclarationDecl音 = []
                                         , kanjiDeclarationDecl形 = Nothing
                                         , kanjiDeclarationDecl義 = Nothing
                                         , kanjiDeclarationDecl鍵 = Nothing
                                         , kanjiDeclarationDecl頻度 = Nothing
                                         , kanjiDeclarationDecl簽 = Nothing
                                         }

data WordConvPair = WordConvPair { wordConvPairDecl字 :: KanjiShapes
                                 , wordConvPairDecl讀 :: Pron
                                 }
    deriving (Eq, Ord, Show, Read)
deriveJSON jsonOptions{fieldLabelModifier = drop 16} ''WordConvPair
makeFields ''WordConvPair

data WordDeclaration = WordDeclaration { wordDeclarationDecl聯 :: [WordConvPair]
                                       , wordDeclarationDecl義 :: Maybe [String]
                                       , wordDeclarationDecl頻度 :: Maybe Double
                                       , wordDeclarationDecl簽 :: Maybe [String]
                                       }
    deriving (Eq, Ord, Show, Read)
deriveJSON jsonOptions{fieldLabelModifier = drop 19} ''WordDeclaration
makeFields ''WordDeclaration

data JaVerbDeclaration = JaVerbDeclaration { jaVerbDeclarationDecl類 :: [JaVerbConjugation]
                                           , jaVerbDeclarationDecl聯 :: [WordConvPair]
                                           , jaVerbDeclarationDecl義 :: Maybe [String]
                                           , jaVerbDeclarationDecl頻度 :: Maybe Double
                                           , jaVerbDeclarationDecl簽 :: Maybe [String]
                                           }
    deriving (Eq, Ord, Show, Read)
deriveJSON jsonOptions{fieldLabelModifier = drop 21} ''JaVerbDeclaration
makeFields ''JaVerbDeclaration

data JaAdjDeclaration = JaAdjDeclaration { jaAdjDeclarationDecl類 :: [JaAdjConjugation]
                                         , jaAdjDeclarationDecl聯 :: [WordConvPair]
                                         , jaAdjDeclarationDecl義 :: Maybe [String]
                                         , jaAdjDeclarationDecl頻度 :: Maybe Double
                                         , jaAdjDeclarationDecl簽 :: Maybe [String]
                                         }
    deriving (Eq, Ord, Show, Read)
deriveJSON jsonOptions{fieldLabelModifier = drop 20} ''JaAdjDeclaration
makeFields ''JaAdjDeclaration

data DictEntry = Entry字 KanjiDeclaration
               | Entry語 WordDeclaration
               | Entry日動詞 JaVerbDeclaration
               | Entry日形容詞 JaAdjDeclaration
               | Entry日副詞 WordDeclaration
    deriving (Show, Read, Eq, Ord)
deriveJSON jsonOptions{constructorTagModifier = drop 5, sumEncoding = ObjectWithSingleField} ''DictEntry
makePrisms ''DictEntry

instance HasDecl義 DictEntry (Maybe [String]) where
  decl義 = lens g s
    where
      g (Entry字 d) = d ^. decl義
      g (Entry語 d) = d ^. decl義
      g (Entry日動詞 d) = d ^. decl義
      g (Entry日形容詞 d) = d ^. decl義
      g (Entry日副詞 d) = d ^. decl義
      s (Entry字 d) v = Entry字 $ d & decl義 .~ v
      s (Entry語 d) v = Entry語 $ d & decl義 .~ v
      s (Entry日動詞 d) v = Entry日動詞 $ d & decl義 .~ v
      s (Entry日形容詞 d) v = Entry日形容詞 $ d & decl義 .~ v
      s (Entry日副詞 d) v = Entry日副詞 $ d & decl義 .~ v

instance HasDecl頻度 DictEntry (Maybe Double) where
  decl頻度 = lens g s
    where
      g (Entry字 d) = d ^. decl頻度
      g (Entry語 d) = d ^. decl頻度
      g (Entry日動詞 d) = d ^. decl頻度
      g (Entry日形容詞 d) = d ^. decl頻度
      g (Entry日副詞 d) = d ^. decl頻度
      s (Entry字 d) v = Entry字 $ d & decl頻度 .~ v
      s (Entry語 d) v = Entry語 $ d & decl頻度 .~ v
      s (Entry日動詞 d) v = Entry日動詞 $ d & decl頻度 .~ v
      s (Entry日形容詞 d) v = Entry日形容詞 $ d & decl頻度 .~ v
      s (Entry日副詞 d) v = Entry日副詞 $ d & decl頻度 .~ v

instance HasDecl簽 DictEntry (Maybe [String]) where
  decl簽 = lens g s
    where
      g (Entry字 d) = d ^. decl簽
      g (Entry語 d) = d ^. decl簽
      g (Entry日動詞 d) = d ^. decl簽
      g (Entry日形容詞 d) = d ^. decl簽
      g (Entry日副詞 d) = d ^. decl簽
      s (Entry字 d) v = Entry字 $ d & decl簽 .~ v
      s (Entry語 d) v = Entry語 $ d & decl簽 .~ v
      s (Entry日動詞 d) v = Entry日動詞 $ d & decl簽 .~ v
      s (Entry日形容詞 d) v = Entry日形容詞 $ d & decl簽 .~ v
      s (Entry日副詞 d) v = Entry日副詞 $ d & decl簽 .~ v

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
entryLabel d = d ^. decl簽

frequency :: DictEntry -> Maybe Double
frequency d = d ^. decl頻度

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
  { wordConvPairDecl字 = KanjiShapes $ M.singleton jaKanjiKey kanaMark
  , wordConvPairDecl讀 = emptyPron { pron日送 = Just s }
  }

nonOkuriganaMark :: String
nonOkuriganaMark = "$"

kanaMark :: String
kanaMark = "$$"
