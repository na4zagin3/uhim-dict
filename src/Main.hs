{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where

import Dictionary.SKK.SKKExtended (SKKDict)
import qualified Dictionary.SKK.SKKExtended as SKK

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

type ShinKana = String
type KyuKana = String
type Kana = String

type Kanji = String
type Frequency = Double


data ConvEntry = KanjiConversion Kanji [Kana] Frequency
               | WordConversion [(Kanji, Kana)] Frequency
    deriving (Show, Read, Eq, Ord)

deriveJSON (defaultOptions {sumEncoding = ObjectWithSingleField}) ''ConvEntry

data ExtractConfig = ExtractConfig { yomiExtractor :: JaYomi -> Maybe Kana
                                   , kanjiExtractor :: KanjiShapes -> Maybe Kanji
                                   , kanjiStandardVariant :: [String]
                                   }

defaultConfig :: ExtractConfig
defaultConfig = ExtractConfig { yomiExtractor = extractKyuKana
                              , kanjiExtractor = extractKyuKanji
                              , kanjiStandardVariant = [ kyuKanjiKey
                                                       , jaKanjiKey
                                                       , commonKanjiKey
                                                       ]
                              }

combinatorial :: [[a]] -> [[a]]
combinatorial [] = []
combinatorial [xs] = map (: []) xs
combinatorial (xs:xss) = concatMap (\x -> map (x :) (combinatorial xss)) xs

expandConversion :: [(Kana, Kanji)] -> [String]
expandConversion = drop 1 . map concat . combinatorial . map (\(x,y) -> [y,x])

extractShinKana :: JaYomi -> Maybe Kana
extractShinKana (NonChange x) = Just x
extractShinKana (Changed "" _) = Nothing
extractShinKana (Changed x _) = Just x

extractKyuKana :: JaYomi -> Maybe Kana
extractKyuKana (NonChange x) = Just x
extractKyuKana (Changed _ "") = Nothing
extractKyuKana (Changed _ x) = Just x

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

extractConvEntry :: ExtractConfig -> DictEntry -> [ConvEntry]
extractConvEntry c (Entry字 decl) = maybeToList $ pronConversion
  where
    pronConversion = do
      let ys = concatMap extractJaProns $ kanji音 decl
      k <- kanjiExtractor c $ kanji體 decl
      return $ KanjiConversion k (mapMaybe (yomiExtractor c) ys) 1 -- ToDo: support frequency

extractConvEntry c (Entry語 decl) = maybeToList $ do
  kys <- mapM (extractWordConvPair c) $ word聯 decl
  return $ WordConversion kys 1

extractWordConvPair :: ExtractConfig -> WordConvPair -> Maybe (Kanji, Kana)
extractWordConvPair c (WordConvPair ks p) = do
  k <- kanjiExtractor c ks
  y <- yomiExtractor c $ extractJaPron p
  return (k, y)

expandEntry :: ConvEntry -> [(String, Kanji, Frequency)]
expandEntry (KanjiConversion k ys f) = map (\y -> (y, k, f)) ys
expandEntry (WordConversion kys f) = map fromConvStr . expandConversion . map extractKana $ kys
    where
      extractKana (k, y) = (y, k)
      fromConvStr s = (s, concatMap fst kys, f)

extractSKK :: ExtractConfig -> [DictEntry] -> SKKDict
extractSKK conf = foldr f SKK.empty . concatMap expandEntry . concatMap (extractConvEntry conf)
    where
      f (a,b,c) = SKK.append a b c

main :: IO ()
main = do
  str <- Y.decodeEither' <$> BS.getContents
  case str of
    Left e -> print e
    Right y -> putStrLn . SKK.emitSKKDictionary $ extractSKK defaultConfig y
