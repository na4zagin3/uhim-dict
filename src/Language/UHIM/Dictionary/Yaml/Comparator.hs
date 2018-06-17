{-# LANGUAGE FlexibleContexts #-}
module Language.UHIM.Dictionary.Yaml.Comparator where

import Control.Lens
import Data.Function (on)
import Data.Maybe

import Language.UHIM.Japanese.Prim
import qualified Language.UHIM.Japanese.Collator as Col

import Language.UHIM.Dictionary.Yaml

data Config = Config { yomiExtractor :: JaYomi -> Maybe Kana
                     , kanjiExtractor :: KanjiShapes -> Maybe Kanji
                     , kanjiStandardVariant :: [String]
                     , kanaCollator :: Col.Collator
                     }

defaultConfig :: Config
defaultConfig = Config { yomiExtractor = extractExtKyuKana
                       , kanjiExtractor = extractKyuKanji
                       , kanjiStandardVariant = [ kyuKanjiKey
                                                , jaKanjiKey
                                                , commonKanjiKey
                                                ]
                       , kanaCollator = Col.japaneseCollator False
                       }

-- ToDo: Rewrite with Prism
extractYomi :: Config -> DictEntry -> String
extractYomi _ (Entry字 _) = ""
extractYomi c (Entry語 wd) = extractYomiFromWordConvPairs c $ wd^.decl聯
extractYomi c (Entry日動詞 wd) = extractYomiFromWordConvPairs c $ wd^.decl聯
extractYomi c (Entry日形容詞 wd) = extractYomiFromWordConvPairs c $ wd^.decl聯
extractYomi c (Entry日副詞 wd) = extractYomiFromWordConvPairs c $ wd^.decl聯

-- ToDo Support inflexion endings. (gh-63)
extractYomiFromWordConvPairs :: Config -> [WordConvPair] -> String
extractYomiFromWordConvPairs c wcs = mconcat . catMaybes $ map extractYomiFromWordConvPair wcs
  where
    extractYomiFromWordConvPair wc = (extractJaPron $ wc^.decl讀) >>= yomiExtractor c >>= extractYomiFromWord wc
    extractYomiFromWord wc "$" = kanjiExtractor c $ wc^.decl字
    extractYomiFromWord _ x = Just x


compareJaClassical :: Config -> DictEntry -> DictEntry -> Ordering
compareJaClassical c a b = mconcat [ compareYomi
                                   , lastResort
                                   ]
  where
    compareYomi = (Col.collate (kanaCollator c) `on` extractYomi c) a b
    lastResort = compare a b
