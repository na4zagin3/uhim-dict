module Language.UHIM.Dictionary.Yaml.Comparator where

import Data.Function (on)
import Data.Maybe

import Language.UHIM.Japanese.Prim
import Language.UHIM.Japanese.Adjective
import Language.UHIM.Japanese.Verb

import Language.UHIM.Dictionary.Yaml
import Language.UHIM.Dictionary.Yaml

data Config = Config { yomiExtractor :: JaYomi -> Maybe Kana
                     , kanjiExtractor :: KanjiShapes -> Maybe Kanji
                     , kanjiStandardVariant :: [String]
                     }

defaultConfig :: Config
defaultConfig = Config { yomiExtractor = extractExtKyuKana
                       , kanjiExtractor = extractKyuKanji
                       , kanjiStandardVariant = [ kyuKanjiKey
                                                , jaKanjiKey
                                                , commonKanjiKey
                                                ]
                       }

extractYomi :: Config -> DictEntry -> String
extractYomi c (Entry字 kd) = ""
extractYomi c (Entry語 wd) = extractYomiFromWordConvPairs c $ word聯 wd
extractYomi c (Entry日動詞 wd) = extractYomiFromWordConvPairs c $ jaVerb聯 wd
extractYomi c (Entry日形容詞 wd) = extractYomiFromWordConvPairs c $ jaAdj聯 wd
extractYomi c (Entry日副詞 wd) = extractYomiFromWordConvPairs c $ word聯 wd

extractYomiFromWordConvPairs :: Config -> [WordConvPair] -> String
extractYomiFromWordConvPairs c wcs = mconcat . catMaybes $ map extractYomi wcs
  where
    extractYomi wc = (extractJaPron $ word讀 wc) >>= yomiExtractor c >>= extractYomiFromWord wc
    extractYomiFromWord wc "$" = kanjiExtractor c $ word字 wc
    extractYomiFromWord _ x = Just x


compareJaClassical :: Config -> DictEntry -> DictEntry -> Ordering
compareJaClassical c a b = mconcat [ compareYomi
                                   , lastResort
                                   ]
  where
    -- ToDo: language specific comparison. e.g, か < カク < かた
    compareYomi = (compare `on` extractYomi c) a b
    lastResort = compare a b
