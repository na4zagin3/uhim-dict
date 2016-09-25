{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
module Language.UHIM.Dictionary.Publish.LaTeX where

import Language.UHIM.Japanese.Prim
import Language.UHIM.Dictionary.Yaml
import Data.String
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M

import Control.Arrow

emitHeadKanji :: ShapeClass -> String -> String
emitHeadKanji JaCommon k = "\\HeadKanjiJaCommon" ++ "{" ++ k ++ "}"
emitHeadKanji (JaTrad []) k = "\\HeadKanjiJaTrad" ++ "{" ++ k ++ "}"
emitHeadKanji (JaTrad v) k = "\\HeadKanjiJaTrad" ++ "[" ++ v ++ "]" ++ "{" ++ k ++ "}"
emitHeadKanji (JaSimp []) k = "\\HeadKanjiJaSimp" ++ "{" ++ k ++ "}"
emitHeadKanji (JaSimp v) k = "\\HeadKanjiJaSimp" ++ "[" ++ v ++ "]" ++ "{" ++ k ++ "}"
emitHeadKanji (Other v) k = "\\HeadKanjiOther" ++ "[" ++ v ++ "]" ++ "{" ++ k ++ "}"

emitHeadKanjiShapes :: (IsString s) => KanjiShapes -> s
emitHeadKanjiShapes (KanjiShapes ks) = fromString . unwords $ map (uncurry emitHeadKanji) maps
  where
    maps :: [(ShapeClass, String)]
    maps = sort . map (first readShapeKey) $ M.toList ks

headWordKanji :: (IsString s) => (JaYomi -> Maybe Kana) -> (KanjiShapes -> Maybe Kanji) -> WordConvPair -> s
headWordKanji extYomi extKanji kp = fromString . f . extYomi . extractJaPron $ word讀 kp
  where
    kanji = fromMaybe "" $ extKanji $ word字 kp
    f Nothing = kanji
    f (Just y) = "\\ltjruby{" ++ kanji ++ "}{" ++ y ++ "}"


headWord :: (IsString s, Eq s) => [WordConvPair] -> [s]
headWord wcp = concat [ ["\\HeadWord[語]{"]
                      , trad
                      , ["}"]
                      , if trad /= simp
                        then ["\\HeadWordVariant[JaSimp]{"] ++ simp ++ ["}"]
                        else []
                      ]
  where
    trad, simp :: (IsString s, Eq s) => [s]
    trad = map (headWordKanji extractExtKyuKana extractKyuKanji) wcp
    simp = map (headWordKanji extractShinKana extractShinKanji) wcp
    maps :: [(ShapeClass, String)]
    maps = sort . map (first readShapeKey) $ M.toList undefined 

emitPosition :: (IsString s, Monoid s, Eq s) => Position -> s
emitPosition = mconcat . mconcat . map (\(f,p) -> [ "\\Position{", fromString f, "}{", fromString $ show p, "}"])

emitEntry :: (IsString s, Monoid s, Eq s) => (Position, DictEntry) -> s
emitEntry (pos, Entry字 decl) = mconcat [ emitPosition pos
                              , emitHeadKanjiShapes shapes
                              ]
  where
    shapes = kanji體 decl


emitEntry (pos, Entry語 decl) = mconcat [ emitPosition pos
                              , mconcat $ headWord $ word聯 decl
                              ]

emitEntry (pos, Entry日動詞 decl) = ""
emitEntry (pos, Entry日形容詞 decl) = ""
emitEntry (pos, Entry日副詞 decl) = ""

data Config = Config { template :: String
                     }

defaultConfig :: Config
defaultConfig = Config { template = ""
                       }

emitDict :: (IsString s, Monoid s, Eq s) => Config -> Dictionary -> [s]
emitDict c ds  = [ fromString $ template c
                 , "\n"
                 , "\\begin{document}\n"
                 , "\\begin{multicols}{5}\n"
                 , mconcat $ map ((mappend "\\EntrySep\n"). emitEntry) ds
                 , "\n"
                 , "\\end{multicols}\n"
                 , "\\end{document}"
                 ]
