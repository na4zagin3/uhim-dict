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
emitHeadKanji JaCommon k = "\\HeadKanjiJaCommon" ++ "{" ++ escape k ++ "}"
emitHeadKanji (JaTrad []) k = "\\HeadKanjiJaTrad" ++ "{" ++ escape k ++ "}"
emitHeadKanji (JaTrad v) k = "\\HeadKanjiJaTrad" ++ "[" ++ escape v ++ "]" ++ "{" ++ escape k ++ "}"
emitHeadKanji (JaSimp []) k = "\\HeadKanjiJaSimp" ++ "{" ++ escape k ++ "}"
emitHeadKanji (JaSimp v) k = "\\HeadKanjiJaSimp" ++ "[" ++ escape v ++ "]" ++ "{" ++ escape k ++ "}"
emitHeadKanji (Other v) k = "\\HeadKanjiOther" ++ "[" ++ escape v ++ "]" ++ "{" ++ escape k ++ "}"

emitHeadKanjiShapes :: (IsString s) => KanjiShapes -> s
emitHeadKanjiShapes (KanjiShapes ks) = fromString . unwords $ map (uncurry emitHeadKanji) maps
  where
    maps :: [(ShapeClass, String)]
    maps = sort . map (first readShapeKey) $ M.toList ks

headWordKanji :: (IsString s) => (JaYomi -> Maybe Kana) -> (KanjiShapes -> Maybe Kanji) -> WordConvPair -> s
headWordKanji extYomi extKanji kp = fromString . f kanji . extYomi . extractJaPron $ word讀 kp
  where
    kanji = fromMaybe "" $ extKanji $ word字 kp
    f k Nothing = k
    f "$$" (Just y) = y
    f k (Just y) = "\\ruby{" ++ escape k ++ "}{" ++ escape y ++ "}"

kanjiYomiElem :: (IsString s, Monoid s) => (String, JaYomi) -> s
kanjiYomiElem (key, NonChange y) = mconcat [ "\\KanjiYomiElem{"
                                           , escape key
                                           , "}{"
                                           , escape y
                                           , "}"
                                           ]
kanjiYomiElem (key, Changed n t) = mconcat [ "\\KanjiYomiElem{"
                                           , escape key
                                           , "}["
                                           , escape n
                                           , "]{"
                                           , escape t
                                           , "}"
                                           ]

kanjiYomi :: (IsString s, Monoid s) => Config -> [Pron] -> s
kanjiYomi _ [] = ""
kanjiYomi c ps = mconcat [ "\\begin{Yomi}\n"
                         , mconcat . mconcat $ map f ps
                         , "\\end{Yomi}"
                         ]
  where
    f :: (IsString s, Monoid s) => Pron -> [s]
    f = (\x -> ["\\YomiUnit{", x, "}\n"]) . mconcat . intersperse (fromString $ yomiSeparator c) . map kanjiYomiElem . extractJaPronList

meaning :: (IsString s, Monoid s) => Config -> [String] -> s
meaning _ [] = ""
meaning _ [s] = mconcat [ "\\begin{MeaningSg}\n"
                        , escape s
                        , "\\end{MeaningSg}\n"
                        ]
meaning _ ss = mconcat [ "\\begin{MeaningPl}\n"
                       , mconcat $ map (\s -> mconcat ["\\Meaning{", escape s, "}\n"]) ss
                       , "\\end{MeaningPl}\n"
                       ]


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
emitPosition = mconcat . mconcat . map (\(f,p) -> [ "\\Position{", escape f, "}{", escape $ show p, "}"])

emitEntry :: (IsString s, Monoid s, Eq s) => Config -> (Position, DictEntry) -> s
emitEntry c (pos, Entry字 decl) = mconcat [ emitPosition pos
                                          , "\n"
                                          , emitHeadKanjiShapes $ kanji體 decl
                                          , "\n"
                                          , kanjiYomi c $ kanji音 decl
                                          , "\n"
                                          , fromMaybe "" (meaning c <$> kanji義 decl)
                                          ]

emitEntry c (pos, Entry語 decl) = mconcat [ emitPosition pos
                                          , "\n"
                                          , mconcat $ headWord $ word聯 decl
                                          , "\n"
                                          , fromMaybe "" (meaning c <$> word義 decl)
                                          ]

emitEntry c (pos, Entry日動詞 decl) = ""
emitEntry c (pos, Entry日形容詞 decl) = ""
emitEntry c (pos, Entry日副詞 decl) = ""

data Config = Config { template :: String
                     , multicols :: (String, String)
                     , yomiSeparator :: String
                     }

defaultConfig :: Config
defaultConfig = Config { template = ""
                       , multicols = ("\\begin{multicols*}{5}\n", "\\end{multicols*}\n")
                       , yomiSeparator = "\\\\"
                       }

emitDict :: (IsString s, Monoid s, Eq s) => Config -> Dictionary -> [s]
emitDict c ds  = [ fromString $ template c
                 , "\n"
                 , "\\begin{document}\n"
                 , fromString . fst $ multicols c
                 , mconcat $ map (entryEnv . emitEntry c) ds
                 , "\n"
                 , fromString . snd $ multicols c
                 , "\\end{document}"
                 ]
  where
    entryEnv s = mconcat [ "\\begin{Entry}\n"
                         , s
                         ,  "\\end{Entry}\n"
                         ]

escapeChar :: IsString s => Char -> s
escapeChar '\\' = "\\textbackslash "
escapeChar '$' = "\\$"
escapeChar '%' = "\\%"
escapeChar '#' = "\\#"
escapeChar '{' = "\\{"
escapeChar '}' = "\\}"
escapeChar '&' = "\\&"
escapeChar '_' = "\\_"
escapeChar '^' = "\\^"
escapeChar c = fromString [c]

escape :: (IsString s) => String -> s
escape = fromString . concatMap escapeChar
