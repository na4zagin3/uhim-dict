{-# LANGUAGE OverloadedStrings #-}
module Language.UHIM.Dictionary.Publish.LaTeX where

import Language.UHIM.Dictionary.Yaml
import Data.String
import Data.List
import Data.Map (Map)
import qualified Data.Map as M

import Control.Arrow

preamble :: (IsString s) => [s]
preamble = [ "\\documentclass[a4paper,landscape]{ltjtarticle}"
           -- , "\\documentclass[a4paper,landscape]{ltjsarticle}"
           , "\\usepackage{luatexja}"
           , "\\usepackage{luatexja-fontspec}"
           , ""
           ]

emitKanji :: Maybe Pron -> ShapeClass -> String -> String
emitKanji Nothing JaCommon k = k
emitKanji Nothing (JaTrad []) k = k
emitKanji Nothing (JaTrad v) k = k
emitKanji Nothing (JaSimp []) k = k
emitKanji Nothing (JaSimp v) k = k
emitKanji Nothing (Other v) k = k

emitKanjiShapes :: (IsString s) => Maybe Pron -> KanjiShapes -> s
emitKanjiShapes Nothing (KanjiShapes ks) = fromString . unwords $ map (uncurry $ emitKanji Nothing) maps
  where
    maps :: [(ShapeClass, String)]
    maps = sort . map (first readShapeKey) $ M.toList ks

emitEntry :: (IsString s) => (Position, DictEntry) -> [s]
emitEntry (pos, Entry字 decl) = [ fromString $ show pos
                              , ": "
                              , emitKanjiShapes Nothing shapes
                              , "\n"
                              , "\n"
                              ]
  where
    shapes = kanji體 decl

emitEntry (pos, Entry語 decl) = []
emitEntry (pos, Entry日動詞 decl) = []
emitEntry (pos, Entry日形容詞 decl) = []
emitEntry (pos, Entry日副詞 decl) = []

emitDict :: (IsString s) => Dictionary -> [s]
emitDict ds = mconcat [ preamble
                         , [ "\n" ]
                         , [ "\\begin{document}" ]
                         , [ "\n" ]
                         , concatMap emitEntry ds
                         , [ "\n" ]
                         , [ "\\end{document}" ]
                         ]
