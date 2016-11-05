{-# LANGUAGE OverloadedStrings #-}

module Language.UHIM.Dictionary.Transform.Variants where

import Language.UHIM.Japanese.Prim

import Language.UHIM.Dictionary.Yaml
import Language.UHIM.Dictionary.SKK.SKKExtended (SKKDict)
import qualified Language.UHIM.Dictionary.SKK.SKKExtended as SKK

import qualified Data.Map as M
import Data.Maybe

data ExtractConfig = ExtractConfig { shapePriority :: Double -> String -> Maybe Double
                                   , baseShape :: KanjiShapes -> [String]
                                   }

defaultConfig :: ExtractConfig
defaultConfig = ExtractConfig { shapePriority = defaultPriority
                              , baseShape = defaultBaseShapes
                              }

defaultPriority :: Double -> String -> Maybe Double
defaultPriority freq _ = Just freq

defaultBaseShapes :: KanjiShapes -> [String]
defaultBaseShapes (KanjiShapes ss) = maybeToList $ M.lookup kyuKanjiKey ss

defaultFrequency :: Double
defaultFrequency = 1.0

revConvFrequency :: Double
revConvFrequency = 1.0

extractKanjiShapes :: (Position, DictEntry) -> Maybe (KanjiShapes, Double)
extractKanjiShapes (_, ent@(Entry字 decl)) = Just (kanji體 decl, fromMaybe defaultFrequency $ frequency ent)
extractKanjiShapes _ = Nothing

expandKanjiShapes :: ExtractConfig -> (KanjiShapes, Double) -> [(Kana, Kanji, Double)]
expandKanjiShapes c (ss@(KanjiShapes m), freq) = do
  f <- baseShape c ss
  (k, s) <- M.toList m
  maybe [] id $ do
    freq' <- shapePriority c freq k
    return [(f, s, freq'), (s, f, revConvFrequency)]

extractSKK :: ExtractConfig -> Dictionary -> SKKDict
extractSKK conf dict = foldr f SKK.empty ykfs
  where
    ykfs = concatMap (catMaybes . mapM (expandKanjiShapes conf) . extractKanjiShapes) dict
    f (a,b,c) = SKK.append a b c
