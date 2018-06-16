{-# LANGUAGE OverloadedStrings #-}

module Language.UHIM.Dictionary.Transform.TUTYomi where

import Language.UHIM.Japanese.Prim
import Language.UHIM.Japanese.Adjective
import Language.UHIM.Japanese.Verb

import Language.UHIM.Dictionary.Yaml
import Language.UHIM.Dictionary.SKK.SKKExtended (SKKDict)
import qualified Language.UHIM.Dictionary.SKK.SKKExtended as SKK

import Data.Maybe

type KyuKana = String
type ShinKana = String

type Priority = Double

data ConvEntry = KanjiConversion Kanji [Kana] Priority
               | WordConversion [(Kanji, Kana)] Priority
               | VerbConversion [(Kanji, Kana)] (Kanji, Kana) Priority
    deriving (Show, Read, Eq, Ord)

-- deriveJSON (defaultOptions {sumEncoding = ObjectWithSingleField}) ''ConvEntry

data ExtractConfig = ExtractConfig { yomiExtractor :: JaYomi -> Maybe Kana
                                   , kanjiExtractor :: KanjiShapes -> Maybe Kanji
                                   , kanjiStandardVariant :: [String]
                                   , inflectionMark :: String
                                   }

defaultConfig :: ExtractConfig
defaultConfig = ExtractConfig { yomiExtractor = extractExtKyuKana
                              , kanjiExtractor = extractKyuKanji
                              , kanjiStandardVariant = [ kyuKanjiKey
                                                       , jaKanjiKey
                                                       , commonKanjiKey
                                                       ]
                              , inflectionMark = tcvimeInflectionMark
                              }

uimDefaultConfig :: ExtractConfig
uimDefaultConfig = defaultConfig { inflectionMark = uimInflectionMark }

tcvimeDefaultConfig :: ExtractConfig
tcvimeDefaultConfig = defaultConfig { inflectionMark = tcvimeInflectionMark }

uimInflectionMark :: String
uimInflectionMark = "\x2014"

tcvimeInflectionMark :: String
tcvimeInflectionMark = "\x2015"

combinatorial :: [[a]] -> [[a]]
combinatorial [] = []
combinatorial [xs] = map (: []) xs
combinatorial (xs:xss) = concatMap (\x -> map (x :) (combinatorial xss)) xs

expandVerbConversion :: [(Kana, Kanji)] -> [String]
expandVerbConversion = map concat . combinatorial . map (\(x,y) -> [y,x])

expandConversion :: [(Kana, Kanji)] -> [String]
expandConversion = drop 1 . map concat . combinatorial . map (\(x,y) -> [y,x])

extractConvEntry :: ExtractConfig -> (Position, DictEntry) -> [ConvEntry]
extractConvEntry c (_, ent@(Entry字 decl)) = maybeToList pronConversion
  where
    pronConversion = do
      let ys = concatMap extractJaProns $ kanji音 decl
      k <- kanjiExtractor c $ kanji體 decl
      return $ KanjiConversion k (mapMaybe (yomiExtractor c) ys) $ fromMaybe 1 $ frequency ent

extractConvEntry c (_, ent@(Entry語 decl)) = maybeToList $ do
  kys <- mapM (extractWordConvPair c) $ word聯 decl
  return $ WordConversion kys $ fromMaybe 1 $ frequency ent

extractConvEntry c (_, ent@(Entry日副詞 decl)) = maybeToList $ do
  kys <- mapM (extractWordConvPair c) $ word聯 decl
  return $ WordConversion kys $ fromMaybe 1 $ frequency ent

extractConvEntry c (_, ent@(Entry日動詞 decl)) = f verbConvSuffixes iMark ++ f verbConvFinalSuffixes ""
  where
    iMark = inflectionMark c
    f getSuffixes conjMark = do
      suf <- map getSuffixes $ jaVerb類 decl
      sufy <- mapMaybe (yomiExtractor c) suf
      kys <- maybeToList . mapM (extractVerbWordConvPair c sufy) $ jaVerb聯 decl
      let okuri = if isRequiredOkurigana c decl then sufy else ""
      return $ VerbConversion kys (okuri, okuri ++ conjMark) $ fromMaybe 1 $ frequency ent

extractConvEntry c (_, ent@(Entry日形容詞 decl)) = do
  let iMark = inflectionMark c
  (suf, conj) <- map adjConvSuffixes $ jaAdj類 decl
  okuri <- mapMaybe (yomiExtractor c) suf
  kys <- maybeToList . mapM (extractVerbWordConvPair c okuri) $ jaAdj聯 decl
  return $ VerbConversion kys (okuri, okuri ++ if conj then iMark else "") $ fromMaybe 1 $ frequency ent

extractWordConvPair :: ExtractConfig -> WordConvPair -> Maybe (Kanji, Kana)
extractWordConvPair c kp@(WordConvPair ks p) = do
  k <- kanjiExtractor c ks
  y <- yomiExtractor' $ extractJaProns p
  return (if k == kanaMark then y else k, y)
  where
    yomiExtractor' [x] = yomiExtractor c x
    yomiExtractor' _ = error $ "extractWordConvPair: Currently, only one pronunciation is allowed; but got: " ++ show kp

extractVerbWordConvPair :: ExtractConfig -> String -> WordConvPair -> Maybe (Kanji, Kana)
extractVerbWordConvPair c suf cp = do
  (k, y) <- extractWordConvPair c cp
  return (k, if y == nonOkuriganaMark then suf else y) -- TODO provide a way to allow "$" as a yomi

expandEntry :: ConvEntry -> [(String, Kanji, Priority)]
expandEntry (KanjiConversion k ys f) = map (\y -> (y, k, f)) ys
expandEntry (WordConversion kys f) = map fromConvStr . expandConversion . map extractKana $ kys
    where
      extractKana (k, y) = (y, k)
      fromConvStr s = (s, concatMap fst kys, f)
expandEntry (VerbConversion kys (sk, sy) f) = map fromConvStr . expandVerbConversion . map extractKana $ kys
    where
      extractKana (k, y) = (y, k)
      fromConvStr s = (s ++ sy, concatMap fst kys ++ sk, f)

extractSKK :: ExtractConfig -> Dictionary -> SKKDict
extractSKK conf = foldr f SKK.empty . concatMap expandEntry . concatMap (extractConvEntry conf)
    where
      f (a,b,c) = SKK.append a b c

adjConvSuffixes :: JaAdjConjugation -> ([JaYomi], Bool)
adjConvSuffixes JaAdjSii = ([NonChange "し"], True)
adjConvSuffixes JaAdjSiku = ([NonChange "し"], True)
adjConvSuffixes JaAdjZii = ([NonChange "じ"], True)
adjConvSuffixes JaAdjZiku = ([NonChange "じ"], True)
adjConvSuffixes JaAdjI = ([NonChange ""], True)
adjConvSuffixes JaAdjKu = ([NonChange ""], True)
adjConvSuffixes JaAdjNa = ([NonChange ""], False)
adjConvSuffixes JaAdjNari = ([NonChange ""], False)
adjConvSuffixes JaAdjTari = ([NonChange ""], False)

verbConvSuffixes :: JaVerbConjugation -> [JaYomi]
verbConvSuffixes (JaVerbConjugation _ _ Quadrigrade) = [NonChange ""]
verbConvSuffixes (JaVerbConjugation _ _ Quinquegrade) = [NonChange ""]
verbConvSuffixes (JaVerbConjugation _ StemS Irregular) = []
verbConvSuffixes (JaVerbConjugation _ StemZ Irregular) = []
verbConvSuffixes jvc = conjEndings jvc

verbConvFinalSuffixes :: JaVerbConjugation -> [JaYomi]
verbConvFinalSuffixes (JaVerbConjugation _ _ Quadrigrade) = []
verbConvFinalSuffixes (JaVerbConjugation _ _ Quinquegrade) = []
verbConvFinalSuffixes (JaVerbConjugation _ StemS Irregular) = [NonChange ""]
verbConvFinalSuffixes (JaVerbConjugation _ StemZ Irregular) = [NonChange ""]
verbConvFinalSuffixes (JaVerbConjugation MiddleJapanese StemK Irregular) = [NonChange "き", NonChange "く"]
verbConvFinalSuffixes (JaVerbConjugation ModernStandardJapanese StemK Irregular) = [NonChange "き"]
verbConvFinalSuffixes (JaVerbConjugation MiddleJapanese StemN Irregular) = [NonChange "に", NonChange "ぬ"]
verbConvFinalSuffixes (JaVerbConjugation ModernStandardJapanese StemN Irregular) = [NonChange "に", NonChange "ぬ"]
verbConvFinalSuffixes (JaVerbConjugation ModernStandardJapanese StemR Irregular) = [NonChange "り"]
verbConvFinalSuffixes (JaVerbConjugation MiddleJapanese StemR Irregular) = [NonChange "り"]
verbConvFinalSuffixes jvc = conjEndings jvc

isRequiredOkurigana :: ExtractConfig -> JaVerbDeclaration -> Bool
isRequiredOkurigana c decl = f $ jaVerb聯 decl
  where
    f [] = error $ "isRequiredOkurigana: No yomi definitions: " ++ show decl
    f [cp] = (yomiExtractor' . extractJaProns $ word讀 cp) /= Just nonOkuriganaMark
    f (_:xs) = f xs
    yomiExtractor' [x] = yomiExtractor c x
    yomiExtractor' _ = error $ "isRequiredOkurigana: Currently, only one pronunciation is allowed; but got: " ++ show decl
