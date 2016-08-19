{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Dictionary.Transform.TUTYomi where

import Dictionary.Yaml
import Dictionary.Yaml.Japanese.Prim
import Dictionary.Yaml.Japanese.Verb
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
                                   }

defaultConfig :: ExtractConfig
defaultConfig = ExtractConfig { yomiExtractor = extractExtKyuKana
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

expandVerbConversion :: [(Kana, Kanji)] -> [String]
expandVerbConversion = map concat . combinatorial . map (\(x,y) -> [y,x])

expandConversion :: [(Kana, Kanji)] -> [String]
expandConversion = drop 1 . map concat . combinatorial . map (\(x,y) -> [y,x])

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

-- ToDo: 終止形と連用形は、句末に助詞を伴わず出現しうることへの対応
extractConvEntry c (Entry日動詞 decl) = do
  suf <- map verbConvSuffixes $ jaVerb類 decl
  sufy <- catMaybes $ map (yomiExtractor c) suf
  kys <- maybeToList . mapM (extractVerbWordConvPair c sufy) $ jaVerb聯 decl
  let okuri = if isRequiredOkurigana c decl then sufy else ""
  return $ VerbConversion kys (okuri, okuri ++ "—") 1

extractWordConvPair :: ExtractConfig -> WordConvPair -> Maybe (Kanji, Kana)
extractWordConvPair c (WordConvPair ks p) = do
  k <- kanjiExtractor c ks
  y <- yomiExtractor c $ extractJaPron p
  return (k, y)

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

extractSKK :: ExtractConfig -> [DictEntry] -> SKKDict
extractSKK conf = foldr f SKK.empty . concatMap expandEntry . concatMap (extractConvEntry conf)
    where
      f (a,b,c) = SKK.append a b c

verbConvSuffixes :: JaVerbConjugation -> [JaYomi]
verbConvSuffixes (JaVerbConjugation _ Quadrigrade) = []
verbConvSuffixes (JaVerbConjugation _ Quinquegrade) = []
verbConvSuffixes (JaVerbConjugation StemS IrregularClassic) = []
verbConvSuffixes (JaVerbConjugation StemS IrregularModern) = []
verbConvSuffixes jvc = conjEndings jvc

isRequiredOkurigana :: ExtractConfig -> JaVerbDeclaration -> Bool
isRequiredOkurigana c decl = f $ jaVerb聯 decl
  where
    f [] = error $ "isRequiredOkurigana: No yomi definitions: " ++ show decl
    f [cp] = (yomiExtractor c . extractJaPron $ word讀 cp) /= Just nonOkuriganaMark
    f (_:xs) = f xs

nonOkuriganaMark :: String
nonOkuriganaMark = "$"

