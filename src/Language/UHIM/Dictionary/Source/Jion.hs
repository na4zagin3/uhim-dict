{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Language.UHIM.Dictionary.Source.Jion where

import Language.UHIM.Dictionary.Yaml
import Language.UHIM.Japanese.Prim

import Control.Lens
import qualified Data.ByteString.Lazy as BL
import qualified Data.Csv as CSV
import qualified Data.List as L
import qualified Data.Map as M
import Data.Map (Map)
import Data.Vector (Vector)
import qualified Data.Vector as V
import GHC.Generics


data JionEntry = JionEntry
  { shinKana :: String
  , kyuKana :: String
  , goOnKanji :: String
  , goKanOnKanji :: String
  , kanOnKanji :: String
  , kanyoOnKanji :: String
  }
  deriving (Show, Read, Ord, Eq, Generic)

instance CSV.FromRecord JionEntry
instance CSV.ToRecord JionEntry

data Yomi = GoOn ShinKana KyuKana
          | KanOn ShinKana KyuKana
          | KanyoOn ShinKana KyuKana
  deriving (Show, Read, Ord, Eq)

splitKanji :: String -> [String]
splitKanji = map pure

expandEntry :: JionEntry -> Map String [Yomi]
expandEntry ent = M.fromListWith (++) kys
  where
    sk = shinKana ent
    kk = kyuKana ent
    gs = splitKanji $ goOnKanji ent
    gks = splitKanji $ goKanOnKanji ent
    ks = splitKanji $ kanOnKanji ent
    cs = splitKanji $ kanyoOnKanji ent
    kys = concat [ map (\k -> (k, [GoOn sk kk])) $ gs ++ gks
                 , map (\k -> (k, [KanOn sk kk])) $ ks ++ gks
                 , map (\k -> (k, [KanyoOn sk kk])) cs
                 ]

expandDict :: [JionEntry] -> Map String [Yomi]
expandDict = M.unionsWith (++) . map expandEntry

emitDict :: Vector JionEntry -> [DictEntry]
emitDict = map snd . M.toList . M.mapWithKey emitEntry . expandDict . V.toList

emitEntry :: String -> [Yomi] -> DictEntry
emitEntry k ys = Entry字 kd
  where
    ps = emitYomis . L.sort $ ys
    kd = emptyKanjiDeclaration & decl體 .~ ks
                               & decl音 .~ ps
    ks = KanjiShapes (M.singleton jaKanjiKey k)


emitKana :: ShinKana -> KyuKana -> JaYomi
emitKana s k | s == k = NonChange s
             | otherwise = Changed s k

emitYomis :: [Yomi] ->  [Pron]
-- emitYomis [GoOn sg kg, KanOn sk kk] = [emptyPron { pron日漢 = Just $ emitKana sk kk, pron日呉 = Just $ emitKana sg kg}]
emitYomis ys = map emitYomi ys

emitYomi :: Yomi ->  Pron
emitYomi (KanOn s k) = emptyPron { pron日漢 = Just $ emitKana s k}
emitYomi (GoOn s k) = emptyPron { pron日呉 = Just $ emitKana s k}
emitYomi (KanyoOn s k) = emptyPron { pron日慣用 = Just $ emitKana s k}

readDict :: BL.ByteString -> Vector JionEntry
readDict = either undefined id . CSV.decode CSV.HasHeader

readDictFromFile :: FilePath -> IO (Vector JionEntry)
readDictFromFile path = readDict <$> BL.readFile path

