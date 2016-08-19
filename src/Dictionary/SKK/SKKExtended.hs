module Dictionary.SKK.SKKExtended ( SKKDict , SKKEntry
                              , empty
                              , append
                              , emitSKKDictionary
                              ) where

import qualified Data.Map as M
import Data.Map (Map)
import qualified Data.List as L
-- import Data.Monoid

type Kana = String
type Kanji = String
type Frequency = Double

type SKKEntry = Map String Frequency
type SKKDict = Map String SKKEntry

empty :: SKKDict
empty = M.empty

append :: Kana -> Kanji -> Frequency -> SKKDict -> SKKDict
append yomi kanji freq = M.insertWith g yomi (M.singleton kanji freq)
    where
      g = M.unionWith max -- ToDo: Frequency or Priority?

emitSKKDictionary :: SKKDict -> String
emitSKKDictionary = unlines . map f . M.toAscList
    where
      f (yomi, ks) = mconcat [yomi, "\t", emitSKKEntry ks]

emitSKKEntry :: SKKEntry -> String
emitSKKEntry = (\x -> "/" ++ x ++ "/") . L.intercalate "/" . concatMap emitSKKCandidate . L.sortOn snd . M.toList

emitSKKCandidate :: (Kanji, Frequency) -> [String]
emitSKKCandidate (k, f) = return $ escapeSKKCandidate k ++ "; " ++ show f

escapeSKKCandidate :: String -> String
escapeSKKCandidate s | any (`elem` "/();") s = error $ "escapeSKKCandidate: Not yet supported string: " ++ show s
                     | otherwise            = s
