module Language.UHIM.Dictionary.SKK.SKKExtended ( SKKDict , SKKEntry
                              , Config(..), defaultConfig
                              , empty
                              , union
                              , append , append'
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

-- |Empty SKK Dictionary.
empty :: SKKDict
empty = M.empty

-- |Append the new entry into the dictionary. It does not allow an idempotent entry
append :: Kana -> Kanji -> Frequency -> SKKDict -> SKKDict
append yomi kanji freq | yomi /= kanji = append' yomi kanji freq
                       | otherwise = id

-- |Append the new entry into the dictionary.
append' :: Kana -> Kanji -> Frequency -> SKKDict -> SKKDict
append' yomi kanji freq = M.insertWith unionEntry yomi (M.singleton kanji freq)

union :: SKKDict -> SKKDict -> SKKDict
union = M.unionWith unionEntry

unionEntry :: SKKEntry -> SKKEntry -> SKKEntry
unionEntry = M.unionWith max -- ToDo: Frequency or Priority?

data Config = Config { outputFrequency :: Bool
                     }

defaultConfig :: Config
defaultConfig = Config { outputFrequency = False }

-- |Convert dictionary to string.
emitSKKDictionary :: Config -> SKKDict -> String
emitSKKDictionary c = unlines . map f . M.toAscList
    where
      f (yomi, ks) = mconcat [yomi, " ", emitSKKEntry c ks]

emitSKKEntry :: Config -> SKKEntry -> String
emitSKKEntry c = (\x -> "/" ++ x ++ "/") . L.intercalate "/" . concatMap (emitSKKCandidate c). L.sortOn snd . M.toList

emitSKKCandidate :: Config -> (Kanji, Frequency) -> [String]
emitSKKCandidate c (k, f) = return $ escapeSKKCandidate k ++ comments
  where
    comments | outputFrequency c = "; " ++ show f
             | otherwise = ""

escapeSKKCandidate :: String -> String
escapeSKKCandidate s | any (`elem` "/();") s = error $ "escapeSKKCandidate: Not yet supported string: " ++ show s
                     | otherwise            = s
