{-# LANGUAGE OverloadedStrings #-}
module Language.UHIM.Japanese.Transform where

import Data.List (sortBy)
import Data.Ord (Down(..), comparing)
import Data.Semigroup

import Language.UHIM.Japanese.Adjective (JaAdjConjugation(..))
import qualified Language.UHIM.Japanese.Adjective as Adj
import Language.UHIM.Japanese.Verb (JaVerbConjugation(..))
import qualified Language.UHIM.Japanese.Verb as Verb
import Language.UHIM.Dictionary.Yaml

data Config = Config
  { verbConjugationSelector :: [JaVerbConjugation] -> JaVerbConjugation
  , adjConjugationSelector :: [JaAdjConjugation] -> JaAdjConjugation
  }

stableReorderBy :: (a -> Bool) -> [a] -> [a]
stableReorderBy p xs = filter p xs <> filter (not . p) xs

-- ToDo: Better default
defaultConfig :: Config
defaultConfig = Config
  { verbConjugationSelector = head . stableReorderBy Verb.isClassicalVerbConjugation
  , adjConjugationSelector = head . stableReorderBy Adj.isClassicalAdjConjugation
  }

appendEnding :: Config -> DictEntry -> DictEntry
appendEnding _ d@(Entry字 _) = d
appendEnding _ d@(Entry語 _) = d
appendEnding c   (Entry日動詞 wd) = Entry日動詞 $ wd {jaVerb聯 = jaVerb聯 wd <> [okurigana (Verb.conjDictForm conj)] }
  where
    conj = verbConjugationSelector c $ jaVerb類 wd
appendEnding c   (Entry日形容詞 wd) = Entry日形容詞 $ wd {jaAdj聯 = jaAdj聯 wd <> [okurigana (Adj.conjDictForm conj)] }
  where
    conj = adjConjugationSelector c $ jaAdj類 wd
appendEnding _ d@(Entry日副詞 _) = d
