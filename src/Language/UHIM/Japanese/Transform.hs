{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.UHIM.Japanese.Transform where

import Control.Lens
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
appendEnding c   (Entry日動詞 wd) = Entry日動詞 $ wd & decl聯 %~ (<> [okurigana okuri])
  where
    conj = verbConjugationSelector c $ wd^.decl類
    okuri = Verb.conjDictForm conj
appendEnding c   (Entry日形容詞 wd) = Entry日形容詞 $ wd & decl聯 %~ (<> [okurigana okuri])
  where
    conj = adjConjugationSelector c $ wd^.decl類
    okuri = Adj.conjDictForm conj
appendEnding _ d@(Entry日副詞 _) = d
