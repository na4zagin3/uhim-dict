{-# LANGUAGE OverloadedStrings #-}
module Language.UHIM.Japanese.Transform where

import Data.Semigroup

import Language.UHIM.Japanese.Adjective
import Language.UHIM.Japanese.Verb
import Language.UHIM.Dictionary.Yaml

data Config = Config
  { verbConjugationSelector :: [JaVerbConjugation] -> JaVerbConjugation
  , adjConjugationSelector :: [JaAdjConjugation] -> JaAdjConjugation
  }

-- ToDo: Better default
defaultConfig :: Config
defaultConfig = Config
  { verbConjugationSelector = head
  , adjConjugationSelector = head
  }

appendEnding :: Config -> DictEntry -> DictEntry
appendEnding _ d@(Entry字 _) = d
appendEnding _ d@(Entry語 _) = d
appendEnding c   (Entry日動詞 wd) = Entry日動詞 $ wd {jaVerb聯 = jaVerb聯 wd <> [okurigana (conjDictForm conj)] }
  where
    conj = verbConjugationSelector c $ jaVerb類 wd
appendEnding c d@(Entry日形容詞 wd) = d
appendEnding _ d@(Entry日副詞 _) = d
