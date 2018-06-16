{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.UHIM.Japanese.Adjective where

import qualified Data.Text as T
import qualified Data.Map as M
import Data.Map (Map)
import Data.Aeson.Types hiding (parse)

import Language.UHIM.Japanese.Prim (JaYomi(..))

data JaAdjConjugation = JaAdjI | JaAdjKu
                      | JaAdjSii | JaAdjSiku
                      | JaAdjZii | JaAdjZiku
                      | JaAdjNa | JaAdjNari
                      | JaAdjTari
    deriving (Eq, Ord, Show, Read)

isClassicalAdjConjugation :: JaAdjConjugation -> Bool
isClassicalAdjConjugation JaAdjKu = True
isClassicalAdjConjugation JaAdjSiku = True
isClassicalAdjConjugation JaAdjZiku = True
isClassicalAdjConjugation JaAdjNari = True
isClassicalAdjConjugation JaAdjTari = True
isClassicalAdjConjugation _ = False

jaAdjClasses :: Map String JaAdjConjugation
jaAdjClasses = M.fromList ls
  where
    ls = [ ("イ",   JaAdjI)
         , ("ク",   JaAdjKu)
         , ("シイ", JaAdjSii)
         , ("シク", JaAdjSiku)
         , ("ジイ", JaAdjZii)
         , ("ジク", JaAdjZiku)
         , ("ナ",   JaAdjNa)
         , ("ナリ", JaAdjNari)
         , ("タリ", JaAdjTari)
         ]

parse :: String -> Maybe JaAdjConjugation
parse = flip M.lookup jaAdjClasses

toSymbol :: JaAdjConjugation -> String
toSymbol JaAdjI = "イ"
toSymbol JaAdjKu = "ク"
toSymbol JaAdjSii = "シイ"
toSymbol JaAdjSiku = "シク"
toSymbol JaAdjZii = "ジイ"
toSymbol JaAdjZiku = "ジク"
toSymbol JaAdjNa = "ナ"
toSymbol JaAdjNari = "ナリ"
toSymbol JaAdjTari = "タリ"

conjDictForm :: JaAdjConjugation -> JaYomi
conjDictForm JaAdjI = NonChange "い"
conjDictForm JaAdjKu = NonChange "し"
conjDictForm JaAdjSii = NonChange "しい"
conjDictForm JaAdjSiku = NonChange "し"
conjDictForm JaAdjZii = NonChange "じい"
conjDictForm JaAdjZiku = NonChange "じ"
-- ToDo: Rethink about endings of those adjective classes
conjDictForm JaAdjNa = mempty
conjDictForm JaAdjNari = mempty
conjDictForm JaAdjTari = mempty

instance ToJSON JaAdjConjugation where
    toJSON jvc = toJSON $ toSymbol jvc

instance FromJSON JaAdjConjugation where
    parseJSON (String v) = maybe (fail . T.unpack $ "Unknown Japanese adjective class:" `mappend` v) pure . parse $ T.unpack v
    parseJSON v          = typeMismatch "JaYomi" v
