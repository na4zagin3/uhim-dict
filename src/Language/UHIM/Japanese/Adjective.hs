{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.UHIM.Japanese.Adjective where

import qualified Data.Text as T
import qualified Data.Map as M
import Data.Map (Map)
import Data.Aeson.Types hiding (parse)

data JaAdjConjugation = JaAdjI | JaAdjKu
                      | JaAdjSii | JaAdjSiku
                      | JaAdjZii | JaAdjZiku
                      | JaAdjNa | JaAdjNari
                      | JaAdjTari
    deriving (Eq, Ord, Show, Read)

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

instance ToJSON JaAdjConjugation where
    toJSON jvc = toJSON $ toSymbol jvc

instance FromJSON JaAdjConjugation where
    parseJSON (String v) = maybe (fail . T.unpack $ "Unknown Japanese adjective class:" `mappend` v) pure . parse $ T.unpack v
    parseJSON v          = typeMismatch "JaYomi" v
