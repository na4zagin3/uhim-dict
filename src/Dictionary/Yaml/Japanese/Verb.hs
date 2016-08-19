{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Dictionary.Yaml.Japanese.Verb where

import qualified Data.Text as T
import qualified Data.Map as M
import Data.Map (Map)
import Data.Aeson.Types hiding (parse)

import Dictionary.Yaml.Japanese.Prim

data JaVerbConjugation = JaVerbConjugation JaVerbStem JaVerbClass
    deriving (Eq, Ord, Show, Read)

data JaVerbStem = StemZero
                | StemK | StemG
                | StemS | StemZ
                | StemT | StemD
                | StemN
                | StemF | StemB | StemP
                | StemM
                | StemY
                | StemR
                | StemW
    deriving (Eq, Ord, Show, Read)

data JaVerbClass = Quinquegrade | Quadrigrade | SuperMonograde | SuperBigrade | SubMonograde | SubBigrade | IrregularModern | IrregularClassic
    deriving (Eq, Ord, Show, Read)

jaVerbClasses :: Map String JaVerbConjugation
jaVerbClasses = M.fromList . map (\(x, s, c) -> (x, JaVerbConjugation s c)) $ ls
  where
    ls = [ ("ア四",   StemZero, Quadrigrade)
         , ("ア五",   StemZero, Quinquegrade)
         , ("ア上二", StemZero, SuperBigrade)
         , ("ア上一", StemZero, SuperMonograde)
         , ("ア下二", StemZero, SubBigrade)
         , ("ア下一", StemZero, SubMonograde)

         , ("カ四",   StemK, Quadrigrade)
         , ("カ五",   StemK, Quinquegrade)
         , ("カ上二", StemK, SuperBigrade)
         , ("カ上一", StemK, SuperMonograde)
         , ("カ下二", StemK, SubBigrade)
         , ("カ下一", StemK, SubMonograde)
         , ("カ變",   StemK, IrregularModern)
         , ("カ変",   StemK, IrregularModern)
         , ("古カ變", StemK, IrregularClassic)
         , ("古カ変", StemK, IrregularClassic)

         , ("ガ四",   StemG, Quadrigrade)
         , ("ガ五",   StemG, Quinquegrade)
         , ("ガ上二", StemG, SuperBigrade)
         , ("ガ上一", StemG, SuperMonograde)
         , ("ガ下二", StemG, SubBigrade)
         , ("ガ下一", StemG, SubMonograde)

         , ("サ四",   StemS, Quadrigrade)
         , ("サ五",   StemS, Quinquegrade)
         , ("サ上二", StemS, SuperBigrade)
         , ("サ上一", StemS, SuperMonograde)
         , ("サ下二", StemS, SubBigrade)
         , ("サ下一", StemS, SubMonograde)
         , ("サ變",   StemS, IrregularModern)
         , ("サ変",   StemS, IrregularModern)
         , ("古サ變", StemS, IrregularClassic)
         , ("古サ変", StemS, IrregularClassic)

         , ("ザ四",   StemZ, Quadrigrade)
         , ("ザ五",   StemZ, Quinquegrade)
         , ("ザ上二", StemZ, SuperBigrade)
         , ("ザ上一", StemZ, SuperMonograde)
         , ("ザ下二", StemZ, SubBigrade)
         , ("ザ下一", StemZ, SubMonograde)

         , ("タ四",   StemT, Quadrigrade)
         , ("タ五",   StemT, Quinquegrade)
         , ("タ上二", StemT, SuperBigrade)
         , ("タ上一", StemT, SuperMonograde)
         , ("タ下二", StemT, SubBigrade)
         , ("タ下一", StemT, SubMonograde)

         , ("ダ四",   StemD, Quadrigrade)
         , ("ダ五",   StemD, Quinquegrade)
         , ("ダ上二", StemD, SuperBigrade)
         , ("ダ上一", StemD, SuperMonograde)
         , ("ダ下二", StemD, SubBigrade)
         , ("ダ下一", StemD, SubMonograde)

         , ("ナ四",   StemN, Quadrigrade)
         , ("ナ五",   StemN, Quinquegrade)
         , ("ナ上二", StemN, SuperBigrade)
         , ("ナ上一", StemN, SuperMonograde)
         , ("ナ下二", StemN, SubBigrade)
         , ("ナ下一", StemN, SubMonograde)
         , ("ナ變",   StemN, IrregularModern)
         , ("ナ変",   StemN, IrregularModern)
         , ("古ナ變", StemN, IrregularClassic)
         , ("古ナ変", StemN, IrregularClassic)

         , ("ハ四",   StemF, Quadrigrade)
         , ("ハ五",   StemF, Quinquegrade)
         , ("ハ上二", StemF, SuperBigrade)
         , ("ハ上一", StemF, SuperMonograde)
         , ("ハ下二", StemF, SubBigrade)
         , ("ハ下一", StemF, SubMonograde)

         , ("バ四",   StemB, Quadrigrade)
         , ("バ五",   StemB, Quinquegrade)
         , ("バ上二", StemB, SuperBigrade)
         , ("バ上一", StemB, SuperMonograde)
         , ("バ下二", StemB, SubBigrade)
         , ("バ下一", StemB, SubMonograde)

         , ("パ四",   StemP, Quadrigrade)
         , ("パ五",   StemP, Quinquegrade)
         , ("パ上二", StemP, SuperBigrade)
         , ("パ上一", StemP, SuperMonograde)
         , ("パ下二", StemP, SubBigrade)
         , ("パ下一", StemP, SubMonograde)

         , ("マ四",   StemM, Quadrigrade)
         , ("マ五",   StemM, Quinquegrade)
         , ("マ上二", StemM, SuperBigrade)
         , ("マ上一", StemM, SuperMonograde)
         , ("マ下二", StemM, SubBigrade)
         , ("マ下一", StemM, SubMonograde)

         , ("ヤ四",   StemY, Quadrigrade)
         , ("ヤ五",   StemY, Quinquegrade)
         , ("ヤ上二", StemY, SuperBigrade)
         , ("ヤ上一", StemY, SuperMonograde)
         , ("ヤ下二", StemY, SubBigrade)
         , ("ヤ下一", StemY, SubMonograde)

         , ("ラ四",   StemR, Quadrigrade)
         , ("ラ五",   StemR, Quinquegrade)
         , ("ラ上二", StemR, SuperBigrade)
         , ("ラ上一", StemR, SuperMonograde)
         , ("ラ下二", StemR, SubBigrade)
         , ("ラ下一", StemR, SubMonograde)
         , ("ラ變",   StemR, IrregularModern)
         , ("ラ変",   StemR, IrregularModern)
         , ("古ラ變", StemR, IrregularClassic)
         , ("古ラ変", StemR, IrregularClassic)

         , ("ワ四",   StemW, Quadrigrade)
         , ("ワ五",   StemW, Quinquegrade)
         , ("ワ上二", StemW, SuperBigrade)
         , ("ワ上一", StemW, SuperMonograde)
         , ("ワ下二", StemW, SubBigrade)
         , ("ワ下一", StemW, SubMonograde)
         ]

parse :: String -> Maybe JaVerbConjugation
parse = flip M.lookup jaVerbClasses

stemRepresentive :: JaVerbStem -> String
stemRepresentive StemZero = "ア"
stemRepresentive StemK = "カ"
stemRepresentive StemG = "ガ"
stemRepresentive StemS = "サ"
stemRepresentive StemZ = "ザ"
stemRepresentive StemT = "タ"
stemRepresentive StemD = "ダ"
stemRepresentive StemN = "ナ"
stemRepresentive StemF = "ハ"
stemRepresentive StemB = "バ"
stemRepresentive StemP = "パ"
stemRepresentive StemM = "マ"
stemRepresentive StemY = "ヤ"
stemRepresentive StemR = "ラ"
stemRepresentive StemW = "ワ"

toSymbol :: JaVerbConjugation -> String
toSymbol (JaVerbConjugation s Quadrigrade) = stemRepresentive s ++ "四"
toSymbol (JaVerbConjugation s Quinquegrade) = stemRepresentive s ++ "五"
toSymbol (JaVerbConjugation s SuperBigrade) = stemRepresentive s ++ "上二"
toSymbol (JaVerbConjugation s SuperMonograde) = stemRepresentive s ++ "上一"
toSymbol (JaVerbConjugation s SubBigrade) = stemRepresentive s ++ "下二"
toSymbol (JaVerbConjugation s SubMonograde) = stemRepresentive s ++ "下一"
toSymbol (JaVerbConjugation s IrregularModern) = stemRepresentive s ++ "變"
toSymbol (JaVerbConjugation s IrregularClassic) = "古" ++ stemRepresentive s ++ "變"

instance ToJSON JaVerbConjugation where
    toJSON jvc = toJSON $ toSymbol jvc

instance FromJSON JaVerbConjugation where
    parseJSON (String v) = maybe (fail . T.unpack $ "Unknown Japanese verb class:" `mappend` v) pure . parse $ T.unpack v
    parseJSON v          = typeMismatch "JaYomi" v

conjEnding :: JaVerbStem -> String -> JaYomi
conjEnding StemZero "a" = NonChange "あ"
conjEnding StemZero "i" = NonChange "い"
conjEnding StemZero "u" = NonChange "う"
conjEnding StemZero "e" = NonChange "え"
conjEnding StemZero "o" = NonChange "お"
conjEnding StemK "a" = NonChange "か"
conjEnding StemK "i" = NonChange "き"
conjEnding StemK "u" = NonChange "く"
conjEnding StemK "e" = NonChange "け"
conjEnding StemK "o" = NonChange "こ"
conjEnding StemG "a" = NonChange "が"
conjEnding StemG "i" = NonChange "ぎ"
conjEnding StemG "u" = NonChange "ぐ"
conjEnding StemG "e" = NonChange "げ"
conjEnding StemG "o" = NonChange "ご"
conjEnding StemS "a" = NonChange "さ"
conjEnding StemS "i" = NonChange "し"
conjEnding StemS "u" = NonChange "す"
conjEnding StemS "e" = NonChange "せ"
conjEnding StemS "o" = NonChange "そ"
conjEnding StemZ "a" = NonChange "ざ"
conjEnding StemZ "i" = NonChange "じ"
conjEnding StemZ "u" = NonChange "ず"
conjEnding StemZ "e" = NonChange "ぜ"
conjEnding StemZ "o" = NonChange "ぞ"
conjEnding StemT "a" = NonChange "た"
conjEnding StemT "i" = NonChange "ち"
conjEnding StemT "u" = NonChange "つ"
conjEnding StemT "e" = NonChange "て"
conjEnding StemT "o" = NonChange "と"
conjEnding StemD "a" = NonChange "だ"
conjEnding StemD "i" = NonChange "ぢ"
conjEnding StemD "u" = NonChange "づ"
conjEnding StemD "e" = NonChange "で"
conjEnding StemD "o" = NonChange "ど"
conjEnding StemN "a" = NonChange "な"
conjEnding StemN "i" = NonChange "に"
conjEnding StemN "u" = NonChange "ぬ"
conjEnding StemN "e" = NonChange "ね"
conjEnding StemN "o" = NonChange "の"
conjEnding StemF "a" = Changed "わ" "は"
conjEnding StemF "i" = Changed "い" "ひ"
conjEnding StemF "u" = Changed "う" "ふ"
conjEnding StemF "e" = Changed "え" "へ"
conjEnding StemF "o" = Changed "お" "ほ"
conjEnding StemB "a" = NonChange "ば"
conjEnding StemB "i" = NonChange "び"
conjEnding StemB "u" = NonChange "ぶ"
conjEnding StemB "e" = NonChange "べ"
conjEnding StemB "o" = NonChange "ぼ"
conjEnding StemP "a" = NonChange "ぱ"
conjEnding StemP "i" = NonChange "ぴ"
conjEnding StemP "u" = NonChange "ぷ"
conjEnding StemP "e" = NonChange "ぺ"
conjEnding StemP "o" = NonChange "ぽ"
conjEnding StemM "a" = NonChange "ま"
conjEnding StemM "i" = NonChange "み"
conjEnding StemM "u" = NonChange "む"
conjEnding StemM "e" = NonChange "め"
conjEnding StemM "o" = NonChange "も"
conjEnding StemY "a" = NonChange "や"
conjEnding StemY "i" = NonChange "い"
conjEnding StemY "u" = NonChange "ゆ"
conjEnding StemY "e" = Changed "え" "\x1b001"
conjEnding StemY "o" = NonChange "よ"
conjEnding StemR "a" = NonChange "ら"
conjEnding StemR "i" = NonChange "り"
conjEnding StemR "u" = NonChange "る"
conjEnding StemR "e" = NonChange "れ"
conjEnding StemR "o" = NonChange "ろ"
conjEnding StemW "a" = NonChange "わ"
conjEnding StemW "i" = Changed "い" "ゐ"
conjEnding StemW "u" = NonChange "う"
conjEnding StemW "e" = Changed "え" "ゑ"
conjEnding StemW "o" = Changed "お" "を"
conjEnding s v = error $ "conjEnding: Unknown Stem (" ++ show s ++ ") and vowel (" ++ v ++ ")"

conjSuffixes :: JaVerbClass -> [String]
conjSuffixes Quadrigrade = ["a", "i", "u", "e"]
conjSuffixes Quinquegrade = ["a", "i", "u", "e", "o"]
conjSuffixes SuperBigrade = ["i", "u"]
conjSuffixes SuperMonograde = ["i"]
conjSuffixes SubBigrade = ["e", "u"]
conjSuffixes SubMonograde = ["e"]
conjSuffixes c = error $ "conjSuffixes: Unknown Verb Class " ++ show c

-- TODO Implement irregular classes
conjEndings :: JaVerbStem -> JaVerbClass -> [JaYomi]
conjEndings s c@IrregularModern = error $ "conjEndings: Unknown Verb Class " ++ show s ++ " " ++ show c
conjEndings s c@IrregularClassic = error $ "conjEndings: Unknown Verb Class " ++ show s ++ " " ++ show c
conjEndings s c = map (conjEnding s) $ conjSuffixes c
