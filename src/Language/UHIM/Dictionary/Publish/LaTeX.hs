{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
module Language.UHIM.Dictionary.Publish.LaTeX where

import Language.UHIM.Japanese.Prim
import Language.UHIM.Japanese.Verb as JV
import Language.UHIM.Japanese.Adjective as JA
import Language.UHIM.Dictionary.Yaml
import Data.Either
import Data.String
import Data.List
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Set as S
import Text.Parsec
import qualified Text.Parsec as P

import Control.Arrow

emitHeadKanji :: ShapeClass -> String -> String
emitHeadKanji JaCommon k = "\\HeadKanjiJaCommon" ++ "{" ++ escape k ++ "}"
emitHeadKanji (JaTrad []) k = "\\HeadKanjiJaTrad" ++ "{" ++ escape k ++ "}"
emitHeadKanji (JaTrad v) k = "\\HeadKanjiJaTrad" ++ "[" ++ escape v ++ "]" ++ "{" ++ escape k ++ "}"
emitHeadKanji (JaSimp []) k = "\\HeadKanjiJaSimp" ++ "{" ++ escape k ++ "}"
emitHeadKanji (JaSimp v) k = "\\HeadKanjiJaSimp" ++ "[" ++ escape v ++ "]" ++ "{" ++ escape k ++ "}"
emitHeadKanji (Other v) k = "\\HeadKanjiOther" ++ "[" ++ escape v ++ "]" ++ "{" ++ escape k ++ "}"

emitHeadKanjiShapes :: (IsString s) => KanjiShapes -> s
emitHeadKanjiShapes (KanjiShapes ks) = fromString . unwords $ map (uncurry emitHeadKanji) maps
  where
    maps :: [(ShapeClass, String)]
    maps = sort . map (first readShapeKey) $ M.toList ks

headWordKanji :: (IsString s) => (JaYomi -> Maybe Kana) -> (KanjiShapes -> Maybe Kanji) -> WordConvPair -> s
headWordKanji extYomi extKanji kp = fromString . f kanji . extractYomi . extractJaProns $ word讀 kp
  where
    kanji = fromMaybe "" $ extKanji $ word字 kp
    f k Nothing = k
    f "$$" (Just y) = y
    f k (Just y) = "\\ruby{" ++ escape k ++ "}{" ++ escape y ++ "}"
    extractYomi [x] = extYomi x
    extractYomi xs = error $ "headWordKanji: Currently, only one pronunciation is allowed; but got: " ++ show kp ++ ", " ++ show xs

kanjiYomiElem :: (IsString s, Monoid s) => (String, JaYomi) -> s
kanjiYomiElem (key, NonChange y) = mconcat [ "\\KanjiYomiElem{"
                                           , escape key
                                           , "}{"
                                           , escape y
                                           , "}"
                                           ]
kanjiYomiElem (key, Changed n t) = mconcat [ "\\KanjiYomiElem{"
                                           , escape key
                                           , "}["
                                           , escape n
                                           , "]{"
                                           , escape t
                                           , "}"
                                           ]

kanjiYomi :: (IsString s, Monoid s, Eq s) => Config -> [Pron] -> s
kanjiYomi _ [] = ""
kanjiYomi c ps = if content == mempty
  then mempty
  else mconcat [ "\\begin{Yomi}\n"
               , content
               , "\\end{Yomi}"
               ]
  where
    f :: (IsString s, Monoid s) => Pron -> s
    f = mconcat . intersperse (fromString $ yomiSeparator c) . map kanjiYomiElem . extractJaPronList
    g :: (IsString s, Monoid s, Eq s) => Pron -> s
    g pron = case f pron of
      x | x == mempty -> mempty
      x | otherwise   -> mconcat ["\\YomiUnit{", x, "}\n"]
    content = mconcat $ map g ps

meaning :: (IsString s, Monoid s) => Config -> [String] -> s
meaning _ [] = ""
meaning _ [s] = mconcat [ "\\begin{MeaningSg}\n"
                        , escape s
                        , "\\end{MeaningSg}\n"
                        ]
meaning _ ss = mconcat [ "\\begin{MeaningPl}\n"
                       , mconcat $ map (\s -> mconcat ["\\Meaning{", escape s, "}\n"]) ss
                       , "\\end{MeaningPl}\n"
                       ]

emitFrequency :: (IsString s, Monoid s) => Config -> Maybe Double -> s
emitFrequency _ Nothing = ""
emitFrequency _ (Just f) = mconcat ["\\Frequency{", fromString $ show f, "}\n"]

tags :: (IsString s, Monoid s) => Config -> [String] -> s
tags _ [] = ""
tags _ ss = mconcat [ "\\begin{Tags}\n"
                    , mconcat $ map (\s -> mconcat ["\\Tag{", escape s, "}\n"]) ss
                    , "\\end{Tags}\n"
                    ]

emitKeys :: (IsString s, Monoid s) => Config -> (Map String String) -> s
emitKeys _ ks | M.null ks = ""
emitKeys _ ks = mconcat
  [ "\\begin{Keys}\n"
  , mconcat $ map (\(t, s) -> mconcat ["\\Key{", escape t, "}{", escapeWithVisibleSpace True s, "}%\n"]) $ M.toList ks
  , "\\end{Keys}\n"
  ]

emitShapes :: (IsString s, Monoid s) => Config -> Map String String -> s
emitShapes _ ks | M.null ks = ""
emitShapes _ ks = mconcat
  [ "\\begin{Shapes}\n"
  , mconcat $ map (\(t, s) -> mconcat ["\\Shape{", escape t, "}{", escape s, "}%\n"]) $ M.toList ks
  , "\\end{Shapes}\n"
  ]


headWord :: (IsString s, Eq s) => String -> [WordConvPair] -> [s]
headWord c wcp = concat [ ["\\HeadWord[", escape c, "]{"]
                      , trad
                      , ["}"]
                      , if trad /= simp
                        then ["\\HeadWordVariant[JaSimp]{"] ++ simp ++ ["}"]
                        else []
                      ]
  where
    trad = map (headWordKanji extractExtKyuKana extractKyuKanji) wcp
    simp = map (headWordKanji extractShinKana extractShinKanji) wcp
    maps :: [(ShapeClass, String)]
    maps = sort . map (first readShapeKey) $ M.toList undefined

verbConj :: (IsString s, Monoid s) => [JaVerbConjugation] -> s
verbConj cs = mconcat [ "\\JaVerbConj{"
                      , mconcat $ intersperse "・" symbs
                      , "}"
                      ]
  where
    symbs = map (fromString . JV.toSymbol) cs

adjConj :: (IsString s, Monoid s) => [JaAdjConjugation] -> s
adjConj cs = mconcat [ "\\JaAdvConj{"
                      , mconcat $ intersperse "・" symbs
                      , "}"
                      ]
  where
    symbs = map (fromString . JA.toSymbol) cs

emitPosition :: (IsString s, Monoid s, Eq s) => Position -> s
emitPosition = mconcat . mconcat . map (\(f,p) -> [ "\\Position{", escape f, "}{", escape $ show p, "}"])

emitEntry :: (IsString s, Monoid s, Eq s) => Config -> (Position, DictEntry) -> s
emitEntry c (pos, d@(Entry字 decl)) = mconcat
  [ emitPosition pos
  , "%\n"
  , emitHeadKanjiShapes $ kanji體 decl
  , "%\n"
  , emitFrequency c $ frequency d
  , "%\n"
  , fromMaybe "" (emitShapes c <$> kanji形 decl)
  , "%\n"
  , fromMaybe "" (emitKeys c <$> kanji鍵 decl)
  , "%\n"
  , kanjiYomi c $ kanji音 decl
  , "%\n"
  , fromMaybe "" (meaning c <$> kanji義 decl)
  , "%\n"
  , fromMaybe "" (tags c <$> entryLabel d)
  ]

emitEntry c (pos, d@(Entry語 decl)) = mconcat
  [ emitPosition pos
  , "\n"
  , mconcat $ headWord "語" $ word聯 decl
  , "\n"
  , emitFrequency c $ frequency d
  , "\n"
  , fromMaybe "" (meaning c <$> word義 decl)
  , "\n"
  , fromMaybe "" (tags c <$> entryLabel d)
  ]

emitEntry c (pos, d@(Entry日副詞 decl)) = mconcat
  [ emitPosition pos
  , "\n"
  , mconcat $ headWord "日副詞" $ word聯 decl
  , "\n"
  , emitFrequency c $ frequency d
  , "\n"
  , fromMaybe "" (meaning c <$> word義 decl)
  , "\n"
  , fromMaybe "" (tags c <$> entryLabel d)
  ]

emitEntry c (pos, d@(Entry日動詞 decl)) = mconcat
  [ emitPosition pos
  , "\n"
  , mconcat $ headWord "日動詞" $ jaVerb聯 decl
  , "\n"
  , verbConj $ jaVerb類 decl
  , "\n"
  , emitFrequency c $ frequency d
  , "\n"
  , fromMaybe "" (meaning c <$> jaVerb義 decl)
  , "\n"
  , fromMaybe "" (tags c <$> entryLabel d)
  ]

emitEntry c (pos, d@(Entry日形容詞 decl)) = mconcat
  [ emitPosition pos
  , "\n"
  , mconcat $ headWord "日形容詞" $ jaAdj聯 decl
  , "\n"
  , adjConj $ jaAdj類 decl
  , "\n"
  , emitFrequency c $ frequency d
  , "\n"
  , fromMaybe "" (meaning c <$> jaAdj義 decl)
  , "\n"
  , fromMaybe "" (tags c <$> entryLabel d)
  ]

emitFilePaths :: (IsString s, Monoid s, Eq s) => [FilePath] -> s
emitFilePaths fps = mconcat $ intersperse "\n" $ map f fps
  where
    f fp = mconcat [ "\\UhimPosition{"
                   , fromString fp
                   , "}"
                   ]

data Config = Config { template :: String
                     , multicols :: (String, String)
                     , yomiSeparator :: String
                     }

defaultConfig :: Config
defaultConfig = Config { template = ""
                       , multicols = ("\\begin{multicols*}{5}\n", "\\end{multicols*}\n")
                       , yomiSeparator = "\\\\"
                       }

emitDict :: (IsString s, Monoid s, Eq s) => Config -> Dictionary -> [s]
emitDict c ds  = [ fromString $ template c
                 , "\n"
                 , "\\begin{document}\n"
                 , fromString . fst $ multicols c
                 , emitFilePaths . S.toList . S.fromList . map fst $ concatMap fst ds
                 , mconcat $ map (entryEnv . emitEntry c) ds
                 , "\n"
                 , fromString . snd $ multicols c
                 , "\\end{document}"
                 ]
  where
    entryEnv s = mconcat [ "\\begin{Entry}\n"
                         , s
                         ,  "\\end{Entry}\n"
                         ]

escapeChar :: (Stream s m Char) => Bool -> ParsecT s u m String
escapeChar escapeSpace = (oneOf "$%#{}&^" >>= \c -> return ("\\" ++ [c])) <|> (char '\\' >> return "\\textbackslash ") <|> escSpace
  where
    escSpace | escapeSpace = char ' ' >> return "\\textvisiblespace "
             | not escapeSpace = parserZero

escapeHandakuten :: (Stream s m Char) => ParsecT s u m String
escapeHandakuten = try $ do
  c <- noneOf "ハヒフヘホ"
  _ <- string "\x309a"
  return $ "{\\bou{" ++ [c] ++ "}}"

escapeString :: (Stream s m Char) => Bool ->ParsecT s u m String
escapeString escapeSpace = fmap concat . many $ choice
  [ escapeHandakuten
  , escapeChar escapeSpace
  , anyChar >>= \c -> return [c]
  ]

escapeWithVisibleSpace :: (IsString s) => Bool -> String -> s
escapeWithVisibleSpace escapeSpace s = fromString . either (error . show) id $ P.parse (escapeString escapeSpace) "LaTeX:escape" s

escape :: (IsString s) => String -> s
escape = escapeWithVisibleSpace False
