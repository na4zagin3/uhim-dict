{-# LANGUAGE OverloadedStrings #-}
module Language.UHIM.Dictionary.Keybind.TcEl where

import Language.UHIM.Dictionary.Keybind.Vim (Mapping(..))
import Data.String
import Data.Map (Map)
import Data.Tuple
import qualified Data.Map as M

data KeyMap = KeyMap { name :: String
                     , indicator :: String
                     , mappings :: [Mapping]
                     }

escapeStringWithQuote :: (IsString s, Monoid s)=> String -> s
escapeStringWithQuote s= mconcat [ "\""
                                 , escapeString s
                                 , "\""
                                 ]

escapeString :: IsString s => String -> s
escapeString = fromString . concatMap escapeChar
  where
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar x = [x]

escapeSymbol :: IsString s => String -> s
escapeSymbol = fromString . concatMap escapeChar
  where
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar ' ' = "\\ "
    escapeChar x = [x]

keyCodeMap :: Map Char Int
keyCodeMap = M.fromList $ zipWith (curry swap) [0..]
  [ '1', '2', '3', '4', '5', '6', '7', '8', '9', '0'
  , 'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p'
  , 'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l', ';'
  , 'z', 'x', 'c', 'v', 'b', 'n', 'm', ',', '.', '/'
  ]

convertKeyCodeMap :: String -> Maybe [Int]
convertKeyCodeMap = mapM (`M.lookup` keyCodeMap)

convertKeySequence :: (IsString s) => String -> Maybe s
convertKeySequence str = do
  cs <- convertKeyCodeMap str
  return . fromString . unwords . map show $ cs

keyEntry :: (IsString s, Monoid s) => String -> String -> String -> Maybe s
keyEntry k ch com = do
  ks <- convertKeySequence k
  return $ mconcat [ "(("
                   , ks
                   , ") . "
                   , fromString ch
                   , ")"
                   , " ;"
                   , fromString com
                   , "\n"
                   ]

keyEntryStr :: (IsString s, Monoid s) => Mapping -> Maybe s
keyEntryStr m = keyEntry k ch com
  where
    k = key m
    ch = escapeStringWithQuote $ character m
    com = comment m

data Config = Config {}
  deriving (Show, Read, Eq, Ord)

emitKeytable :: (IsString s, Monoid s) => KeyMap -> s
emitKeytable _ = mconcat [ "(setq tcode-tbl (make-vector 40 (make-string 40 ?□)))"
                         , "\n"
                         , "(setq tcode-non-2-stroke-char-list (list (tcode-string-to-char \"□\")))"
                         , "\n"
                         ]



emit :: (IsString s, Monoid s) => KeyMap -> s
emit m = mconcat [ "(require 'tc)"
                   , "\n"
                   , "(setq tcode-input-method '", imSymbol, ")"
                   , "\n"
                   , setq "tcode-transparent-mode-indicator" "------"
                   , "\n"
                   , setq "tcode-alnum-2byte-tcode-mode-indicator" "Ｔ "
                   , "\n"
                   , setq "tcode-hiragana-mode-indicator" " ひ"
                   , "\n"
                   , setq "tcode-katakana-mode-indicator" " カ"
                   , "\n"
                   , setq "tcode-tcode-mode-indicator" imIndicator
                   , "\n"
                   , "(setq tcode-stroke-file-name (concat tcode-data-directory "
                   , imSymbol
                   , " \".st\"))"
                   , "\n"
                   , emitKeytable m
                   , "\n"
                   , "(setq tcode-special-commands-alist '(\n"
                   , defaultKeys
                   , keys
                   , "))\n"
                   ]
  where
    imSymbol :: (IsString s, Monoid s) => s
    imSymbol = escapeStringWithQuote $ name m
    imIndicator = fromString $ indicator m
    Just defaultKeys = mconcat $ map (\(k, v) -> keyEntry k v "") defaultMaps
    Just keys = mconcat $ map keyEntryStr $ mappings m

setq :: (IsString s, Monoid s) => String -> String -> s
setq symb str = mconcat [ "(setq "
                        , escapeSymbol symb
                        , " "
                        , escapeStringWithQuote str
                        , ")"
                        ]


defaultMaps :: [(String, String)]
defaultMaps = [ ("alj", "tcode-mazegaki-begin-conversion")
              , ("09", "tcode-mazegaki-begin-alternate-conversion")
              , ("18", "(lambda () (tcode-mazegaki-convert 1 current-prefix-arg))")
              , ("28", "(lambda () (tcode-mazegaki-convert 2 current-prefix-arg))")
              , ("38", "(lambda () (tcode-mazegaki-convert 3 current-prefix-arg))")
              , ("48", "(lambda () (tcode-mazegaki-convert 4 current-prefix-arg))")
              , ("58", "(lambda () (tcode-mazegaki-convert nil t))")
              , ("29", "(lambda () (tcode-mazegaki-convert 2 t))")
              , ("39", "(lambda () (tcode-mazegaki-convert 3 t))")
              , ("49", "(lambda () (tcode-mazegaki-convert 4 t))")
              , ("59", "(lambda () (tcode-mazegaki-convert 5 t))")
              ]
