module Language.UHIM.Japanese.Collator where

-- ToDo: Replace this class by Collator in text-icu

import qualified Data.Map as M
import Data.Maybe

type CollationTuple = (Int, Int, Int, Int)
newtype CollationVector = CollationVector ([Int], [Int], [Int], [Int])
  deriving (Show, Ord, Eq)

data Collator = Collator
  { collationMap :: M.Map String CollationTuple
  , tokenizeString :: M.Map String [String]
  }

hiraganaLarge n = (n, 1, 1, 1)
hiraganaSmall n = (n, 1, 2, 2)
katakanaLarge n = (n, 1, 1, 2)
katakanaSmall n = (n, 1, 2, 2)

japaneseCollator kaYe = Collator
  { collationMap = M.fromList
      [ ("\x3099", (0, 2, 0, 0))
      , ("\x309A", (0, 3, 0, 0))
      , ("あ", hiraganaLarge 11)
      , ("ぁ", hiraganaSmall 11)
      , ("ア", katakanaLarge 11)
      , ("ァ", hiraganaSmall 11)
      , ("い", hiraganaLarge 12)
      , ("ぃ", hiraganaSmall 12)
      , ("イ", katakanaLarge 12)
      , ("ィ", hiraganaSmall 12)
      , ("う", hiraganaLarge 13)
      , ("ぅ", hiraganaSmall 13)
      , ("ウ", katakanaLarge 13)
      , ("ゥ", hiraganaSmall 13)
      , ("え", hiraganaLarge 14)
      , ("ぇ", hiraganaSmall 14)
      , ("エ", katakanaLarge 14)
      , ("ェ", hiraganaSmall 14)
      , ("お", hiraganaLarge 15)
      , ("ぉ", hiraganaSmall 15)
      , ("オ", katakanaLarge 15)
      , ("ォ", hiraganaSmall 15)

      , ("か", hiraganaLarge 21)
      , ("カ", katakanaLarge 21)
      , ("ヵ", katakanaSmall 21)
      , ("き", hiraganaLarge 22)
      , ("キ", katakanaLarge 22)
      , ("く", hiraganaLarge 23)
      , ("ク", katakanaLarge 23)
      , ("け", hiraganaLarge 24)
      , ("ケ", katakanaLarge 24)
      , ("ヶ", katakanaSmall 24)
      , ("こ", hiraganaLarge 25)
      , ("コ", katakanaLarge 25)

      , ("さ", hiraganaLarge 31)
      , ("サ", katakanaLarge 31)
      , ("し", hiraganaLarge 32)
      , ("シ", katakanaLarge 32)
      , ("す", hiraganaLarge 33)
      , ("ス", katakanaLarge 33)
      , ("せ", hiraganaLarge 34)
      , ("セ", katakanaLarge 34)
      , ("そ", hiraganaLarge 35)
      , ("ソ", katakanaLarge 35)

      , ("た", hiraganaLarge 41)
      , ("タ", katakanaLarge 41)
      , ("ち", hiraganaLarge 42)
      , ("チ", katakanaLarge 42)
      , ("つ", hiraganaLarge 43)
      , ("っ", hiraganaSmall 43)
      , ("ツ", katakanaLarge 43)
      , ("ッ", katakanaSmall 43)
      , ("て", hiraganaLarge 44)
      , ("テ", katakanaLarge 44)
      , ("と", hiraganaLarge 45)
      , ("ト", katakanaLarge 45)

      , ("な", hiraganaLarge 51)
      , ("ナ", katakanaLarge 51)
      , ("に", hiraganaLarge 52)
      , ("ニ", katakanaLarge 52)
      , ("ぬ", hiraganaLarge 53)
      , ("ヌ", katakanaLarge 53)
      , ("ね", hiraganaLarge 54)
      , ("ネ", katakanaLarge 54)
      , ("の", hiraganaLarge 55)
      , ("ノ", katakanaLarge 55)

      , ("は", hiraganaLarge 61)
      , ("ハ", katakanaLarge 61)
      , ("ひ", hiraganaLarge 62)
      , ("ヒ", katakanaLarge 62)
      , ("ふ", hiraganaLarge 63)
      , ("フ", katakanaLarge 63)
      , ("へ", hiraganaLarge 64)
      , ("ヘ", katakanaLarge 64)
      , ("ほ", hiraganaLarge 65)
      , ("ホ", katakanaLarge 65)

      , ("ま", hiraganaLarge 71)
      , ("マ", katakanaLarge 71)
      , ("み", hiraganaLarge 72)
      , ("ミ", katakanaLarge 72)
      , ("む", hiraganaLarge 73)
      , ("ム", katakanaLarge 73)
      , ("め", hiraganaLarge 74)
      , ("メ", katakanaLarge 74)
      , ("も", hiraganaLarge 75)
      , ("モ", katakanaLarge 75)

      , ("や", hiraganaLarge 81)
      , ("ゃ", hiraganaSmall 81)
      , ("ヤ", katakanaLarge 81)
      , ("ャ", katakanaSmall 81)
      , ("ゆ", hiraganaLarge 82)
      , ("ゅ", hiraganaSmall 82)
      , ("ユ", katakanaLarge 82)
      , ("ュ", katakanaSmall 82)
      , ("\x01b001", katakanaLarge 84)
      , ("よ", hiraganaLarge 85)
      , ("ょ", hiraganaSmall 85)
      , ("ヨ", katakanaLarge 85)
      , ("ョ", katakanaSmall 85)

      , ("ら", hiraganaLarge 91)
      , ("ラ", katakanaLarge 91)
      , ("り", hiraganaLarge 92)
      , ("リ", katakanaLarge 92)
      , ("る", hiraganaLarge 93)
      , ("ル", katakanaLarge 93)
      , ("れ", hiraganaLarge 94)
      , ("レ", katakanaLarge 94)
      , ("ろ", hiraganaLarge 95)
      , ("ロ", katakanaLarge 95)

      , ("わ", hiraganaLarge 101)
      , ("ゎ", hiraganaSmall 101)
      , ("ワ", katakanaLarge 101)
      , ("ヮ", katakanaSmall 101)
      , ("ゐ", hiraganaLarge 102)
      , ("ヰ", katakanaLarge 102)
      , ("ゑ", hiraganaLarge 104)
      , ("ヱ", katakanaLarge 104)
      , ("を", hiraganaLarge 105)
      , ("ヲ", katakanaLarge 105)
      ]
  , tokenizeString = M.fromList
      [ ("が", ["か", "\x3099"])
      , ("ガ", ["カ", "\x3099"])
      , ("ぎ", ["き", "\x3099"])
      , ("ギ", ["キ", "\x3099"])
      , ("ぐ", ["く", "\x3099"])
      , ("グ", ["ク", "\x3099"])
      , ("げ", ["け", "\x3099"])
      , ("ゲ", ["ケ", "\x3099"])
      , ("ご", ["こ", "\x3099"])
      , ("ゴ", ["コ", "\x3099"])
      , ("ざ", ["さ", "\x3099"])
      , ("ザ", ["サ", "\x3099"])
      , ("じ", ["し", "\x3099"])
      , ("ジ", ["シ", "\x3099"])
      , ("ず", ["す", "\x3099"])
      , ("ズ", ["ス", "\x3099"])
      , ("ぜ", ["せ", "\x3099"])
      , ("ゼ", ["セ", "\x3099"])
      , ("ぞ", ["そ", "\x3099"])
      , ("ゾ", ["ソ", "\x3099"])
      , ("だ", ["た", "\x3099"])
      , ("ダ", ["タ", "\x3099"])
      , ("ぢ", ["ち", "\x3099"])
      , ("ヂ", ["チ", "\x3099"])
      , ("づ", ["つ", "\x3099"])
      , ("ヅ", ["ツ", "\x3099"])
      , ("で", ["て", "\x3099"])
      , ("デ", ["テ", "\x3099"])
      , ("ど", ["と", "\x3099"])
      , ("ド", ["ト", "\x3099"])
      , ("ば", ["は", "\x3099"])
      , ("ぱ", ["は", "\x309A"])
      , ("バ", ["ハ", "\x3099"])
      , ("パ", ["ハ", "\x309A"])
      , ("び", ["ひ", "\x3099"])
      , ("ぴ", ["ひ", "\x309A"])
      , ("ビ", ["ヒ", "\x3099"])
      , ("ピ", ["ヒ", "\x309A"])
      , ("ぶ", ["ふ", "\x3099"])
      , ("ぷ", ["ふ", "\x309A"])
      , ("ブ", ["フ", "\x3099"])
      , ("プ", ["フ", "\x309A"])
      , ("べ", ["へ", "\x3099"])
      , ("ぺ", ["へ", "\x309A"])
      , ("ベ", ["ヘ", "\x3099"])
      , ("ペ", ["ヘ", "\x309A"])
      , ("ぼ", ["ほ", "\x3099"])
      , ("ぽ", ["ほ", "\x309A"])
      , ("ボ", ["ホ", "\x3099"])
      , ("ポ", ["ホ", "\x309A"])
      ]
  }

generateCollationOrder :: [[[[String]]]] -> M.Map String CollationTuple
generateCollationOrder l = M.fromList $ do
  (i1, l') <- zip [0..] l
  (i2, l'') <- zip [0..] l'
  (i3, l''') <- zip [0..] l''
  (i4, c) <- zip [0..] l'''
  return (c, (i1, i2, i3, i4))

collationString :: [CollationTuple] -> CollationVector
collationString = CollationVector . foldr f ([], [], [], [])
  where
    f (a, b, c, d) (as, bs, cs, ds) = (g a as, g b bs, g c cs, g d ds)
    g 0 xs = xs
    g i xs = i:xs

collationVector :: Collator -> String -> CollationVector
collationVector c = cs . tokenize
  where
    cMap = collationMap c
    tMap = tokenizeString c
    tokenize :: String -> [String]
    tokenize = concatMap (\x -> fromMaybe [[x]] $ M.lookup [x] tMap)
    cs = collationString . map (fromMaybe (0, 0, 0, 0) . (`M.lookup` cMap))

-- |
-- Collate strings.
--
-- >>> import qualified Data.List as L
-- >>> collate (japaneseCollator False) "あ" "あ"
-- EQ
-- >>> collate (japaneseCollator False) "あ" "ア"
-- LT
-- >>> L.sortBy (collate (japaneseCollator False)) ["あ", "ア", "あか", "は", "はあ", "はぁ", "はか", "はが", "ばか", "バカ", "ぱか"] == ["あ", "ア", "あか", "は", "はあ", "はぁ", "はか", "はが", "ばか", "バカ", "ぱか"]
-- True
collate :: Collator -> String -> String -> Ordering
collate c s1 s2 = compare c1 c2
  where
    cMap = collationMap c
    tMap = tokenizeString c
    tokenized1 = tokenize s1
    tokenized2 = tokenize s2
    tokenize :: String -> [String]
    tokenize = concatMap (\x -> fromMaybe [[x]] $ M.lookup [x] tMap)
    c1 = cs tokenized1
    c2 = cs tokenized2
    cs = collationString . map (fromMaybe (0, 0, 0, 0) . (`M.lookup` cMap))
