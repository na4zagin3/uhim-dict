{-# Language OverloadedStrings #-}
module Language.UHIM.Japanese.CollatorSpec where

import Control.Monad.IO.Class
import Data.String
import qualified Data.Text.ICU.Collate as Col

import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Language.UHIM.Japanese.Collator

hiragana :: String
hiragana = "あぁいぃうぅえぇおぉかがきぎくぐけげこごさざしじすずせぜそぞただちぢつづってでとどなにぬねのはばぱひびぴふぶぷへべぺほぼぽまみむめもやゃゆゅよょらりるれろわゎゐゑを"

katakana :: String
katakana = "アァイィウゥエェオォカガキギクグケゲコゴサザシジスズセゼソゾタダチヂツヅッテデトドナニヌネノハバパヒビピフブプヘベペホボポマミムメモヤャユュヨョラリルレロワヮヰヱヲヵヶ"

special :: [String]
special = ["ウ゚", "う゚", "イ゚", "い゚"]

genKana :: H.MonadGen m => m String
genKana = Gen.element $ hs ++ ks ++ special
  where
    hs = map (\x -> [x]) hiragana
    ks = map (\x -> [x]) katakana

genKanaList :: H.MonadGen m => m String
genKanaList = do
  kanas <- Gen.list (Range.linear 0 500) genKana
  return $ concat kanas

showKanaList :: Collator -> String -> String
showKanaList c s = mconcat [show s, ": ", s, ": ", show $ collationVector c s]

hprop_collation_reflexive :: H.Property
hprop_collation_reflexive = H.property $ do
  let jcol = japaneseCollator False
  s <- H.forAllWith (showKanaList jcol) $ genKanaList
  collate jcol s s H.=== EQ

inverseOrdering :: Ordering -> Ordering
inverseOrdering EQ = EQ
inverseOrdering LT = GT
inverseOrdering GT = LT

hprop_collation_antisymmetric :: H.Property
hprop_collation_antisymmetric = H.property $ do
  let jcol = japaneseCollator False
  s1 <- H.forAllWith (showKanaList jcol) $ genKanaList
  s2 <- H.forAllWith (showKanaList jcol) $ genKanaList
  let c = collate jcol s1 s2
  H.annotateShow c
  collate jcol s2 s1 H.=== inverseOrdering c

transitiveOrdering :: Ordering -> Ordering -> Maybe Ordering
transitiveOrdering LT LT = Just LT
transitiveOrdering LT EQ = Just LT
transitiveOrdering EQ LT = Just LT
transitiveOrdering EQ EQ = Just EQ
transitiveOrdering GT EQ = Just GT
transitiveOrdering EQ GT = Just GT
transitiveOrdering GT GT = Just GT
transitiveOrdering _  _  = Nothing

hprop_collation_transitive :: H.Property
hprop_collation_transitive = H.property $ do
  let jcol = japaneseCollator False
  s1 <- H.forAllWith (showKanaList jcol) $ genKanaList
  s2 <- H.forAllWith (showKanaList jcol) $ genKanaList
  s3 <- H.forAllWith (showKanaList jcol) $ genKanaList
  let c1 = collate jcol s1 s2
  H.annotateShow c1
  let c2 = collate jcol s2 s3
  H.annotateShow c2
  case transitiveOrdering c1 c2 of
    Nothing -> H.success
    Just c -> collate jcol s1 s3 H.=== c

hprop_collation_same_as_icu :: H.Property
hprop_collation_same_as_icu = H.property $ do
  let jcol = japaneseCollator False
  col <- liftIO $ Col.open "ja"
  liftIO $ Col.setAttribute col $ Col.HiraganaQuaternaryMode True
  liftIO $ Col.setAttribute col $ Col.Strength Col.Quaternary
  s1 <- H.forAllWith (showKanaList jcol) $ genKanaList
  s2 <- H.forAllWith (showKanaList jcol) $ genKanaList
  expected <- liftIO $ Col.collate col (fromString s1) (fromString s2)
  collate jcol s1 s2 H.=== expected
