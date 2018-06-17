{-# Language OverloadedStrings #-}
module Language.UHIM.Dictionary.YamlSpec where

import Control.Lens
import Data.Map (Map)

import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Language.UHIM.Japanese.Adjective (JaAdjConjugation)
import Language.UHIM.Japanese.Prim
import Language.UHIM.Japanese.Verb (JaVerbConjugation(..))

import Language.UHIM.Dictionary.Yaml

genString :: H.MonadGen m => m String
genString = Gen.list (Range.linear 0 100) Gen.alpha

genJaYomi :: H.MonadGen m => m JaYomi
genJaYomi = Gen.choice
    [ NonChange <$> genString
    , Changed <$> genString <*> genString
    ]
  where

genMaybeString :: H.MonadGen m => m (Maybe String)
genMaybeString = Gen.maybe genString

genMaybeJaYomi :: H.MonadGen m => m (Maybe JaYomi)
genMaybeJaYomi = Gen.maybe genJaYomi

genPron :: H.MonadGen m => m Pron
genPron = Pron
  <$> genMaybeJaYomi
  <*> genMaybeJaYomi
  <*> genMaybeJaYomi
  <*> genMaybeJaYomi
  <*> genMaybeJaYomi
  <*> genMaybeJaYomi
  <*> genMaybeJaYomi

genKanjiShapes :: H.MonadGen m => m KanjiShapes
genKanjiShapes = KanjiShapes <$> genSM
  where
    genSM = Gen.map (Range.linear 0 3) $ do
      k <- genKanjiKey
      v <- genKanji
      return (k, v)

genKanjiKey :: H.MonadGen m => m String
genKanjiKey = Gen.element
  [ commonKanjiKey
  , kyuKanjiKey
  , shinKanjiKey
  , jaKanjiKey
  ]

genKanji :: H.MonadGen m => m String
genKanji = Gen.element
  [ "一"
  , "年"
  ]

genAllEnum :: (Bounded t, Enum t, H.MonadGen m) => m t
genAllEnum = Gen.element [minBound..]

genJaVerbConjugations :: H.MonadGen m => m [JaVerbConjugation]
genJaVerbConjugations = Gen.list (Range.linear 0 3) $ JaVerbConjugation <$> genAllEnum <*> genAllEnum <*> genAllEnum

genJaAdjConjugations :: H.MonadGen m => m [JaAdjConjugation]
genJaAdjConjugations = Gen.list (Range.linear 0 3) $ Gen.element [minBound..]

genDeclMeaning :: H.MonadGen m => m (Maybe [String])
genDeclMeaning = Gen.maybe . Gen.list (Range.linear 0 3) $ Gen.string (Range.linear 0 3) Gen.ascii

genDeclKeys :: H.MonadGen m => m (Maybe (Map String String))
genDeclKeys = Gen.maybe . Gen.map (Range.linear 0 3) $ do
  n <- Gen.element
    [ "TUT"
    , "T"
    ]
  s <- Gen.string (Range.linear 0 3) Gen.ascii
  return (n, s)

genDeclFreq :: H.MonadGen m => m (Maybe Double)
genDeclFreq = pure Nothing

genDeclTags :: H.MonadGen m => m (Maybe [String])
genDeclTags = Gen.maybe . Gen.list (Range.linear 0 3) $ Gen.string (Range.linear 0 3) Gen.ascii

genKanjiDeclaration :: H.MonadGen m => m KanjiDeclaration
genKanjiDeclaration = KanjiDeclaration
  <$> genKanjiShapes
  <*> Gen.list (Range.linear 0 10) genPron
  <*> Gen.maybe (Gen.map (Range.linear 0 3) genShapePair)
  <*> genDeclMeaning
  <*> genDeclKeys
  <*> genDeclFreq
  <*> genDeclTags
  where
    genShapePair :: H.MonadGen m => m (String, String)
    genShapePair = do
      key <- genKanjiKey
      kanji <- genKanji
      return (key, kanji)

genWordConvPair :: H.MonadGen m => m WordConvPair
genWordConvPair = WordConvPair
  <$> genKanjiShapes
  <*> genPron

genWordConvPairs :: H.MonadGen m => m [WordConvPair]
genWordConvPairs = Gen.list (Range.linear 0 10) genWordConvPair

genWordDeclaration :: H.MonadGen m => m WordDeclaration
genWordDeclaration = WordDeclaration
  <$> genWordConvPairs
  <*> genDeclMeaning
  <*> genDeclFreq
  <*> genDeclTags

genJaVerbDeclaration :: H.MonadGen m => m JaVerbDeclaration
genJaVerbDeclaration = JaVerbDeclaration
  <$> genJaVerbConjugations
  <*> genWordConvPairs
  <*> genDeclMeaning
  <*> genDeclFreq
  <*> genDeclTags

genJaAdjDeclaration :: H.MonadGen m => m JaAdjDeclaration
genJaAdjDeclaration = JaAdjDeclaration
  <$> genJaAdjConjugations
  <*> genWordConvPairs
  <*> genDeclMeaning
  <*> genDeclFreq
  <*> genDeclTags

genDictEntry :: H.MonadGen m => m DictEntry
genDictEntry = Gen.choice
  [ Entry字 <$> genKanjiDeclaration
  , Entry語 <$> genWordDeclaration
  , Entry日動詞 <$> genJaVerbDeclaration
  , Entry日形容詞 <$> genJaAdjDeclaration
  , Entry日副詞 <$> genWordDeclaration
  ]

testUpdateIdentical
  :: (Monad m, Show t1, Show t2, Eq t1, Eq t2) =>
     H.Gen t1
     -> H.Gen t2 -> (t1 -> t2) -> (t2 -> t1 -> t1) -> H.PropertyT m ()
testUpdateIdentical gen genSub getter setter = do
  orig <- H.forAll gen
  let v = getter orig
  setter v orig H.=== orig
  v' <- H.forAll genSub
  let orig' = setter v' orig
  getter orig' H.=== v'

hprop_DictEntry_HasDecl義 :: H.Property
hprop_DictEntry_HasDecl義 = H.property $ do
  testUpdateIdentical genDictEntry genDeclMeaning (^. decl義) (set decl義)

hprop_DictEntry_HasDecl頻度 :: H.Property
hprop_DictEntry_HasDecl頻度 = H.property $ do
  testUpdateIdentical genDictEntry genDeclFreq (^. decl頻度) (set decl頻度)

hprop_DictEntry_HasDecl簽 :: H.Property
hprop_DictEntry_HasDecl簽 = H.property $ do
  testUpdateIdentical genDictEntry genDeclTags (^. decl簽) (set decl簽)
