{-# Language OverloadedStrings #-}
module Language.UHIM.Japanese.TransformSpec where

import Control.Monad.IO.Class
import Data.Semigroup
import Data.Set (Set)
import qualified Data.Set as S
import Data.String
import qualified Data.Text.ICU.Collate as Col
import Language.UHIM.Japanese.Adjective (JaAdjConjugation(..))
import Language.UHIM.Japanese.Verb (JaVerbConjugation(..), isClassicalVerbConjugation)
import Language.UHIM.Japanese.Prim

import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Language.UHIM.Japanese.Transform

classicalAdjClasses :: Set JaAdjConjugation
classicalAdjClasses = S.fromList
  [ JaAdjKu
  , JaAdjSiku
  , JaAdjZiku
  , JaAdjNari
  , JaAdjTari
  ]

modernAdjClasses :: Set JaAdjConjugation
modernAdjClasses = S.fromList
  [ JaAdjI
  , JaAdjSii
  , JaAdjZii
  , JaAdjNa
  ]

genAdjClass :: H.MonadGen m => m JaAdjConjugation
genAdjClass = Gen.element classes
  where
    classes = S.toList $ modernAdjClasses <> classicalAdjClasses

genAdjClasses :: H.MonadGen m => m [JaAdjConjugation]
genAdjClasses = Gen.list (Range.linear 1 10) genAdjClass

genVerbClass :: H.MonadGen m => m JaVerbConjugation
genVerbClass = JaVerbConjugation <$> genVar <*> genStem <*> genClass
  where
    genVar = Gen.element [minBound..]
    genStem = Gen.element [minBound..]
    genClass = Gen.element [minBound..]

genVerbClasses :: H.MonadGen m => m [JaVerbConjugation]
genVerbClasses = Gen.list (Range.linear 1 10) genVerbClass

hprop_default_adjConjugationSelector_chooses_classical_conjugations :: H.Property
hprop_default_adjConjugationSelector_chooses_classical_conjugations = H.property $ do
  cs <- H.forAll genAdjClasses
  case S.fromList cs `S.intersection` classicalAdjClasses of
    ccs | S.null ccs -> H.success
    ccs -> do
      let c = adjConjugationSelector defaultConfig cs
      H.annotateShow $ c
      (c `elem` ccs) H.=== True

hprop_default_verbConjugationSelector_chooses_classical_conjugations :: H.Property
hprop_default_verbConjugationSelector_chooses_classical_conjugations = H.property $ do
  cs <- H.forAll genVerbClasses
  let ccs = filter isClassicalVerbConjugation cs
  if null ccs
    then H.success
    else do
      let c = verbConjugationSelector defaultConfig cs
      H.annotateShow $ c
      (c `elem` ccs) H.=== True
