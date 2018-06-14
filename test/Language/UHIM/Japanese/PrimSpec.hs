{-# Language OverloadedStrings #-}
module Language.UHIM.Japanese.PrimSpec where

import Control.Monad.IO.Class
import Data.Semigroup
import Data.String
import qualified Data.Text.ICU.Collate as Col

import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Language.UHIM.Japanese.Prim

genJaYomi :: H.MonadGen m => m JaYomi
genJaYomi = Gen.choice
    [ NonChange <$> genString
    , Changed <$> genString <*> genString
    ]
  where
    genString = Gen.list (Range.linear 0 100) Gen.alpha

hprop_JaYomi_Semigroup_transitive :: H.Property
hprop_JaYomi_Semigroup_transitive = H.property $ do
  s1 <- H.forAll $ genJaYomi
  s2 <- H.forAll $ genJaYomi
  s3 <- H.forAll $ genJaYomi
  s1 <> (s2 <> s3) H.=== (s1 <> s2) <> s3

hprop_JaYomi_Monoid_identity :: H.Property
hprop_JaYomi_Monoid_identity = H.property $ do
  s <- H.forAll $ genJaYomi
  s <> mempty H.=== s
  mempty <> s H.=== s

