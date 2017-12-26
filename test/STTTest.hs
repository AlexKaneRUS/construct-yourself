{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Construction (Context (..), Substitutable (..),
                               Substitution (..), Type (..), compose)
import qualified Data.Map     as M
import           Data.Set
import           Test.Hspec


main :: IO ()
main = hspec $ do
    describe "Substitute monoid test" testSubMonoid
    describe "Context monoid test" testContMonoid
    describe "Substitution type test" testSubType
    describe "Substitution context test" testSubCont
    describe "Composition of substitutions test" testCompSub

singleSub :: Substitution
singleSub = Substitution (M.fromList [("x1", TVar "x2")])

doubleSub :: Substitution
doubleSub = Substitution (M.fromList [("x2", TVar "x3"), ("x3", TVar "x4")])

tripleSub :: Substitution
tripleSub = Substitution (M.fromList [("x1", TVar "x2"), ("x2", TVar "x3"), ("x3", TVar "x4")])

singleCont :: Context
singleCont = Context (M.fromList [("x1", TVar "x2")])

doubleCont :: Context
doubleCont = Context (M.fromList [("x2", TVar "x3"), ("x3", TVar "x4")])

tripleCont :: Context
tripleCont = Context (M.fromList [("x1", TVar "x2"), ("x2", TVar "x3"), ("x3", TVar "x4")])

testSubMonoid :: SpecWith ()
testSubMonoid = do
    it "#1" $ (mempty :: Substitution) `shouldBe` Substitution M.empty
    it "#2" $ (mempty `mappend` singleSub) `shouldBe` singleSub
    it "#3" $ (singleSub `mappend` singleSub) `shouldBe` singleSub
    it "#4" $ (singleSub `mappend` doubleSub) `shouldBe` tripleSub
    it "#5" $ (doubleSub `mappend` tripleSub) `shouldBe` tripleSub

testContMonoid :: SpecWith ()
testContMonoid = do
    it "#1" $ (mempty :: Context) `shouldBe` Context M.empty
    it "#2" $ (mempty `mappend` singleSub) `shouldBe` singleSub
    it "#3" $ (singleSub `mappend` singleSub) `shouldBe` singleSub
    it "#4" $ (singleSub `mappend` doubleSub) `shouldBe` tripleSub
    it "#5" $ (doubleSub `mappend` tripleSub) `shouldBe` tripleSub

testSubType :: SpecWith ()
testSubType = do
    it "#1" $ substituteT singleSub (TVar "x1") `shouldBe` (TVar "x2")
    it "#2" $ substituteT singleSub (TVar "x2") `shouldBe` (TVar "x2")
    it "#3" $ substituteT singleSub (TArr (TVar "x2") (TVar "x3")) `shouldBe` (TArr (TVar "x2") (TVar "x3"))
    it "#4" $ substituteT doubleSub (TArr (TVar "x2") (TVar "x3")) `shouldBe` (TArr (TVar "x3") (TVar "x4"))
    it "#5" $ substituteT tripleSub (TArr (TArr (TVar "x6") (TVar "x3")) (TVar "x3")) `shouldBe` (TArr (TArr (TVar "x6") (TVar "x4")) (TVar "x4"))

testSubCont :: SpecWith ()
testSubCont = do
    it "#1" $ substituteT singleSub singleCont `shouldBe` singleCont
    it "#2" $ substituteT tripleSub (Context (M.fromList [("x3", TVar "x1")])) `shouldBe` Context (M.fromList [("x3", TVar "x2")])
    it "#3" $ substituteT tripleSub (mempty :: Context) `shouldBe` mempty
    it "#4" $ substituteT tripleSub (Context (M.fromList [("a1", TVar "x1"), ("a2", TVar "x3")])) `shouldBe` Context (M.fromList [("a1", TVar "x2"), ("a2", TVar "x4")])
    it "#5" $ substituteT mempty (Context (M.fromList [("a1", TVar "x1"), ("a2", TVar "x3")])) `shouldBe` Context (M.fromList [("a1", TVar "x1"), ("a2", TVar "x3")])

testCompSub :: SpecWith ()
testCompSub = do
    it "#1" $ compose singleSub doubleSub `shouldBe` Substitution (M.fromList [("x1", TVar "x3"), ("x2", TVar "x3"), ("x3", TVar "x4")])
    it "#2" $ compose (Substitution (M.fromList [("x1", TArr (TVar "x3") (TVar "x4"))])) (Substitution (M.fromList [("x3", TVar "x5")]))  `shouldBe` Substitution (M.fromList [("x1", TArr (TVar "x5") (TVar "x4")), ("x3", TVar "x5")])
    it "#3" $ compose (Substitution (M.fromList [("x1", TArr (TVar "x3") (TVar "x4"))])) (Substitution (M.fromList [("x4", TArr (TVar "x3") (TVar "x5"))]))  `shouldBe` Substitution (M.fromList [("x1", TArr (TVar "x3") (TArr (TVar "x3") (TVar "x5"))), ("x4", TArr (TVar "x3") (TVar "x5"))])
    it "#4" $ compose mempty (Substitution (M.fromList [("x4", TArr (TVar "x3") (TVar "x5"))])) `shouldBe` Substitution (M.fromList [("x4", TArr (TVar "x3") (TVar "x5"))])
    it "#5" $ compose (Substitution (M.fromList [("x6", TArr (TVar "x3") (TArr (TVar "x3") (TVar "x5"))), ("x4", TArr (TVar "x3") (TVar "x5"))])) tripleSub `shouldBe` (Substitution (M.fromList [("x6", TArr (TVar "x4") (TArr (TVar "x4") (TVar "x5"))), ("x4", TArr (TVar "x4") (TVar "x5"))])) `mappend` tripleSub
