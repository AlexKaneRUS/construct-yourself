{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Construction (Context (..), PrincipalPair (..),
                               Substitutable (..), Substitution (..), Term (..),
                               Type (..), compose, e, pp, termP, u)
import qualified Data.Map     as M
import           Data.Set
import qualified Data.Set     as S
import           Test.Hspec
import           Text.Parsec


main :: IO ()
main = hspec $ do
    describe "Substitute monoid test" testSubMonoid
    describe "Context monoid test" testContMonoid
    describe "Substitution type test" testSubType
    describe "Substitution context test" testSubCont
    describe "Composition of substitutions test" testCompSub
    describe "E algorithm test" testE
    describe "U algorithm test" testU
    describe "Parse and output test" testParseAndOut

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

testE :: SpecWith ()
testE = do
    it "#1" $ e mempty (Lam "x1" (Var "x1")) (TVar "r") `shouldBe` Just (S.fromList [(TVar "r", TArr (TVar "x0") (TVar "x1")), (TVar "x1", TVar "x0")])
    it "#2" $ e (Context (M.fromList [("x2", TVar "a1")])) (Lam "x1" (App (Var "x1") (Var "x2"))) (TVar "r") `shouldBe` Just (S.fromList [(TVar "r", TArr (TVar "x0") (TVar "x1")), (TVar "x3", TVar "a1"), (TArr (TVar "x3") (TVar "x1"), TVar "x0")])
    it "#3" $ e (Context (M.fromList [("x2", TArr (TVar "a1") (TVar "a2"))])) (Lam "x1" (App (Var "x2") (Var "x1"))) (TVar "r") `shouldBe` Just (S.fromList [(TVar "r", TArr (TVar "x0") (TVar "x1")), (TVar "x3", TVar "x0"), (TArr (TVar "x3") (TVar "x1"), TArr (TVar "a1") (TVar "a2"))])
    it "#4" $ e mempty (Lam "x1" (Lam "x2" (App (Var "x2") (Var "x1")))) (TVar "r") `shouldBe` Just (S.fromList [(TVar "r", TArr (TVar "x0") (TVar "x1")), (TVar "x1", TArr (TVar "x2") (TVar "x3")), (TVar "x4", TVar "x0"), (TArr (TVar "x4") (TVar "x3"), TVar "x2")])
    it "#5" $ e (Context (M.fromList [("x1", TVar "a1")])) (Lam "x4" (App (Lam "x2" (App (Var "x2") (Var "x1"))) (Var "x1"))) (TVar "r") `shouldBe` Just (S.fromList [(TVar "r", TArr (TVar "x0") (TVar "x2")), (TVar "x3", TVar "a1"), (TVar "x6", TVar "a1"), (TArr (TVar "x3") (TVar "x2"), TArr (TVar "x4") (TVar "x5")), (TArr (TVar "x6") (TVar "x5"), TVar "x4")])

testU :: SpecWith ()
testU = do
    it "#1" $ u (S.fromList [(TVar "r", TArr (TVar "x0") (TVar "x1")), (TVar "x1", TVar "x0")])`shouldBe` Just (Substitution (M.fromList [("r", TArr (TVar "x0") (TVar "x0")), ("x1", TVar "x0")]))
    it "#2" $ u (S.fromList [(TVar "r", TArr (TVar "x0") (TVar "x1")), (TVar "x3", TVar "a1"), (TArr (TVar "x3") (TVar "x1"), TVar "x0")]) `shouldBe` Just (Substitution (M.fromList [("r", TArr (TArr (TVar "a1") (TVar "x1")) (TVar "x1")), ("x0", TArr (TVar "a1") (TVar "x1")), ("x3", TVar "a1")]))
    it "#3" $ u (S.fromList [(TVar "r", TArr (TVar "x0") (TVar "x1")), (TVar "x3", TVar "x0"), (TArr (TVar "x3") (TVar "x1"), TArr (TVar "a1") (TVar "a2"))]) `shouldBe` Just (Substitution (M.fromList [("r", TArr (TVar "a1") (TVar "a2")), ("x0", TVar "a1"), ("x1", TVar "a2"), ("x3", TVar "a1")]))
    it "#4" $ u (S.fromList [(TVar "r", TArr (TVar "x0") (TVar "x1")), (TVar "x1", TArr (TVar "x2") (TVar "x3")), (TVar "x4", TVar "x0"), (TArr (TVar "x4") (TVar "x3"), TVar "x2")]) `shouldBe` Just (Substitution (M.fromList [("r", TArr (TVar "x0") (TArr (TArr (TVar "x0") (TVar "x3")) (TVar "x3"))), ("x1", TArr (TArr (TVar "x0") (TVar "x3")) (TVar "x3")), ("x2", TArr (TVar "x0") (TVar "x3")), ("x4", TVar "x0")]))
    it "#5" $ u (S.fromList [(TVar "r", TArr (TVar "x0") (TVar "x2")), (TVar "x3", TVar "a1"), (TVar "x6", TVar "a1"), (TArr (TVar "x3") (TVar "x2"), TArr (TVar "x4") (TVar "x5")), (TArr (TVar "x6") (TVar "x5"), TVar "x4")]) `shouldBe` Nothing

testParseAndOut :: SpecWith ()
testParseAndOut = do
    it "#1" $ (fmap show . pp) <$> parse termP "" "\\x.x" `shouldBe` Right (Just "|- x0 -> x0")
    it "#2" $ (fmap show . pp) <$> parse termP "" "\\x.x x1" `shouldBe` Right (Just "x1 : a1 |- (a1 -> x2) -> x2")
    it "#3" $ (fmap show . pp) <$> parse termP "" "\\x.(\\x1.x1 x)" `shouldBe` Right (Just "|- x0 -> (x0 -> x3) -> x3")
    it "#4" $ (fmap show . pp) <$> parse termP "" "\\x.(\\x1.x1 x x)" `shouldBe` Right (Just "|- x0 -> (x0 -> x0 -> x3) -> x3")
    it "#5" $ (fmap show . pp) <$> parse termP "" "\\x.(\\x1.(\\x4.x4 x x3))" `shouldBe` Right (Just "x3 : a1 |- x0 -> x2 -> (x0 -> a1 -> x6) -> x6")
