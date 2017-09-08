<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> first iteration
{-# LANGUAGE OverloadedStrings #-}

module Main where

<<<<<<< HEAD
import           Construction (Name, Term (..), bound, free, fresh, alpha, beta, eta, reduce, substitute)
=======
import           Construction (Name, Term (..), bound, free, fresh)
>>>>>>> first iteration
import           Test.Hspec
import Data.Set


<<<<<<< HEAD
main :: IO ()
main = hspec $ do
    describe "Fresh test" testFresh
    describe "Free test" testFree
    describe "Bound test" testBound
    describe "Alpha test" testAlpha
    describe "Substitution test" testSubs
    describe "Beta test" testBeta
    describe "Eta test" testEta
    describe "Reduce test" testReduce
    describe "Equal test" testEqual

varX, varX0, varX1, varX2 :: Term
varX = Var "x"
varX0 = Var "x0"
varX1 = Var "x1"
varX2 = Var "x2"

lamId, lamK, lamKK :: Term
lamId = Lam "x" varX -- \x.x
lamK  = Lam "x" $ Lam "x1" varX -- \x x1.x == \x.(\x1.x)
lamKK = Lam "x" $ Lam "x1" varX1 -- \x x1.x1 == \x.(\x1.x1)

appXX, appXX1 :: Term
appXX = App varX varX -- (x x)
appXX1 = App varX varX1 -- (x x1)

testFresh :: SpecWith ()
testFresh = do
    let conflicts = fromList ["a", "x", "x1"]
    it "should test fresh function" $ 
        toList conflicts `shouldNotContain` [fresh conflicts]

testFree :: SpecWith ()
testFree = do
    it "#1" $ free varX `shouldBe` singleton "x"
    it "#2" $ free varX1 `shouldBe` singleton "x1"
    it "#3" $ free lamId `shouldBe` empty
    it "#4" $ free lamK `shouldBe` empty
    it "#5" $ free lamKK `shouldBe` empty
    it "#6" $ free appXX  `shouldBe` singleton "x"
    it "#7" $ free (Lam "x" appXX) `shouldBe` empty
    it "#8" $ free (Lam "x" appXX1) `shouldBe` singleton "x1"
    it "#9" $ free (Lam "x2" appXX1) `shouldBe` fromList ["x", "x1"]

testBound :: SpecWith ()
testBound = do
    it "#1"  $ bound varX `shouldBe` empty
    it "#2"  $ bound varX1 `shouldBe` empty
    it "#3"  $ bound lamId `shouldBe` singleton "x"
    it "#4"  $ bound lamK `shouldBe` fromList ["x", "x1"]
    it "#5"  $ bound lamKK `shouldBe` fromList ["x", "x1"]
    it "#6"  $ bound appXX  `shouldBe` empty
    it "#7"  $ bound (Lam "x" appXX) `shouldBe` singleton "x"
    it "#8"  $ bound (Lam "x" appXX1) `shouldBe` singleton "x"
    it "#9"  $ bound (Lam "x2" appXX1) `shouldBe` singleton "x2"
    it "#10" $ bound (Lam "x2" (Lam "x" appXX1)) `shouldBe` fromList ["x", "x2"]

testAlpha :: SpecWith ()
testAlpha = do
    it "#1" $ alpha lamId (singleton "x1") `shouldBe` lamId
    it "#2" $ alpha lamId (singleton "x") `shouldBe` (Lam "x0" varX0)
    it "#3" $ alpha lamId (fromList ["x", "x0", "x1"]) `shouldBe` (Lam "x2" varX2)
    it "#4" $ alpha lamId (fromList ["x", "x0", "x1"]) `shouldBe` (Lam "x2" varX2)

    let term = (Lam "x2" (Lam "x" appXX1))
    it "#5" $ alpha term (fromList ["x2"]) `shouldBe` (Lam "x0" (Lam "x" (App (Var "x") (Var "x1"))))
    it "#6" $ alpha term (fromList ["x"]) `shouldBe` (Lam "x2" (Lam "x0" (App (Var "x0") (Var "x1"))))
    it "#7" $ alpha term (fromList ["x", "x2"]) `shouldBe` (Lam "x0" (Lam "x0" (App (Var "x0") (Var "x1"))))

testSubs :: SpecWith ()
testSubs = do
    it "#1" $ substitute varX "x" varX `shouldBe` varX
    it "#2" $ substitute varX "x1" varX2 `shouldBe` varX
    it "#3" $ substitute varX "x" varX2 `shouldBe` varX2
    it "#4" $ substitute appXX1 "x" varX2 `shouldBe` App varX2 varX1
    it "#5" $ substitute appXX1 "x1" varX2 `shouldBe` App varX varX2
    it "#6" $ substitute lamId "x" varX2 `shouldBe` lamId
    it "#7" $ substitute lamK "x1" varX2 `shouldBe` lamK
    it "#8" $ substitute (Lam "x" (App varX varX2)) "x2" varX1 `shouldBe` Lam "x" (App varX varX1)
    it "#9" $ substitute (Lam "x" (App varX varX2)) "x2" appXX1 `shouldBe` Lam "x0" (App varX0 appXX1)

testBeta :: SpecWith ()
testBeta = do
    it "#1"  $ beta varX `shouldBe` varX
    it "#2"  $ beta appXX1 `shouldBe` appXX1
    it "#3"  $ beta lamK `shouldBe` lamK
    it "#4"  $ beta lamId `shouldBe` lamId
    it "#5"  $ beta (App lamId varX) `shouldBe` varX
    it "#6"  $ beta (App lamId varX1) `shouldBe` varX1
    it "#7"  $ beta (App lamK varX) `shouldBe` Lam "x1" varX
    it "#8"  $ beta (App lamKK varX) `shouldBe` Lam "x1" varX1
    it "#9"  $ beta (App lamKK varX) `shouldBe` Lam "x1" varX1
    it "#10" $ beta (App (App lamKK varX) appXX1) `shouldBe` App (Lam "x1" varX1) appXX1

testEta :: SpecWith ()
testEta = do
    it "#1" $ eta varX `shouldBe` varX
    it "#2" $ eta appXX1 `shouldBe` appXX1
    it "#3" $ eta (Lam "x" (App varX1 varX1)) `shouldBe`  Lam "x" (App varX1 varX1)
    it "#4" $ eta (Lam "x" (App varX1 varX)) `shouldBe` varX1
    it "#5" $ eta (Lam "x" (App varX varX)) `shouldBe` (Lam "x" (App varX varX))

testReduce :: SpecWith ()
testReduce = do
    it "#1" $ reduce varX `shouldBe` varX
    it "#2" $ reduce appXX1 `shouldBe` appXX1
    it "#3" $ reduce lamId `shouldBe` lamId
    it "#4" $ reduce (App lamK varX) `shouldBe` Lam "x1" varX
    it "#5" $ reduce (App (App lamK varX) varX2) `shouldBe` varX
    it "#6" $ reduce (Lam "x0" (App (App (App lamK varX) varX2) varX0)) `shouldBe` varX
      
testEqual :: SpecWith ()
testEqual = do
    it "#1" $ Lam "x" (Var "x") `shouldBe` Lam "y" (Var "y")
    it "#2" $ reduce (App lamK lamId) `shouldBe` lamKK -- K I = K*
    let lamS = Lam "x" (Lam "y" (Lam "z" $ App (App (Var "x") (Var "z")) (App (Var "y") (Var "z"))))
    it "#3" $ reduce (App (App lamS lamK) lamK) `shouldBe` lamId -- S K K = I
=======
main :: IO ()
main = putStrLn "Test suite not yet implemented"
>>>>>>> initial
=======
main :: IO ()
main = hspec $ 
    describe "Fresh test" testFresh

testFresh :: SpecWith ()
testFresh = do
    let conflicts = fromList ["a", "x", "x1"]
    it "should generate fresh names" $ 
        toList conflicts `shouldNotContain` [fresh conflicts]
    
>>>>>>> first iteration
