{-# LANGUAGE RecordWildCards #-}
                                 -- they make your code clean and clear.
                                 -- Read about this extension here:
                                 -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html



module Construction.Internal.Functions
  ( Context (..)
  , fresh, free, bound
  ) where

import           Construction.Internal.Types (ContextElement (..), ContextT,
                                              Name, Term (..), Type (..),
                                              TypeEquation, TypeEquationSystem)
import           Control.Arrow               (second)
import qualified Data.Map.Strict             as M
import           Data.Set                    (Set, delete, empty, fromList,
                                              insert, member, notMember,
                                              singleton, toList, union)
import           Data.Text                   (pack)
import           Debug.Trace                 (trace)


-- Context is just set of names that are in our context.
type Context = Set Name

-- | @fresh@ generates new variable different from every variables in set.
fresh :: Set Name -> Name
fresh conflicts = head . dropWhile (`member` conflicts) $ nameGen -- This is ugly name generator. Make it better.
  where nameGen = [pack $ 'x' : show ind | ind <- [0..] :: [Int]]

-- | @free@ finds all free (Amazing!) variables from given term.
free :: Term -> Set Name
free (Var var)           = singleton var
free (App algo arg)      = free algo `union` free arg
free (Lam variable body) = variable `delete` free body

-- | @bound@ finds all bounded variables from given term.
-- This function uses RecordWildCards.
-- If you like it refactor @free@ function.
bound :: Term -> Set Name
bound Var{}   = empty
bound App{..} = bound algo `union` bound arg
bound Lam{..} = variable `insert` bound body

-- a[n := b] - substiturion
substitute :: Term -> Name -> Term -> Term
substitute v@Var{..} n b | var == n  = b
                         | otherwise = v
substitute App{..} n b = App (substitute algo n b) (substitute arg n b)
substitute lam@Lam{..} n b = if n `notElem` free body then Lam variable (substitute body n b)
                             else lam

-- | alpha reduction
alpha :: Term -> Set Name -> Term
alpha v@Var{} _ = v
alpha App{..} conflicts = App (alpha algo conflicts) (alpha arg conflicts)
alpha Lam{..} conflicts = if variable `elem` conflicts then Lam newName (alpha newBody conflicts)
                          else Lam variable (alpha body conflicts)
  where
    newName = fresh (conflicts `union` insert variable (bound body))
    newBody = substitute body variable (Var newName)

-- | beta reduction
beta :: Term -> Term
beta (App Lam{..} right) = substitute body variable right
beta a                   = a

-- | eta reduction
eta :: Term -> Term
eta term@Lam{..} =
  case body of
    App left (Var x) -> if x `notElem` free left then body
                        else term
    _ -> term

typeSub :: Type -> Name -> Type -> Type
typeSub t@VarType{..} n nT = if varType == n then nT
                             else t
typeSub Arrow{..} n nT = Arrow (typeSub from n nT) (typeSub to n nT)

type MultSub = [(Name, Type)]

freeT :: Type -> Set Name
freeT VarType{..} = singleton varType
freeT Arrow{..}   = freeT from `union` freeT to

multTypeSub :: Type -> MultSub -> Type
multTypeSub t@VarType{..} list = if varType `elem` fmap fst list then M.fromList list M.! varType
                                 else t
multTypeSub Arrow{..} list = Arrow (multTypeSub from list) (multTypeSub to list)

compSub :: MultSub -> MultSub -> MultSub
compSub s t = fmap (second (`multTypeSub` t)) s ++ t

unify :: Type -> Type -> MultSub
unify (VarType a) vt@(VarType b) = if a == b then []
                                   else [(a, vt)]
unify VarType{..} typeB = if varType `notElem` freeT typeB then [(varType, typeB)]
                          else error "Can't unify"
unify ar@Arrow{..} vt@VarType{} = unify vt ar
unify (Arrow fromA toA) (Arrow fromB toB) = compSub uniToAToB (unify (multTypeSub fromA uniToAToB) (multTypeSub fromB uniToAToB))
  where
    uniToAToB = unify toA toB

typeEquationsSystem :: [Name] -> ContextT -> Term -> Type -> TypeEquationSystem
typeEquationsSystem _ context Var{..} t = [(t, M.fromList context M.! var)]
typeEquationsSystem taken context App{..} t = typeEquationsSystem newTaken context algo (Arrow freshVar t) ++ typeEquationsSystem newTaken context arg freshVar
  where
    freshVar = VarType (fresh (fromList taken))
    newTaken = varType freshVar : taken
typeEquationsSystem taken context Lam{..} t = trace (show newContext) (typeEquationsSystem finalTaken newContext body freshVarB ++ [(t, arrowT)])
  where
    freshVarA = VarType (fresh (fromList taken))
    newTaken = varType freshVarA : taken
    newContext = context ++ [(variable, freshVarA)]
    freshVarB = VarType (fresh (fromList newTaken))
    finalTaken = varType freshVarB : newTaken
    arrowT = Arrow freshVarA freshVarB

solveEqSystem :: TypeEquationSystem -> MultSub
solveEqSystem [] = []
solveEqSystem ((a@VarType{..}, b) : xs) | a == b = solveEqSystem xs
                                        | varType `elem` freeT b = error "Can't solve system of type equations."
                                        | otherwise = trace (show singleSub ++ " " ++ show xs ++ " " ++ show subbedTail) (compSub singleSub (solveEqSystem subbedTail))
  where
    singleSub = [(varType, b)]
    subbedTail = fmap (\(x, y) -> (multTypeSub x singleSub, multTypeSub y singleSub)) xs
solveEqSystem ((a, b@VarType{}) : xs) = solveEqSystem ((b, a) : xs)
solveEqSystem ((Arrow fromA toA, Arrow fromB toB) : xs) = solveEqSystem ((fromA, fromB) : (toA, toB) : xs)

pp :: Term -> Type
pp term = trace (show eqSystem) (snd (last systemSolved))
  where
    fvM = free term
    freshNames = foldl (\x y -> (fresh (fromList x) : x)) [] fvM
    contextT = zip (toList fvM) (fmap VarType freshNames)
    toFind = VarType (pack "sigm")
    eqSystem = typeEquationsSystem (varType toFind : freshNames) contextT term toFind
    systemSolved = solveEqSystem eqSystem
