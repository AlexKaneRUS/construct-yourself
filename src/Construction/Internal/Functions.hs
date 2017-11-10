{-# LANGUAGE RecordWildCards #-}
                                 -- they make your code clean and clear.
                                 -- Read about this extension here:
                                 -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html



module Construction.Internal.Functions
  ( Context (..)        -- make restrictions is good practice. As you can see here
  , fresh, free, bound  -- we make "public" not all functions, but only Context, fresh, ...
  , reduce, substitute, alpha, beta, eta
  ) where

import           Construction.Internal.Types (ContextElement (..), ContextT,
                                              Name, Term (..), Type (..),
                                              TypeEquation, TypeEquationSystem)
import           Control.Arrow               (second)
import qualified Data.Map.Strict             as M
import           Data.Set                    (Set, delete, empty, insert,
                                              member, notMember, singleton,
                                              union)
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


alpha :: Term -> Set Name -> Term
alpha Lam{..} conflicts | hasConflict = let all_conflicts = conflicts `union` free body
                                            n_variable = fresh all_conflicts
                                            n_body = substitute body variable (Var n_variable)
                                        in Lam n_variable n_body
                        | otherwise   = Lam variable (alpha body conflicts)
                      where hasConflict = variable `member` conflicts
alpha App{..} conflicts = App (alpha algo conflicts) (alpha arg conflicts)
alpha var _ = var

beta :: Term -> Term
beta (App (Lam n e1) e2) = substitute e1 n e2
beta App{..} = let b_algo = beta algo
               in if b_algo /= algo then App b_algo arg else App algo (beta arg)
beta (Lam n e) = Lam n (beta e)
beta var = var

eta :: Term -> Term
eta l@(Lam v (App algo (Var e))) | hasEta    = algo
                                 | otherwise = l
  where hasEta = v == e && v `notMember` free algo
eta term = term


substitute :: Term -> Name -> Term -> Term
substitute v@Var{..} n e | var == n  = e
                         | otherwise = v
substitute   App{..} n e = App (substitute algo n e) (substitute arg n e)
substitute l@Lam{..} n e | variable == n = l
                         | otherwise = let cond   = variable `member` free e
                                           a_lam  = alpha l (free e)
                                           s_body = substitute body n e
                                       in if cond then substitute a_lam n e
                                                  else Lam variable s_body


instance Eq Term where
  Var v1 == Var v2 = v1 == v2
  App algo1 arg1 == App algo2 arg2 = algo1 == algo2 && arg1 == arg2
  Lam v1 b1 == Lam v2 b2 = sub1 == sub2
    where
      freshVar = Var $ fresh $ free b1 `union` free b2
      sub1 = substitute b1 v1 freshVar
      sub2 = substitute b2 v2 freshVar
  _ == _ = False


-- | reduce term
reduce :: Term -> Term
reduce term = let term' = beta term
              in if term' == term
                 then eta term
                 else reduce term'

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
