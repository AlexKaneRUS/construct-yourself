{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
module Construction.Internal.TypeFunctions where

import           Construction.Internal.Functions hiding (Context, substitute)
import           Construction.Internal.Types
import           Data.List                       (nub)
import           Data.Map                        (fromList, keys, mapWithKey,
                                                  member)
import qualified Data.Map                        as M (union, (!))
import           Data.Set                        (Set (..), delete, elemAt,
                                                  insert, singleton, toList)
import qualified Data.Set                        as S (fromList, map, union)
import           Data.Text                       (pack)

-- Split a set of elements to the first element and rest set
split :: Ord a => Set a -> (a, Set a)
split set = let x = elemAt 0 set
            in  (x, delete x set)

-- Take variable type from context or return Nothing
(!) :: Context -> Name -> Maybe Type
ctx ! x | member x (getCtx ctx) = Just $ getCtx ctx M.! x
        | otherwise             = Nothing

-- Something we can perform substitution with
class Substitutable a where
  substituteT :: Substitution -> a -> a

-- Substitution in context
--   [a:=t]empty       => empty
--   [a:=t]{x:t1 ... } => {x:([a:=t]t1) ... }
instance Substitutable Context where
  substituteT sub (Context context) = Context (fmap (substituteT sub) context)

-- Substitution in type:
--   [a:=t] a     => t
--   [a:=t] b     => b
--   [a:=t](r->p) => ([a:=t]r)->([a:=t]p)
instance Substitutable Type where
  substituteT (Substitution sub) t@(TVar name) = if name `elem` keys sub then (sub M.! name)
                                                 else t
  substituteT sub (TArr l r) = TArr (substituteT sub l) (substituteT sub r)

-- Compose two substitutions
-- S@[a1 := t1, ...] . [b1 := s1 ...] = [b1 := S(s1) ... a1 := t1 ...]
compose :: Substitution -> Substitution -> Substitution
compose (Substitution b) sub@(Substitution a) = Substitution (fmap (substituteT sub) b `M.union` a)

-- Create new context from free variables of some term
contextFromTerm :: Term -> Context
contextFromTerm term = Context $ fromList $ zip (toList $ free term) vars
  where
    vars = fmap (TVar . pack . ('a':) . show) [1..]

-- Find a substitution that can solve the set of equations
u :: Set Equation -> Maybe Substitution
u set | null set  = pure mempty
      | otherwise = let (x, rest) = split set
                    in case x of
                      (a@TVar{..}, b) -> do
                          cond <- if tvar `elem` getTypeNames b then Nothing else Just (mempty :: Substitution)
                          if a == b then u rest
                          else do
                              let newSub = Substitution (fromList [(tvar, b)])
                              let newSyst = S.map (\(x, y) -> (substituteT newSub x, substituteT newSub y)) rest
                              restSub <- u newSyst
                              pure (compose newSub restSub)
                      (a, b@TVar{}) -> u ((b, a) `insert` rest)
                      (TArr a1 a2, TArr b1 b2) -> u ((a1, b1) `insert` ((a2, b2) `insert` rest))


-- Generate equations set from some term
-- NB: you can use @fresh@ function to generate type names
e :: Context -> Term -> Type -> Maybe (Set Equation)
e ctx term tpe = e' (getTypeNames tpe ++ keys (getCtx ctx)) ctx term tpe

e' :: [Name] -> Context -> Term -> Type -> Maybe (Set Equation)
e' taken ctx term tpe = case term of
                   Var{..} -> singleton . ((,) tpe) <$> ctx ! var
                   App{..} -> do
                       let freshN = fresh (S.fromList taken)
                       let freshNT = TVar freshN
                       algoSet <- e' (freshN : taken) ctx algo (TArr freshNT tpe)
                       argSet <- e' (nub (concatMap getEquationNames algoSet)) ctx arg freshNT
                       pure (algoSet `S.union` argSet)
                   Lam{..} -> do
                     let freshN = fresh (S.fromList taken)
                     let freshNT = TVar freshN
                     let newCtx = ctx `mappend` Context (fromList [(variable, freshNT)])
                     let newTaken = freshN : taken
                     let freshN' = fresh (S.fromList newTaken)
                     let freshNT' = TVar freshN'
                     resSet <- e' (freshN' : newTaken) newCtx body freshNT'
                     pure ((tpe, TArr freshNT freshNT') `insert` resSet)

getEquationNames :: Equation -> [Name]
getEquationNames (t1, t2) = nub (getTypeNames t1 ++ getTypeNames t2)

getTypeNames :: Type -> [Name]
getTypeNames (TVar n)   = [n]
getTypeNames (TArr l r) = getTypeNames l ++ getTypeNames r

-- Find a principal pair of some term if exists
pp :: Term -> Maybe PrincipalPair
pp term = do let ctx = contextFromTerm term
             let tpe = TVar "r"
             eqs <- e ctx term tpe
             subs <- u eqs
             pure (PP (substituteT subs ctx, substituteT subs tpe))
