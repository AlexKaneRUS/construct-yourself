module Construction.Internal.Types
  ( Equation
  , Name, Term(..)
  , Type (..), Context (..), Substitution (..), PrincipalPair (..)
  ) where

import           Data.Map  (Map (..), empty, keys, mapWithKey, toList, union,
                            (!))
import           Data.Set  (Set (..))
import           Data.Text (Text, unpack)

type Name = Text -- just alias, no more

data Term = Var { var :: Name }                     -- Variables: a, b, ...
          | App { algo :: Term, arg :: Term }       -- Application: M N
          | Lam { variable :: Name, body :: Term }  -- Abstraction: \x. M
  deriving (Show) -- we deriving some common classes like Show.
                  -- With this deriving you can use function "show"
                  -- to print your term.

data Type = TVar { tvar :: Name }                   -- Type variables: a, b, ...
          | TArr { from :: Type, to :: Type }       -- Arrow types: a -> b
  deriving (Eq, Ord)

instance Show Type where
  show (TVar a)           = unpack a
  show (TArr (TVar a) to) = unpack a ++ " -> " ++ show to
  show (TArr from to)     = "("  ++ show from ++ ") -> " ++ show to

newtype Context = Context { getCtx :: Map Name Type } -- Types of variables
  deriving Eq

instance Show Context where
  show (Context context) = (tail . init) (concatMap (\(n, t) -> " " ++ unpack n ++ " : " ++ show t ++ ",") pairs)
    where
      pairs = toList context

newtype Substitution = Substitution { getSubs :: Map Name Type } -- Substitute type variable by some type
  deriving (Eq, Show)

type Equation = (Type, Type) -- Equation on types

instance Monoid Context where
  mempty = Context empty
  Context a `mappend` Context b = Context (a `union` b)

instance Monoid Substitution where
  mempty = Substitution empty
  Substitution a `mappend` Substitution b = Substitution (a `union` b)

data PrincipalPair = PP (Context, Type)

instance Show PrincipalPair where
  show (PP (c, t)) = show c ++ " |- " ++ show t
