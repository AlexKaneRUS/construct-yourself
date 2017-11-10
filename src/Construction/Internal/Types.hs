module Construction.Internal.Types
  ( Equation
  , Name, Term(..)
  , Type (..), Context (..), Substitution (..)
  ) where

import           Data.Map  (Map (..))
import           Data.Set  (Set (..))
import           Data.Text (Text)

type Name = Text -- just alias, no more

data Type
  = VarType { varType  :: Name
            }
  | Arrow   { from :: Type
            , to   :: Type
            }
            deriving (Show, Eq, Ord)

data Term = Var { var :: Name }                     -- Variables: a, b, ...
          | App { algo :: Term, arg :: Term }       -- Application: M N
          | Lam { variable :: Name, body :: Term }  -- Abstraction: \x. M
  deriving (Show) -- we deriving some common classes like Show.
                  -- With this deriving you can use function "show"
                  -- to print your term.

type Equation = (Type, Type)

data Type = TVar { tvar :: Name }                   -- Type variables: a, b, ...
          | TArr { from :: Type, to :: Type }       -- Arrow types: a -> b
  deriving (Eq, Ord, Show)

newtype Context = Context { getCtx :: Map Name Type } -- Types of variables
  deriving (Show)

newtype Substitution = Substitution { getSubs :: Map Name Type } -- Substitute type variable by some type
  deriving (Show)

type Equation = (Type, Type) -- Equation on types

instance Monoid Context where
  mempty = undefined
  Context a `mappend` Context b = undefined

instance Monoid Substitution where
  mempty = undefined
  Substitution a `mappend` Substitution b = undefined
