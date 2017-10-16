module Construction.Internal.Types
  ( Name, Term(..), ContextT, Type (..), ContextElement, TypeEquation, TypeEquationSystem
  ) where

import           Data.Text (Text)



type Name = Text -- just alias, no more

type ContextT = [ContextElement]
type ContextElement = (Name, Type)

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
  deriving (Show, Eq, Ord) -- we deriving some common classes like Show.
                           -- With this deriving you can use function "show"
                           -- to print your term.

type TypeEquation = (Type, Type)
type TypeEquationSystem = [TypeEquation]
