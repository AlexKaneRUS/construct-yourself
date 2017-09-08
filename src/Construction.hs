module Construction
  ( Name, Term(..)
  , bound, free, fresh
<<<<<<< HEAD
  , reduce, substitute, alpha, beta, eta
  , termP, varP, appP, lamP, bracketP
  ) where

import           Construction.Internal.Functions (alpha, beta, bound, eta, free,
                                                  fresh, reduce, substitute)
import           Construction.Internal.Parser    (appP, bracketP, lamP, termP,
                                                  varP)
import           Construction.Internal.Types     (Name, Term (..))
=======
  ) where

import Construction.Internal.Types (Name, Term)
import Construction.Internal.Functions (bound, free, fresh)
>>>>>>> first iteration
