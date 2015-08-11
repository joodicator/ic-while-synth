module ASP(
    Predicate(..), Variable(..), Function(..), Constant(..), LuaFunc(..),
    Rule(..), Head(..), Body(..), Conjunct(..), Literal(..),
    Atom(..), Comparison(..), Expr(..), Term(..), CBiOp(..), EBiOp(..), EUnOp(..),
    FreeVars(..), freeVars, isFreeIn,

    propToRules
) where

import ASP.Base
import ASP.Show()

import qualified Logic

import qualified Data.Set as S
import Data.Monoid
import Data.MonoTraversable

--------------------------------------------------------------------------------
-- Conversion from classical propositions.

-- Given a rule head H and a classical proposition P in ASP literals, and a
-- possibly empty body fragment fixing the domain for each variable for which
-- this is necessary, gives a set of rules equivalent to H :- P, under the
-- assumption that negation in P is equivalent to negation-as-failure and not
-- classical negation in ASP's sense.
propToRules :: (Variable -> Body) -> Head -> Logic.Prop Literal -> [Rule]
propToRules dom rHead
  = map bodyToRule . propToBodies
  where
    bodyToRule :: Body -> Rule    
    bodyToRule rBody
      = rHead :- ofoldMap dom (headVars `S.union` bodyVars) <> rBody
      where bodyVars = closeFreeVars $ freeVars rBody
    headVars = closeFreeVars $ freeVars rHead

    -- Given a set S of variables, computes the smallest set T for which:
    -- 1. S `isSubsetOf` T
    -- 2. forall v in T. freeVars (dom v) `isSubsetOf` T
    closeFreeVars :: S.Set Variable -> S.Set Variable
    closeFreeVars vars
      | subVars `S.isSubsetOf` vars = vars
      | otherwise                   = closeFreeVars (vars `S.union` subVars)
      where subVars = ofoldMap (freeVars . dom) vars
  
-- Given a classical proposition in ASP literals, gives a disjunctive list of
-- rule bodies equivalent to the proposition in the same sense as propToRules.
propToBodies :: Logic.Prop Literal -> [Body]
propToBodies prop
  = map disj . S.toList . Logic.unDisj $ Logic.pToDNF' id prop
  where
    disj (Logic.Conj cs) = Body . map CLiteral $ S.toList cs
