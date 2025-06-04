module Staged.BuiltIn
  ( tyNat,
  )
where

import Staged.BuiltIn.Core
import Staged.Syntax
import Prelude

ass0exprIsNonnegative :: Ass0Expr
ass0exprIsNonnegative =
  A0App (A0BuiltInName (BuiltInArity2 BILeq)) (A0Literal (ALitInt 0))

tyNat :: Ass0TypeExpr
tyNat = A0TyPrim (A0TyPrimBase ATyPrimInt) (Just ass0exprIsNonnegative)
