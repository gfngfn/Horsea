module Staged.BuiltIn
  ( ass0exprListMap,
    ass0exprMaybeMap,
    tyNat,
  )
where

import Staged.BuiltIn.Core
import Staged.Syntax
import Prelude

ass0exprListMap :: Ass0Expr
ass0exprListMap = A0BuiltInName (BuiltInArity2 BIListMap)

ass0exprMaybeMap :: Ass0Expr
ass0exprMaybeMap = A0BuiltInName (BuiltInArity2 BIMaybeMap)

ass0exprIsNonnegative :: Ass0Expr
ass0exprIsNonnegative =
  A0App (A0BuiltInName (BuiltInArity2 BIIntLeq)) (A0Literal (ALitInt 0))

tyNat :: Ass0TypeExpr
tyNat = A0TyPrim (A0TyPrimBase ATyPrimInt) (Just ass0exprIsNonnegative)
