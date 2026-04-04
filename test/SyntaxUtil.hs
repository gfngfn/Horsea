module SyntaxUtil where

import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Tuple.Extra (first)
import Staged.Core
import Staged.SrcSyntax
import Staged.Syntax

type TypeExprVoid = TypeExprF ()

type ExprVoid = ExprF ()

type BindVoid = BindF ()

type PatternVoid = PatternF ()

typ :: TypeExprMainF () -> TypeExprVoid
typ = Expr ()

tyInt :: TypeExprVoid
tyInt = typ (Constructor ([], "Int"))

tyNat :: TypeExprVoid
tyNat = typ (Constructor ([], "Nat"))

tyBool :: TypeExprVoid
tyBool = typ (Constructor ([], "Bool"))

tyVar :: Text -> TypeExprVoid
tyVar a = typ (TyVar (TypeVar a))

tyNormalVec :: ExprVoid -> TypeExprVoid
tyNormalVec e = typ (App (typ (Constructor ([], "Vec"))) Nothing e)

tyPersVec :: ExprVoid -> TypeExprVoid
tyPersVec e = typ (App (typ (Constructor ([], "Vec"))) Nothing (typ (Persistent e)))

tyCode :: TypeExprVoid -> TypeExprVoid
tyCode = typ . Bracket

tyDepFun :: Var -> TypeExprVoid -> TypeExprVoid -> TypeExprVoid
tyDepFun x tye1 tye2 = typ (TyArrow Nothing (Just x, tye1) tye2)

tyDepFunWithLabel :: Label -> Var -> TypeExprVoid -> TypeExprVoid -> TypeExprVoid
tyDepFunWithLabel label x tye1 tye2 = typ (TyArrow (Just label) (Just x, tye1) tye2)

tyNondepFun :: TypeExprVoid -> TypeExprVoid -> TypeExprVoid
tyNondepFun tye1 tye2 = typ (TyArrow Nothing (Nothing, tye1) tye2)

tyNondepFunWithLabel :: Label -> TypeExprVoid -> TypeExprVoid -> TypeExprVoid
tyNondepFunWithLabel label tye1 tye2 = typ (TyArrow (Just label) (Nothing, tye1) tye2)

tyDepOmsFun :: Label -> Var -> TypeExprVoid -> TypeExprVoid -> TypeExprVoid
tyDepOmsFun label x tye1 tye2 = typ (TyOmsArrow label (Just x, tye1) tye2)

tyNondepOmsFun :: Label -> TypeExprVoid -> TypeExprVoid -> TypeExprVoid
tyNondepOmsFun label tye1 tye2 = typ (TyOmsArrow label (Nothing, tye1) tye2)

tyInfFun :: Var -> TypeExprVoid -> TypeExprVoid -> TypeExprVoid
tyInfFun x tye1 tye2 = typ (TyInfArrow (x, tye1) tye2)

tyRefinement :: Var -> TypeExprVoid -> ExprVoid -> TypeExprVoid
tyRefinement x tye1 e2 = typ (TyRefinement x tye1 e2)

tyForAll :: TypeVar -> TypeExprVoid -> TypeExprVoid
tyForAll tyvar tye = typ (TyForAll tyvar tye)

expr :: ExprMainF () -> ExprVoid
expr = Expr ()

litInt :: Int -> ExprVoid
litInt = expr . Literal . LitInt

litFloat :: Double -> ExprVoid
litFloat = expr . Literal . LitFloat

litList :: [ExprVoid] -> ExprVoid
litList = expr . Literal . LitList

litVec :: [Int] -> ExprVoid
litVec = expr . Literal . LitVec

short :: Var -> ExprMainF ann
short x = Var ([], x)

long :: [Var] -> Var -> ExprMainF ann
long ms x = Var (ms, x)

var :: Var -> ExprVoid
var = expr . short

longVar :: [Var] -> Var -> ExprVoid
longVar ms x = expr (long ms x)

nonrecLam :: (Var, TypeExprVoid) -> ExprVoid -> ExprVoid
nonrecLam binder e = expr (Lam Nothing Nothing binder e)

recLam :: (Var, TypeExprVoid) -> (Var, TypeExprVoid) -> ExprVoid -> ExprVoid
recLam binderF binderX e = expr (Lam (Just binderF) Nothing binderX e)

lamOms :: Label -> (Var, TypeExprVoid) -> ExprVoid -> ExprVoid
lamOms label binderX e = expr (LamOms label binderX e)

lamInf :: (Var, TypeExprVoid) -> ExprVoid -> ExprVoid
lamInf binderX e = expr (LamInf binderX e)

app :: ExprVoid -> ExprVoid -> ExprVoid
app e1 e2 = expr (App e1 Nothing e2)

appWithLabel :: ExprVoid -> Label -> ExprVoid -> ExprVoid
appWithLabel e1 label e2 = expr (App e1 (Just label) e2)

appOms :: ExprVoid -> Label -> ExprVoid -> ExprVoid
appOms e1 label e2 = expr (AppOms e1 label e2)

appInfGiven :: ExprVoid -> ExprVoid -> ExprVoid
appInfGiven e1 e2 = expr (AppInfGiven e1 e2)

appInfType :: ExprVoid -> TypeExprVoid -> ExprVoid
appInfType e1 tye2 = expr (AppInfType e1 tye2)

binOp :: Var -> ExprVoid -> ExprVoid -> ExprVoid
binOp op e1 = app (app (var op) e1)

add, sub :: ExprVoid -> ExprVoid -> ExprVoid
add = binOp "+"
sub = binOp "-"

prods :: ExprVoid -> (Var, ExprVoid) -> [(Var, ExprVoid)] -> ExprVoid
prods e1 pair2 rest = expr (Product e1 (fmap (first ((),)) (pair2 :| rest)))

mult :: ExprVoid -> ExprVoid -> [ExprVoid] -> ExprVoid
mult e1 e2 esRest = expr (Product e1 (fmap (((), "*"),) (e2 :| esRest)))

divi :: ExprVoid -> ExprVoid -> ExprVoid
divi e1 e2 = expr (Product e1 (fmap (((), "/"),) (e2 :| [])))

upcast :: ExprVoid -> TypeExprVoid -> ExprVoid
upcast e1 tye2 = expr (As e1 tye2)

bracket :: ExprVoid -> ExprVoid
bracket = expr . Bracket

escape :: ExprVoid -> ExprVoid
escape = expr . Escape

pat :: PatternMainF () -> PatternVoid
pat = Pattern ()

patBool :: Bool -> PatternVoid
patBool = pat . PatBool

patVar :: Var -> PatternVoid
patVar = pat . PatVar

patConstructor :: ConstructorName -> PatternVoid
patConstructor ctor = pat (PatConstructor ([], ctor))

patApp :: PatternVoid -> PatternVoid -> PatternVoid
patApp pat1 = pat . PatApp pat1

type Ass0ExprText = Ass0ExprF Text

type Ass1ExprText = Ass1ExprF Text

type Ass0TypeExprText = Ass0TypeExprF Text

type Ass1TypeExprText = Ass1TypeExprF Text

type StrictAss0TypeExprText = StrictAss0TypeExprF Text

a0litInt :: Int -> Ass0ExprText
a0litInt n = A0Literal (ALitInt n)

a0var :: Text -> Ass0ExprText
a0var = A0Var . AssVarStatic

a0app :: Ass0ExprText -> Ass0ExprText -> Ass0ExprText
a0app = A0App

a0nonrecLam :: Text -> StrictAss0TypeExprText -> Ass0ExprText -> Ass0ExprText
a0nonrecLam x sa0tye1 = A0Lam Nothing (AssVarStatic x, sa0tye1)

a0recLam :: Text -> StrictAss0TypeExprText -> Text -> StrictAss0TypeExprText -> Ass0ExprText -> Ass0ExprText
a0recLam f sa0tyeRec x sa0tye1 = A0Lam (Just (AssVarStatic f, sa0tyeRec)) (AssVarStatic x, sa0tye1)

a0bracket :: Ass1ExprText -> Ass0ExprText
a0bracket = A0Bracket

sa0tyInt :: StrictAss0TypeExprText
sa0tyInt = SA0TyPrim (A0TyPrimBase ATyPrimInt) Nothing

sa0nondepTyArrow :: StrictAss0TypeExprText -> StrictAss0TypeExprText -> StrictAss0TypeExprText
sa0nondepTyArrow sa0tye1 = SA0TyArrow (Nothing, sa0tye1)

a1var :: Text -> Ass1ExprText
a1var = A1Var . AssVarStatic

a1app :: Ass1ExprText -> Ass1ExprText -> Ass1ExprText
a1app = A1App

a1nonrecLam :: Text -> Ass1TypeExprText -> Ass1ExprText -> Ass1ExprText
a1nonrecLam x a1tye1 = A1Lam Nothing (AssVarStatic x, a1tye1)

a1escape :: Ass0ExprText -> Ass1ExprText
a1escape = A1Escape

a1tyInt :: Ass1TypeExprText
a1tyInt = A1TyPrim (A1TyPrimBase ATyPrimInt)

a1tyVec :: Ass0ExprText -> Ass1TypeExprText
a1tyVec a0e = A1TyPrim (a1TyVec a0e)
