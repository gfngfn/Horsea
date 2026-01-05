module Staged.Syntax
  ( DatasetParam (..),
    StaticVar (..),
    AssVarF (..),
    Symbol (..),
    symbolToVar,
    AssTypeVar (..),
    AssLiteralF (..),
    Ass0ExprF (..),
    Ass1ExprF (..),
    a1LetIn,
    AssBindF (..),
    makeExprFromBinds,
    Type1EquationF (..),
    Type1PrimEquationF (..),
    Ass0TypeExprF (..),
    StrictAss0TypeExprF (..),
    AssPrimBaseType (..),
    validatePrimBaseType,
    Ass0PrimType (..),
    Ass1TypeExprF (..),
    Ass1PrimTypeF (..),
    AssPersTypeExpr (..),
    persistentTypeTo0,
    persistentTypeTo1,
    liftPrimType,
    Ass0ValF (..),
    Ass1ValF (..),
    Ass0TypeValF (..),
    Ass0PrimTypeVal (..),
    Ass1TypeValF (..),
    Ass1PrimTypeVal (..),
    EvalEnv,
    EvalEnvEntry (..),
    a0TyVec,
    a0TyMat,
    a1TyVec,
    a1TyMat,
    mapAssLiteral,
    mapMAssLiteral,
    strictify,
    decomposeType1Equation,
    AppContextF,
    AppContextEntryF (..),
    ResultF (..),
    AssVar,
    Ass0Expr,
    Ass1Expr,
    AssBind,
    Type1Equation,
    Ass0TypeExpr,
    StrictAss0TypeExpr,
    Ass1TypeExpr,
    Ass0Val,
    Ass1Val,
    Ass0TypeVal,
    Ass1TypeVal,
    Ass1PrimType,
    AppContext,
    Result0,
    Result1,
  )
where

import Data.Functor.Identity
import Data.Map (Map)
import Data.Text (Text)
import Staged.BuiltIn.Core
import Util.Matrix (Matrix)
import Util.TokenUtil (Span)
import Util.Vector (Vector)
import Prelude

data DatasetParam f a = DatasetParam
  { numTrain :: a,
    numTest :: a,
    image :: f a,
    label :: f a
  }
  deriving stock (Eq, Show, Functor)

newtype StaticVar = StaticVar Int
  deriving newtype (Eq, Ord, Show)

data AssVarF sv
  = -- | Statically assigned variables.
    AssVarStatic sv
  | -- | Dynamically generated variables.
    AssVarDynamic Int
  deriving stock (Eq, Ord, Show, Functor)

-- | The type of symbols generated dynamically for hygienicity.
newtype Symbol = Symbol Int
  deriving newtype (Eq, Show)

symbolToVar :: Symbol -> AssVarF sv
symbolToVar (Symbol n) = AssVarDynamic n

newtype AssTypeVar = AssTypeVar Int
  deriving newtype (Eq, Ord, Show)

data AssLiteralF af sv
  = ALitInt Int
  | ALitFloat Double
  | ALitBool Bool
  | ALitUnit
  | ALitString Text
  | ALitList [af sv]
  | ALitVec Vector
  | ALitMat Matrix
  deriving stock (Eq, Show, Functor)

-- | The type of stage-0 expressions obtained by elaboration through typechecking.
data Ass0ExprF sv
  = A0Literal (AssLiteralF Ass0ExprF sv)
  | -- | Uses of built-in functions.
    A0BuiltInName BuiltIn
  | A0Var (AssVarF sv)
  | A0Lam (Maybe (AssVarF sv, StrictAss0TypeExprF sv)) (AssVarF sv, StrictAss0TypeExprF sv) (Ass0ExprF sv)
  | A0App (Ass0ExprF sv) (Ass0ExprF sv)
  | A0LetIn (AssVarF sv, StrictAss0TypeExprF sv) (Ass0ExprF sv) (Ass0ExprF sv)
  | A0LetTupleIn (AssVarF sv) (AssVarF sv) (Ass0ExprF sv) (Ass0ExprF sv)
  | A0Sequential (Ass0ExprF sv) (Ass0ExprF sv)
  | A0Tuple (Ass0ExprF sv) (Ass0ExprF sv)
  | A0IfThenElse (Ass0ExprF sv) (Ass0ExprF sv) (Ass0ExprF sv)
  | A0Bracket (Ass1ExprF sv)
  | A0TyEqAssert Span (Type1EquationF sv)
  | -- | Assertions for refinement predicates, where the first expression is a predicate,
    -- and the second is a target expression of the assertion.
    A0RefinementAssert Span (Ass0ExprF sv) (Ass0ExprF sv)
  | A0AppType (Ass0ExprF sv) (StrictAss0TypeExprF sv)
  deriving stock (Eq, Show, Functor)

-- | The type of stage-1 expressions obtained by elaboration through typechecking.
data Ass1ExprF sv
  = A1Literal (AssLiteralF Ass1ExprF sv)
  | A1Var (AssVarF sv)
  | A1BuiltInName Ass1BuiltIn
  | A1Lam (Maybe (AssVarF sv, Ass1TypeExprF sv)) (AssVarF sv, Ass1TypeExprF sv) (Ass1ExprF sv)
  | A1App (Ass1ExprF sv) (Ass1ExprF sv)
  | A1LetTupleIn (AssVarF sv) (AssVarF sv) (Ass1ExprF sv) (Ass1ExprF sv)
  | A1Sequential (Ass1ExprF sv) (Ass1ExprF sv)
  | A1Tuple (Ass1ExprF sv) (Ass1ExprF sv) -- TODO: generalize tuples
  | A1IfThenElse (Ass1ExprF sv) (Ass1ExprF sv) (Ass1ExprF sv)
  | A1Escape (Ass0ExprF sv)
  | A1AppType (Ass1ExprF sv) (Ass1TypeExprF sv)
  deriving stock (Eq, Show, Functor)

a1LetIn :: (AssVarF sv, Ass1TypeExprF sv) -> Ass1ExprF sv -> Ass1ExprF sv -> Ass1ExprF sv
a1LetIn (x, a1tye) a1e1 a1e2 =
  A1App (A1Lam Nothing (x, a1tye) a1e2) a1e1

-- | The type of bindings obtained by elaboration through typechecking.
data AssBindF sv
  = ABind0 (AssVarF sv, StrictAss0TypeExprF sv) (Ass0ExprF sv)
  | ABind1 (AssVarF sv, Ass1TypeExprF sv) (Ass1ExprF sv)
  deriving stock (Eq, Show, Functor)

makeExprFromBinds :: forall sv. [AssBindF sv] -> Ass0ExprF sv -> Ass0ExprF sv
makeExprFromBinds abinds' a0eFinal = go0 abinds'
  where
    go0 :: [AssBindF sv] -> Ass0ExprF sv
    go0 = \case
      [] -> a0eFinal
      ABind0 xsty a0e : abinds -> A0LetIn xsty a0e (go0 abinds)
      ABind1 xty a1e : abinds -> A0Bracket (a1LetIn xty a1e (go1 abinds))

    go1 :: [AssBindF sv] -> Ass1ExprF sv
    go1 = \case
      [] -> A1Escape a0eFinal
      ABind1 xty a1e : abinds -> a1LetIn xty a1e (go1 abinds)
      ABind0 xsty a0e : abinds -> A1Escape (A0LetIn xsty a0e (go0 abinds))

-- | The type of internal stage-0 type expressions for typechecking.
data Ass0TypeExprF sv
  = -- | Primitive types possibly equipped with a refinement predicate.
    A0TyPrim Ass0PrimType (Maybe (Ass0ExprF sv))
  | -- | List types possibly equipped with a refinement predicate.
    A0TyList (Ass0TypeExprF sv) (Maybe (Ass0ExprF sv))
  | A0TyVar AssTypeVar
  | A0TyProduct (Ass0TypeExprF sv) (Ass0TypeExprF sv) -- TODO: generalize product types
  | -- | (Possibly dependent) function types.
    A0TyArrow (Maybe (AssVarF sv), Ass0TypeExprF sv) (Ass0TypeExprF sv)
  | -- | Function types with an implicit parameter.
    A0TyOptArrow (AssVarF sv, Ass0TypeExprF sv) (Ass0TypeExprF sv)
  | A0TyCode (Ass1TypeExprF sv)
  | -- | Polymorphic types.
    A0TyImplicitForAll AssTypeVar (Ass0TypeExprF sv)
  deriving stock (Eq, Show, Functor)

-- | The type of type annotations in target terms.
-- Unlike @Ass0TypeExprF@, this does not contain function types with implicit parameters.
data StrictAss0TypeExprF sv
  = -- | Primitive types possibly equipped with a refinement predicate.
    SA0TyPrim Ass0PrimType (Maybe (Ass0ExprF sv))
  | -- | List types possibly equipped with a refinement predicate.
    SA0TyList (StrictAss0TypeExprF sv) (Maybe (Ass0ExprF sv))
  | SA0TyVar AssTypeVar
  | SA0TyProduct (StrictAss0TypeExprF sv) (StrictAss0TypeExprF sv) -- TODO: generalize product types
  | -- | (Possibly dependent) function types.
    SA0TyArrow (Maybe (AssVarF sv), StrictAss0TypeExprF sv) (StrictAss0TypeExprF sv)
  | SA0TyCode (Ass1TypeExprF sv)
  | -- | Polymorphic types.
    SA0TyExplicitForAll AssTypeVar (StrictAss0TypeExprF sv)
  deriving stock (Eq, Show, Functor)

data AssPrimBaseType
  = ATyPrimInt
  | ATyPrimFloat
  | ATyPrimUnit
  | ATyPrimBool
  | ATyPrimString
  | ATyPrimDevice
  | ATyPrimActivation
  | ATyPrimVarStore
  | ATyPrimOptimizer
  deriving stock (Eq, Show)

validatePrimBaseType :: Text -> Maybe AssPrimBaseType
validatePrimBaseType = \case
  "Int" -> Just ATyPrimInt
  "Float" -> Just ATyPrimFloat
  "Unit" -> Just ATyPrimUnit
  "Bool" -> Just ATyPrimBool
  "String" -> Just ATyPrimString
  "Device" -> Just ATyPrimDevice
  "Activation" -> Just ATyPrimActivation
  "VarStore" -> Just ATyPrimVarStore
  "Optimizer" -> Just ATyPrimOptimizer
  _ -> Nothing

data Ass0PrimType
  = A0TyPrimBase AssPrimBaseType
  | A0TyTensor [Int]
  | A0TyDataset (DatasetParam [] Int)
  deriving stock (Eq, Show)

-- | The type of stage-1 type expressions.
data Ass1TypeExprF sv
  = A1TyPrim (Ass1PrimTypeF sv)
  | A1TyList (Ass1TypeExprF sv)
  | A1TyVar AssTypeVar
  | A1TyProduct (Ass1TypeExprF sv) (Ass1TypeExprF sv) -- TODO: generalize product types
  | A1TyArrow (Ass1TypeExprF sv) (Ass1TypeExprF sv)
  | A1TyImplicitForAll AssTypeVar (Ass1TypeExprF sv)
  deriving stock (Eq, Show, Functor)

data Ass1PrimTypeF sv
  = A1TyPrimBase AssPrimBaseType
  | A1TyTensor (Ass0ExprF sv)
  | A1TyDataset (DatasetParam Identity (Ass0ExprF sv))
  deriving stock (Eq, Show, Functor)

-- | The type of types for persistent value items.
data AssPersTypeExpr
  = APersTyPrim Ass0PrimType
  | APersTyVar AssTypeVar
  | APersTyList AssPersTypeExpr
  | APersTyProduct AssPersTypeExpr AssPersTypeExpr
  | APersTyArrow AssPersTypeExpr AssPersTypeExpr
  | APersTyImplicitForAll AssTypeVar AssPersTypeExpr
  deriving stock (Eq, Show)

persistentTypeTo0 :: AssPersTypeExpr -> Ass0TypeExprF sv
persistentTypeTo0 = \case
  APersTyPrim a0tyPrim -> A0TyPrim a0tyPrim Nothing
  APersTyVar atyvar -> A0TyVar atyvar
  APersTyList aPtye -> A0TyList (persistentTypeTo0 aPtye) Nothing
  APersTyProduct aPtye1 aPtye2 -> A0TyProduct (persistentTypeTo0 aPtye1) (persistentTypeTo0 aPtye2)
  APersTyArrow aPtye1 aPtye2 -> A0TyArrow (Nothing, persistentTypeTo0 aPtye1) (persistentTypeTo0 aPtye2)
  APersTyImplicitForAll atyvar aPtye -> A0TyImplicitForAll atyvar (persistentTypeTo0 aPtye)

persistentTypeTo1 :: AssPersTypeExpr -> Ass1TypeExprF sv
persistentTypeTo1 = \case
  APersTyPrim a0tyPrim -> A1TyPrim (liftPrimType a0tyPrim)
  APersTyVar atyvar -> A1TyVar atyvar
  APersTyList aPtye -> A1TyList (persistentTypeTo1 aPtye)
  APersTyProduct aPtye1 aPtye2 -> A1TyProduct (persistentTypeTo1 aPtye1) (persistentTypeTo1 aPtye2)
  APersTyArrow aPtye1 aPtye2 -> A1TyArrow (persistentTypeTo1 aPtye1) (persistentTypeTo1 aPtye2)
  APersTyImplicitForAll atyvar aPtye -> A1TyImplicitForAll atyvar (persistentTypeTo1 aPtye)

liftPrimType :: Ass0PrimType -> Ass1PrimTypeF sv
liftPrimType = \case
  A0TyPrimBase tyPrimBase ->
    A1TyPrimBase tyPrimBase
  A0TyTensor ns ->
    A1TyTensor (liftIntList ns)
  A0TyDataset DatasetParam {numTrain, numTest, image, label} ->
    A1TyDataset
      DatasetParam
        { numTrain = liftInt numTrain,
          numTest = liftInt numTest,
          image = Identity (liftIntList image),
          label = Identity (liftIntList label)
        }
  where
    liftInt = A0Literal . ALitInt
    liftIntList = A0Literal . ALitList . map liftInt

-- | The type of stage-0 term values.
data Ass0ValF sv
  = A0ValLiteral (AssLiteralF Ass0ValF sv)
  | A0ValTuple (Ass0ValF sv) (Ass0ValF sv)
  | -- | Function closures.
    A0ValLam (Maybe (AssVarF sv, Ass0TypeValF sv)) (AssVarF sv, Ass0TypeValF sv) (Ass0ExprF sv) EvalEnv
  | -- | code fragments.
    A0ValBracket (Ass1ValF sv)
  | -- | Possibly partially applied built-in functions.
    A0ValPartialBuiltInApp (Ass0PartialBuiltInApp (Ass0ValF sv))
  deriving stock (Eq, Show, Functor)

-- | The type of stage-1 term values.
data Ass1ValF sv
  = A1ValLiteral (AssLiteralF Ass1ValF sv)
  | A1ValConst Ass1BuiltIn
  | A1ValVar Symbol
  | A1ValLam (Maybe (Symbol, Ass1TypeValF sv)) (Symbol, Ass1TypeValF sv) (Ass1ValF sv)
  | A1ValApp (Ass1ValF sv) (Ass1ValF sv)
  | A1ValLetTupleIn Symbol Symbol (Ass1ValF sv) (Ass1ValF sv)
  | A1ValSequential (Ass1ValF sv) (Ass1ValF sv)
  | A1ValTuple (Ass1ValF sv) (Ass1ValF sv)
  | A1ValIfThenElse (Ass1ValF sv) (Ass1ValF sv) (Ass1ValF sv)
  deriving stock (Eq, Show, Functor)

-- | The type of stage-0 type values.
data Ass0TypeValF sv
  = -- | Primitive types possibly equipped with a refinement predicate.
    A0TyValPrim Ass0PrimTypeVal (Maybe (Ass0ValF sv))
  | -- | List types possibly equipped with a refinement predicate.
    A0TyValList (Ass0TypeValF sv) (Maybe (Ass0ValF sv))
  | A0TyValVar AssTypeVar
  | A0TyValProduct (Ass0TypeValF sv) (Ass0TypeValF sv)
  | A0TyValArrow (Maybe (AssVarF sv), Ass0TypeValF sv) (StrictAss0TypeExprF sv)
  | A0TyValCode (Ass1TypeValF sv)
  | A0TyValExplicitForAll AssTypeVar (StrictAss0TypeExprF sv)
  deriving stock (Eq, Show, Functor)

data Ass0PrimTypeVal
  = A0TyValPrimBase AssPrimBaseType
  | A0TyValTensor [Int]
  | A0TyValDataset (DatasetParam [] Int)
  deriving stock (Eq, Show)

-- | The type of stage-1 type values.
data Ass1TypeValF sv
  = A1TyValPrim Ass1PrimTypeVal
  | A1TyValList (Ass1TypeValF sv)
  | A1TyValVar AssTypeVar
  | A1TyValProduct (Ass1TypeValF sv) (Ass1TypeValF sv)
  | A1TyValArrow (Ass1TypeValF sv) (Ass1TypeValF sv)
  | A1TyValImplicitForAll AssTypeVar (Ass1TypeExprF sv)
  deriving stock (Eq, Show, Functor)

data Ass1PrimTypeVal
  = A1TyValPrimBase AssPrimBaseType
  | A1TyValTensor [Int]
  | A1TyValDataset (DatasetParam [] Int)
  deriving stock (Eq, Show)

-- | The type of well-formed type equations for assertion.
data Type1EquationF sv
  = TyEq1Prim (Type1PrimEquationF sv)
  | TyEq1List (Type1EquationF sv)
  | TyEq1Arrow (Type1EquationF sv) (Type1EquationF sv)
  | TyEq1Product (Type1EquationF sv) (Type1EquationF sv)
  deriving stock (Eq, Show, Functor)

data Type1PrimEquationF sv
  = TyEq1PrimBase AssPrimBaseType
  | TyEq1TensorByLiteral [(Ass0ExprF sv, Ass0ExprF sv)]
  | -- | Pairs of expressions of type `List Nat`.
    TyEq1TensorByWhole (Ass0ExprF sv) (Ass0ExprF sv)
  | TyEq1Dataset (DatasetParam Identity (Ass0ExprF sv)) (DatasetParam Identity (Ass0ExprF sv))
  deriving stock (Eq, Show, Functor)

type EvalEnv = Map AssVar EvalEnvEntry

data EvalEnvEntry
  = Ass0ValEntry Ass0Val
  | SymbolEntry Symbol
  deriving stock (Eq, Show)

mapAssLiteral :: (e1 sv -> e2 sv) -> AssLiteralF e1 sv -> AssLiteralF e2 sv
mapAssLiteral f = \case
  ALitInt n -> ALitInt n
  ALitFloat r -> ALitFloat r
  ALitBool b -> ALitBool b
  ALitUnit -> ALitUnit
  ALitString t -> ALitString t
  ALitList es -> ALitList (map f es)
  ALitVec vec -> ALitVec vec
  ALitMat mat -> ALitMat mat

mapMAssLiteral :: (Monad m) => (e sv -> m (v sv)) -> AssLiteralF e sv -> m (AssLiteralF v sv)
mapMAssLiteral eval = \case
  ALitInt n -> pure $ ALitInt n
  ALitFloat r -> pure $ ALitFloat r
  ALitBool b -> pure $ ALitBool b
  ALitUnit -> pure ALitUnit
  ALitString t -> pure $ ALitString t
  ALitList a0es -> ALitList <$> mapM eval a0es
  ALitVec vec -> pure $ ALitVec vec
  ALitMat mat -> pure $ ALitMat mat

strictify :: Ass0TypeExprF sv -> StrictAss0TypeExprF sv
strictify = \case
  A0TyPrim a0tyPrim maybePred -> SA0TyPrim a0tyPrim maybePred
  A0TyVar atyvar -> SA0TyVar atyvar
  A0TyList a0tye maybePred -> SA0TyList (strictify a0tye) maybePred
  A0TyProduct a0tye1 a0tye2 -> SA0TyProduct (strictify a0tye1) (strictify a0tye2)
  A0TyArrow (x1opt, a0tye1) a0tye2 -> SA0TyArrow (x1opt, strictify a0tye1) (strictify a0tye2)
  A0TyCode a1tye1 -> SA0TyCode a1tye1
  A0TyOptArrow (x1, a0tye1) a0tye2 -> SA0TyArrow (Just x1, strictify a0tye1) (strictify a0tye2)
  A0TyImplicitForAll atyvar a0tye -> SA0TyExplicitForAll atyvar (strictify a0tye)

a0TyVec :: Int -> Ass0PrimType
a0TyVec n = A0TyTensor [n]

a0TyMat :: Int -> Int -> Ass0PrimType
a0TyMat m n = A0TyTensor [m, n]

a1TyVec :: Ass0ExprF ann -> Ass1PrimTypeF ann
a1TyVec a0e = A1TyTensor (A0Literal (ALitList [a0e]))

a1TyMat :: Ass0ExprF ann -> Ass0ExprF ann -> Ass1PrimTypeF ann
a1TyMat a0e1 a0e2 = A1TyTensor (A0Literal (ALitList [a0e1, a0e2]))

decomposeType1Equation :: Type1EquationF sv -> (Ass1TypeExprF sv, Ass1TypeExprF sv)
decomposeType1Equation = \case
  TyEq1Prim ty1eqPrim ->
    case ty1eqPrim of
      TyEq1PrimBase tyPrimBase ->
        prims $ A1TyPrimBase tyPrimBase
      TyEq1TensorByLiteral zipped ->
        let a0eList1 = A0Literal (ALitList (map fst zipped))
            a0eList2 = A0Literal (ALitList (map snd zipped))
         in (A1TyPrim (A1TyTensor a0eList1), A1TyPrim (A1TyTensor a0eList2))
      TyEq1TensorByWhole a0eList1 a0eList2 ->
        (A1TyPrim (A1TyTensor a0eList1), A1TyPrim (A1TyTensor a0eList2))
      TyEq1Dataset datasetParam1 datasetParam2 ->
        (A1TyPrim (A1TyDataset datasetParam1), A1TyPrim (A1TyDataset datasetParam2))
  TyEq1List ty1eqElem ->
    let (a1tye1elem, a1tye2elem) = decomposeType1Equation ty1eqElem
     in (A1TyList a1tye1elem, A1TyList a1tye2elem)
  TyEq1Arrow ty1eqDom ty1eqCod ->
    let (a1tye11, a1tye21) = decomposeType1Equation ty1eqDom
        (a1tye12, a1tye22) = decomposeType1Equation ty1eqCod
     in (A1TyArrow a1tye11 a1tye12, A1TyArrow a1tye21 a1tye22)
  TyEq1Product ty1eq1 ty1eq2 ->
    let (a1tye11, a1tye21) = decomposeType1Equation ty1eq1
        (a1tye12, a1tye22) = decomposeType1Equation ty1eq2
     in (A1TyProduct a1tye11 a1tye12, A1TyProduct a1tye21 a1tye22)
  where
    prims p = (A1TyPrim p, A1TyPrim p)

-- | The type of application contexts, which play a key role of
-- the "Let arguments go first" [Xie & Oliveira 2018] formalization.
type AppContextF sv = [AppContextEntryF sv]

data AppContextEntryF sv
  = AppArg0 (Ass0ExprF sv) (Ass0TypeExprF sv)
  | AppArg1 (Ass1TypeExprF sv)
  | AppArgOptGiven0 (Ass0ExprF sv) (Ass0TypeExprF sv)
  | AppArgOptOmitted0
  deriving (Eq, Show, Functor)

-- | The type of the results of the "Let arguments go first" traversal.
data ResultF af sv
  = Pure (af sv)
  | Cast0 (Maybe (Ass0ExprF sv)) (Ass0TypeExprF sv) (ResultF af sv)
  | Cast1 (Maybe (Ass0ExprF sv)) (Ass1TypeExprF sv) (ResultF af sv)
  | CastGiven0 (Maybe (Ass0ExprF sv)) (Ass0TypeExprF sv) (ResultF af sv)
  | FillInferred0 (Ass0ExprF sv) (ResultF af sv)
  | InsertInferred0 (Ass0ExprF sv) (ResultF af sv)
  | InsertInferredType0 (Ass0TypeExprF sv) (ResultF af sv)
  | InsertType1 (Ass1TypeExprF sv) (ResultF af sv)
  deriving (Eq, Show, Functor)

type AssVar = AssVarF StaticVar

type Ass0Expr = Ass0ExprF StaticVar

type Ass1Expr = Ass1ExprF StaticVar

type AssBind = AssBindF StaticVar

type Type1Equation = Type1EquationF StaticVar

type Ass0TypeExpr = Ass0TypeExprF StaticVar

type StrictAss0TypeExpr = StrictAss0TypeExprF StaticVar

type Ass1TypeExpr = Ass1TypeExprF StaticVar

type Ass1PrimType = Ass1PrimTypeF StaticVar

type Ass0Val = Ass0ValF StaticVar

type Ass1Val = Ass1ValF StaticVar

type Ass0TypeVal = Ass0TypeValF StaticVar

type Ass1TypeVal = Ass1TypeValF StaticVar

type AppContext = AppContextF StaticVar

type Result0 = ResultF Ass0TypeExprF StaticVar

type Result1 = ResultF Ass1TypeExprF StaticVar
