module Staged.Formatter
  ( Disp (..),
    render,
    putRenderedLines,
    renderAtStage0,
    putRenderedLinesAtStage0,
    putRenderedLinesAtStage1,
  )
where

import Data.Functor.Identity
import Data.List qualified as List
import Data.Text (Text)
import Data.Text qualified as Text
import Prettyprinter
import Prettyprinter.Render.Terminal
import Staged.BuiltIn.Core
import Staged.EvalError
import Staged.SrcSyntax
import Staged.Syntax
import Staged.TypeError
import Surface.BindingTime.Analyzer qualified as Bta
import Surface.BindingTime.Core qualified as Bta
import Surface.BindingTime.Stager qualified as Bta
import Surface.Syntax qualified as Surface
import Util.FrontError (FrontError (..))
import Util.LocationInFile (LocationInFile (LocationInFile), SpanInFile (..))
import Util.Matrix qualified as Matrix
import Util.ParserUtil (ParseError (..))
import Util.Vector qualified as Vector
import Prelude

type Ann = AnsiStyle

bindingTime0Style :: Doc Ann -> Doc Ann
bindingTime0Style = annotate (color Green)

bindingTime1Style :: Doc Ann -> Doc Ann
bindingTime1Style = annotate (color Red)

stage0Style :: Doc Ann -> Doc Ann
stage0Style = annotate (color Cyan) -- reAnnotate (<> color Cyan)

stage1Style :: Doc Ann -> Doc Ann
stage1Style = annotate (color Magenta) -- reAnnotate (<> color Magenta)

stagingOperatorStyle :: Doc Ann -> Doc Ann
stagingOperatorStyle = annotate (color Yellow)

assertionStyle :: Doc Ann -> Doc Ann
assertionStyle = id -- annotate (bgColorDull Blue)

data Associativity
  = Atomic
  | FunDomain
  | Outermost
  deriving (Eq, Ord) -- `Atomic` is the smallest

class Disp a where
  dispGen :: Associativity -> a -> Doc Ann
  disp :: a -> Doc Ann
  disp = dispGen Outermost

renderDoc :: Int -> Doc Ann -> Text
renderDoc wid doc =
  renderStrict $
    layoutSmart (LayoutOptions {layoutPageWidth = AvailablePerLine wid 1.0}) doc

render :: (Disp a) => Int -> a -> Text
render wid = renderDoc wid . disp

putRenderedLines :: (Disp a) => Int -> a -> IO ()
putRenderedLines wid x =
  putStrLn $ Text.unpack $ render wid x

renderAtStage0 :: (Disp a) => Int -> a -> Text
renderAtStage0 wid = renderDoc wid . stage0Style . disp

putRenderedLinesAtStage0 :: (Disp a) => Int -> a -> IO ()
putRenderedLinesAtStage0 wid x =
  putStrLn $ Text.unpack $ renderAtStage0 wid x

renderAtStage1 :: (Disp a) => Int -> a -> Text
renderAtStage1 wid = renderDoc wid . stage1Style . disp

putRenderedLinesAtStage1 :: (Disp a) => Int -> a -> IO ()
putRenderedLinesAtStage1 wid x =
  putStrLn $ Text.unpack $ renderAtStage1 wid x

commaSep :: [Doc Ann] -> Doc Ann
commaSep = sep . punctuate comma

disps :: (Disp a) => [a] -> Doc Ann
disps [] = mempty
disps (first : rest) = List.foldl' (\doc x -> doc <> "," <+> disp x) (disp first) rest

deepenParenWhen :: Bool -> Doc Ann -> Doc Ann
deepenParenWhen b doc = if b then "(" <> nest 2 doc <> ")" else doc

dispNonrecLam :: (Disp var, Disp ty, Disp expr) => Associativity -> Maybe Label -> var -> ty -> expr -> Doc Ann
dispNonrecLam req labelOpt x tye1 e2 =
  deepenParenWhen (req <= FunDomain) $
    group (doc <+> ":" <+> disp tye1 <> "." <> nest 2 (line <> disp e2))
  where
    doc =
      case labelOpt of
        Nothing -> "λ" <> disp x
        Just label -> "λ" <+> "#" <> disp label <+> disp x

dispRecLam :: (Disp var, Disp ty, Disp expr) => Associativity -> var -> ty -> Maybe Label -> var -> ty -> expr -> Doc Ann
dispRecLam req f tyeRec labelOpt x tye1 e2 =
  deepenParenWhen (req <= FunDomain) $
    group (docBinderF <+> docBinderX <> nest 2 (line <> disp e2))
  where
    docBinderF = "rec" <+> disp f <+> ":" <+> disp tyeRec <> "."
    docBinderX = doc <+> ":" <+> disp tye1 <> "."
    doc =
      case labelOpt of
        Nothing -> "λ" <> disp x
        Just label -> "λ" <+> "#" <> disp label <+> disp x

dispLamOpt :: (Disp var, Disp ty, Disp expr) => Associativity -> var -> ty -> expr -> Doc Ann
dispLamOpt req x tye1 e2 =
  deepenParenWhen (req <= FunDomain) $
    group ("λ{" <> disp x <+> ":" <+> disp tye1 <> "}." <> nest 2 (line <> disp e2))

dispApp :: (Disp expr) => Associativity -> expr -> Maybe Label -> expr -> Doc Ann
dispApp req e1 labelOpt e2 =
  deepenParenWhen (req <= Atomic) $
    group $
      case labelOpt of
        Nothing -> doc1 <> nest 2 (line <> doc2)
        Just label -> doc1 <+> "#" <> disp label <> nest 2 (line <> doc2)
  where
    doc1 = dispGen FunDomain e1
    doc2 = dispGen Atomic e2

dispAppOptGiven :: (Disp expr) => Associativity -> expr -> expr -> Doc Ann
dispAppOptGiven req e1 e2 =
  deepenParenWhen (req <= Atomic) $
    group (dispGen FunDomain e1 <> nest 2 (line <> "{" <> disp e2 <> "}"))

dispAppOptOmitted :: (Disp expr) => Associativity -> expr -> Doc Ann
dispAppOptOmitted req e1 =
  deepenParenWhen (req <= Atomic) $
    group (dispGen FunDomain e1 <> nest 2 (line <> "_"))

dispAppType :: (Disp expr, Disp ty) => Associativity -> expr -> ty -> Doc Ann
dispAppType req e1 tye2 =
  deepenParenWhen (req <= Atomic) $
    group (dispGen FunDomain e1 <> nest 2 (line <> dispGen Atomic tye2))

dispLetIn :: (Disp var, Disp param, Disp expr) => Associativity -> var -> [param] -> expr -> expr -> Doc Ann
dispLetIn req x params e1 e2 =
  deepenParenWhen (req <= FunDomain) $
    group ("let" <+> d <+> "=" <> nest 2 (line <> disp e1) <+> "in" <> line <> disp e2)
  where
    d = sep (disp x : map disp params)

dispLetRecIn :: (Disp var, Disp param, Disp ty, Disp expr) => Associativity -> var -> [param] -> ty -> expr -> expr -> Doc Ann
dispLetRecIn req x params tye e1 e2 =
  deepenParenWhen (req <= FunDomain) $
    group ("let" <+> "rec" <+> d <+> ":" <+> disp tye <+> "=" <> nest 2 (line <> disp e1) <+> "in" <> line <> disp e2)
  where
    d = sep (disp x : map disp params)

dispLetInWithAnnot :: (Disp var, Disp ty, Disp expr) => Associativity -> var -> ty -> expr -> expr -> Doc Ann
dispLetInWithAnnot req x tye e1 e2 =
  deepenParenWhen (req <= FunDomain) $
    group ("let" <+> disp x <+> ":" <+> disp tye <+> "=" <> nest 2 (line <> disp e1) <+> "in" <> line <> disp e2)

dispLetOpenIn :: (Disp var, Disp expr) => Associativity -> var -> expr -> Doc Ann
dispLetOpenIn req m e =
  deepenParenWhen (req <= FunDomain) $
    group ("let open" <+> disp m <+> "in" <> line <> disp e)

dispLetTupleIn :: (Disp var, Disp expr) => Associativity -> var -> var -> expr -> expr -> Doc Ann
dispLetTupleIn req xL xR e1 e2 =
  deepenParenWhen (req <= FunDomain) $
    group ("let (" <> disp xL <> "," <+> disp xR <> ") =" <+> disp e1 <+> "in" <> line <> disp e2)

dispSequential :: (Disp expr) => Associativity -> expr -> expr -> Doc Ann
dispSequential req e1 e2 =
  deepenParenWhen (req <= FunDomain) $
    group (disp e1 <> ";" <> line <> disp e2)

dispTuple :: (Disp expr) => expr -> expr -> Doc Ann
dispTuple e1 e2 =
  "(" <> nest 2 (disp e1 <> "," <+> disp e2) <> ")"

dispIfThenElse :: (Disp expr) => Associativity -> expr -> expr -> expr -> Doc Ann
dispIfThenElse req e0 e1 e2 =
  deepenParenWhen (req <= FunDomain) $
    group (docIf <+> docThen <+> docElse)
  where
    docIf = "if" <> nest 2 (line <> disp e0)
    docThen = "then" <> nest 2 (line <> disp e1)
    docElse = "else" <> nest 2 (line <> disp e2)

dispStringLiteral :: Text -> Doc Ann
dispStringLiteral t = "\"" <> disp t <> "\"" -- TODO: escape special characters

dispAs :: (Disp expr, Disp ty) => Associativity -> expr -> ty -> Doc Ann
dispAs req e1 tye2 =
  deepenParenWhen (req <= FunDomain) $ group (disp e1 <+> "as" <+> disp tye2)

dispPersistent :: (Disp expr) => expr -> Doc Ann
dispPersistent e =
  stagingOperatorStyle "%" <> stage0Style (dispGen Atomic e)

dispPersistentListLiteral :: (Disp expr) => [expr] -> Doc Ann
dispPersistentListLiteral es =
  stagingOperatorStyle "%" <> stage0Style (dispListLiteral es)

dispBracket :: (Disp expr) => expr -> Doc Ann
dispBracket e =
  stagingOperatorStyle "&" <> stage1Style (dispGen Atomic e)

dispEscape :: (Disp expr) => expr -> Doc Ann
dispEscape e =
  stagingOperatorStyle "~" <> stage0Style (dispGen Atomic e)

dispTypeVar :: AssTypeVar -> Doc Ann
dispTypeVar (AssTypeVar n) = "'a" <> disp n

dispForAllType :: (Disp ty) => Associativity -> AssTypeVar -> ty -> Doc Ann
dispForAllType req atyvar tye =
  deepenParenWhen (req <= Atomic) $
    group ("forall" <+> dispTypeVar atyvar <> "." <+> disp tye)

dispListType :: (Disp ty) => Associativity -> ty -> Doc Ann
dispListType req tye =
  deepenParenWhen (req <= Atomic) $
    group ("List" <+> dispGen Atomic tye)

dispProductType :: (Disp ty) => Associativity -> ty -> ty -> Doc Ann
dispProductType req tye1 tye2 =
  deepenParenWhen (req <= Atomic) $
    group (dispGen Atomic tye1 <+> "*" <+> dispGen Atomic tye2)

dispArrowType :: (Disp var, Disp ty1, Disp ty2) => Associativity -> Maybe var -> ty1 -> ty2 -> Doc Ann
dispArrowType req xOpt tye1 tye2 =
  deepenParenWhen (req <= FunDomain) $
    group (docDom <> " ->" <> line <> disp tye2)
  where
    docDom =
      case xOpt of
        Just x -> "(" <> disp x <+> ":" <+> disp tye1 <> ")"
        Nothing -> dispGen FunDomain tye1

dispNondepArrowType :: (Disp ty) => Associativity -> ty -> ty -> Doc Ann
dispNondepArrowType req =
  dispArrowType req (Nothing :: Maybe Text)

dispOptArrowType :: (Disp var, Disp ty1, Disp ty2) => Associativity -> var -> ty1 -> ty2 -> Doc Ann
dispOptArrowType req x tye1 tye2 =
  deepenParenWhen (req <= FunDomain) $
    group (docDom <> " ->" <> line <> disp tye2)
  where
    docDom = "{" <> disp x <+> ":" <+> disp tye1 <> "}"

dispInternalRefinementType :: (Disp ty, Disp expr) => Associativity -> ty -> expr -> Doc Ann
dispInternalRefinementType _req tye ePred =
  "(" <> disp tye <+> "|" <+> disp ePred <> ")"

dispInternalRefinementListType :: (Disp ty, Disp expr) => Associativity -> ty -> expr -> Doc Ann
dispInternalRefinementListType _req tye ePred =
  "(" <> dispListType Outermost tye <+> "|" <+> disp ePred <> ")"

dispListLiteral :: (Disp e) => [e] -> Doc Ann
dispListLiteral es =
  "[" <> disps es <> "]"

dispVectorLiteral :: [Int] -> Doc Ann
dispVectorLiteral ns =
  encloseSep ("[|" <> space) (space <> "|]") (";" <> softline) (disp <$> ns)

dispMatrixLiteral :: [[Int]] -> Doc Ann
dispMatrixLiteral nss =
  encloseSep ("[#" <> space) (space <> "#]") (";" <> softline) (dispRowContents <$> nss)

dispRowContents :: [Int] -> Doc Ann
dispRowContents row =
  commaSep (disp <$> row)

dispNameWithArgs :: Associativity -> Doc Ann -> (arg -> Doc Ann) -> [arg] -> Doc Ann
dispNameWithArgs req name dispArg args =
  case args of
    [] -> name
    _ : _ -> deepenParenWhen (req <= Atomic) (List.foldl' (<+>) name (map dispArg args))

dispDatasetParam :: (a -> Doc Ann) -> (f a -> Doc Ann) -> DatasetParam f a -> Doc Ann
dispDatasetParam dispElem dispList DatasetParam {numTrain, numTest, image, label} =
  dispElem numTrain <> " " <> dispElem numTest <> " " <> dispList image <> " " <> dispList label

dispLongName :: (Disp var) => [var] -> var -> Doc Ann
dispLongName ms x =
  foldr (\m doc -> disp m <> "." <> doc) (disp x) ms

instance Disp Text where
  dispGen _ = pretty

instance Disp String where
  dispGen _ = pretty

instance Disp Int where
  dispGen _ = pretty

instance (Disp sv) => Disp (AssVarF sv) where
  dispGen _ (AssVarStatic x) = disp x
  dispGen _ (AssVarDynamic n) = "#S" <> disp n

instance Disp Symbol where
  dispGen _ (Symbol n) = "#S" <> disp n

instance (Disp e) => Disp (Literal e) where
  dispGen _ = \case
    LitInt n -> pretty n
    LitFloat r -> pretty r
    LitUnit -> "()"
    LitBool b -> if b then "true" else "false"
    LitString t -> dispStringLiteral t
    LitList es -> dispListLiteral es
    LitVec ns -> dispVectorLiteral ns
    LitMat nss -> dispMatrixLiteral nss

instance Disp (ExprF ann) where
  dispGen req (Expr _ann exprMain) = dispGen req exprMain

instance Disp (ExprMainF ann) where
  dispGen req = \case
    Literal lit -> dispGen req lit
    Var (ms, x) -> dispLongName ms x
    Lam Nothing labelOpt (x, tye1) e2 -> dispNonrecLam req labelOpt x tye1 e2
    Lam (Just (f, tyeRec)) labelOpt (x, tye1) e2 -> dispRecLam req f tyeRec labelOpt x tye1 e2
    App e1 labelOpt e2 -> dispApp req e1 labelOpt e2
    LamOpt (x, tye1) e2 -> dispLamOpt req x tye1 e2
    AppOptGiven e1 e2 -> dispAppOptGiven req e1 e2
    AppOptOmitted e1 -> dispAppOptOmitted req e1
    LetIn x params e1 e2 -> dispLetIn req x params e1 e2
    LetRecIn x params tye e1 e2 -> dispLetRecIn req x params tye e1 e2
    LetTupleIn xL xR e1 e2 -> dispLetTupleIn req xL xR e1 e2
    LetOpenIn m e -> dispLetOpenIn req m e
    Sequential e1 e2 -> dispSequential req e1 e2
    Tuple e1 e2 -> dispTuple e1 e2
    IfThenElse e0 e1 e2 -> dispIfThenElse req e0 e1 e2
    As e1 tye2 -> dispAs req e1 tye2
    Bracket e1 -> dispBracket e1
    Escape e1 -> dispEscape e1

instance Disp (LamBinderF ann) where
  dispGen _ = \case
    MandatoryBinder Nothing (x, tye) -> "(" <> disp x <+> ":" <+> disp tye <> ")"
    MandatoryBinder (Just label) (x, tye) -> "(#" <> disp label <+> disp x <+> ":" <+> disp tye <> ")"
    OptionalBinder (x, tye) -> "{" <> disp x <+> ":" <+> disp tye <> "}"

instance Disp (TypeExprF ann) where
  dispGen req (TypeExpr _ann typeExprMain) = dispGen req typeExprMain

instance Disp (TypeExprMainF ann) where
  dispGen req = \case
    TyName tyName args -> dispNameWithArgs req (disp tyName) (dispGen Atomic) args
    TyVar (TypeVar tyvar) -> "'" <> disp tyvar
    TyArrow (xOpt, tye1) tye2 -> dispArrowType req xOpt tye1 tye2
    TyCode tye1 -> dispBracket tye1
    TyOptArrow (x, tye1) tye2 -> dispOptArrowType req x tye1 tye2
    TyRefinement x tye1 e2 -> "(" <> disp x <+> ":" <+> disp tye1 <+> "|" <+> disp e2 <+> ")"
    TyProduct tye1 tye2 -> dispProductType req tye1 tye2
    TyForAll (TypeVar tyvar) tye -> "forall '" <> disp tyvar <+> "->" <+> disp tye

instance Disp (ArgForTypeF ann) where
  dispGen req = \case
    ExprArgPersistent e -> dispPersistent e
    ExprArgNormal e -> dispGen req e
    TypeArg tye -> dispGen req tye

instance Disp BuiltInArity1 where
  dispGen _ = \case
    BIGenVadd -> "GEN_VADD"
    BIMtranspose m n -> "MTRANSPOSE@{" <> disps [m, n] <> "}"
    BIDeviceGenCudaIfAvailable -> "DEVICE.GEN_CUDA_IF_AVAILABLE"
    BITensorGenZeros -> "TENSOR.GEN_ZEROS"
    BITensorGenGrad -> "TENSOR.GEN_GRAD"
    BITensorGenZeroGrad -> "TENSOR.GEN_ZERO_GRAD"
    BITensorGenSubUpdate -> "TENSOR.GEN_SUB_UPDATE"
    BITensorGenCountEqual -> "TENSOR.GEN_COUNT_EQUAL"
    BITensorGenDropout -> "TENSOR.GEN_DROPOUT"
    BITupleFirst -> "FST"
    BITupleSecond -> "SND"

instance Disp BuiltInArity2 where
  dispGen _ = \case
    BIAdd -> "+"
    BISub -> "-"
    BIMult -> "*"
    BIDiv -> "//"
    BIMod -> "mod"
    BILeq -> "<="
    BIEqual -> "=="
    BIAnd -> "&&"
    BIListMap -> "LIST.MAP"
    BIGenVconcat -> "GEN_VCONCAT"
    BIGenMtranspose -> "GEN_MTRANSPOSE"
    BIVadd n -> "VADD@{" <> disp n <> "}"
    BIVconcat m n -> "VCONCAT@{" <> disps [m, n] <> "}"
    BIMconcatVert m1 m2 n -> "MCONCAT_VERT@{" <> disps [m1, m2, n] <> "}"
    BIDropAt -> "DROP_AT"
    BIBroadcastable -> "BROADCASTABLE"
    BIBroadcast -> "BROADCAST"
    BIReshapeable -> "RESHAPEABLE"
    BIListCons -> "::"
    BIListAppend -> "LIST.APPEND"
    BIListIter -> "LIST.ITER"
    BITensorGenAdd -> "TENSOR.GEN_ADD"
    BITensorGenMult -> "TENSOR.GEN_MULT"
    BITensorGenArgmax -> "TENSOR.GEN_ARGMAX"
    BITensorGenCrossEntropyForLogits -> "TENSOR.GEN_CROSS_ENTROPY_FOR_LOGITS"
    BITensorAdd ns -> "TENSOR.ADD@{" <> dispListLiteral ns <> "}"
    BITensorMm k m n -> "TENSOR.MM@{" <> disps [k, m, n] <> "}"
    BITensorGenReshape -> "TENSOR.GEN_RESHAPE"
    BILayerGenForward -> "LAYER.GEN_FORWARD"

instance Disp BuiltInArity3 where
  dispGen _ = \case
    BIGenMconcatVert -> "GEN_MCONCAT_VERT"
    BITensorGenMm -> "TENSOR.GEN_MM"
    BILayerGenLinear -> "LAYER.GEN_LINEAR"

instance Disp BuiltInArity5 where
  dispGen _ = \case
    BIDatasetHelperGenTrainBatch -> "DATASET_HELPER.GEN_TRAIN_BATCH"

instance Disp BuiltInArity7 where
  dispGen _ = \case
    BIDatasetHelperGenBatchAccuracy -> "DATASET_HELPER.GEN_BATCH_ACCURACY"

instance Disp BuiltInArity8 where
  dispGen _ = \case
    BILayerGenConv2d -> "LAYER.GEN_CONV2D"

instance Disp BuiltInArity10 where
  dispGen _ = \case
    BITensorGenMaxPool2d -> "TENSOR.GEN_MAX_POOL2D"

instance Disp BuiltIn where
  dispGen req = \case
    BuiltInArity1 bi1 -> dispGen req bi1
    BuiltInArity2 bi2 -> dispGen req bi2
    BuiltInArity3 bi3 -> dispGen req bi3
    BuiltInArity5 bi5 -> dispGen req bi5
    BuiltInArity7 bi7 -> dispGen req bi7
    BuiltInArity8 bi8 -> dispGen req bi8
    BuiltInArity10 bi10 -> dispGen req bi10
    BuiltInOther s -> "OTHER '" <> disp s <> "'"

instance (Disp e) => Disp (Surface.Literal e) where
  dispGen _ = \case
    Surface.LitInt n -> pretty n
    Surface.LitFloat r -> pretty r
    Surface.LitUnit -> "()"
    Surface.LitBool b -> if b then "true" else "false"
    Surface.LitString t -> dispStringLiteral t
    Surface.LitList es -> dispListLiteral es
    Surface.LitVec ns -> dispVectorLiteral ns
    Surface.LitMat nss -> dispMatrixLiteral nss

instance Disp Surface.Expr where
  dispGen req (Surface.Expr _ann exprMain) = dispGen req exprMain

instance Disp Surface.ExprMain where
  dispGen req = \case
    Surface.Literal lit -> dispGen req lit
    Surface.Var (ms, x) -> dispLongName ms x
    Surface.Lam Nothing (x, tye1) e2 -> dispNonrecLam req Nothing x tye1 e2
    Surface.Lam (Just (f, tyeRec)) (x, tye1) e2 -> dispRecLam req f tyeRec Nothing x tye1 e2
    Surface.App e1 e2 -> dispApp req e1 Nothing e2
    Surface.LetIn x params eBody e2 -> dispLetIn req x params eBody e2
    Surface.LetRecIn f params tyeBody eBody e2 -> dispLetRecIn req f params tyeBody eBody e2
    Surface.LetTupleIn xL xR e1 e2 -> dispLetTupleIn req xL xR e1 e2
    Surface.LetOpenIn m e -> dispLetOpenIn req m e
    Surface.Sequential e1 e2 -> dispSequential req e1 e2
    Surface.Tuple e1 e2 -> dispTuple e1 e2
    Surface.IfThenElse e0 e1 e2 -> dispIfThenElse req e0 e1 e2
    Surface.As e1 tye2 -> dispAs req e1 tye2
    Surface.LamOpt (x, tye1) e2 -> dispLamOpt req x tye1 e2
    Surface.AppOptGiven e1 e2 -> dispAppOptGiven req e1 e2
    Surface.AppOptOmitted e1 -> dispAppOptOmitted req e1

instance Disp Surface.LamBinder where
  dispGen _ = \case
    Surface.MandatoryBinder (x, tye) -> "(" <> disp x <+> ":" <+> disp tye <> ")"
    Surface.OptionalBinder (x, tye) -> "{" <> disp x <+> ":" <+> disp tye <> "}"

instance Disp Surface.TypeExpr where
  dispGen req (Surface.TypeExpr _ann typeExprMain) = dispGen req typeExprMain

instance Disp Surface.TypeExprMain where
  dispGen req = \case
    Surface.TyName tyName args -> dispNameWithArgs req (disp tyName) (dispGen Atomic) args
    Surface.TyArrow (xOpt, tye1) tye2 -> dispArrowType req xOpt tye1 tye2
    Surface.TyOptArrow (x, tye1) tye2 -> dispOptArrowType req x tye1 tye2
    Surface.TyProduct tye1 tye2 -> dispProductType req tye1 tye2

instance Disp Surface.ArgForType where
  dispGen req = \case
    Surface.ExprArg e -> dispGen req e
    Surface.TypeArg tye -> dispGen req tye

instance (Disp sv, Disp (af sv)) => Disp (AssLiteralF af sv) where
  dispGen _ = \case
    ALitInt n -> pretty n
    ALitFloat r -> pretty r
    ALitBool True -> "true"
    ALitBool False -> "false"
    ALitUnit -> "()"
    ALitString t -> dispStringLiteral t
    ALitList es -> dispListLiteral es
    ALitVec v -> dispVectorLiteral (Vector.toList v)
    ALitMat m -> dispMatrixLiteral (Matrix.toRows m)

instance (Disp sv) => Disp (Ass0ExprF sv) where
  dispGen req = \case
    A0Literal lit -> disp lit
    A0Var y -> disp y
    A0BuiltInName builtInName -> disp builtInName
    A0Lam Nothing (y, a0tye1) a0e2 -> dispNonrecLam req Nothing y a0tye1 a0e2
    A0Lam (Just (f, a0tyeRec)) (y, a0tye1) a0e2 -> dispRecLam req f a0tyeRec Nothing y a0tye1 a0e2
    A0App a0e1 a0e2 -> dispApp req a0e1 Nothing a0e2
    A0LetIn (y, a0tye1) a0e1 a0e2 -> dispLetInWithAnnot req y a0tye1 a0e1 a0e2
    A0LetTupleIn xL xR a0e1 a0e2 -> dispLetTupleIn req xL xR a0e1 a0e2
    A0Sequential a0e1 a0e2 -> dispSequential req a0e1 a0e2
    A0Tuple a0e1 a0e2 -> dispTuple a0e1 a0e2
    A0Bracket a1e1 -> dispBracket a1e1
    A0IfThenElse a0e0 a0e1 a0e2 -> dispIfThenElse req a0e0 a0e1 a0e2
    A0TyEqAssert _loc ty1eq ->
      let (a1tye1, a1tye2) = decomposeType1Equation ty1eq
       in group (assertionStyle ("{" <> dispBracket a1tye1 <+> "=>" <+> dispBracket a1tye2 <> "}"))
    A0RefinementAssert _loc a0ePred a0eTarget ->
      deepenParenWhen (req <= Atomic) $
        "ASSERT" <+> disp a0ePred <+> "FOR" <+> disp a0eTarget
    A0AppType a0e1 sa0tye2 ->
      dispAppType req a0e1 sa0tye2

instance (Disp sv) => Disp (Ass1ExprF sv) where
  dispGen req = \case
    A1Literal lit -> disp lit
    A1Var x -> disp x
    A1BuiltInName a1builtInName -> disp a1builtInName
    A1Lam Nothing (x, a1tye1) a1e2 -> dispNonrecLam req Nothing x a1tye1 a1e2
    A1Lam (Just (f, a1tyeRec)) (x, a1tye1) a1e2 -> dispRecLam req f a1tyeRec Nothing x a1tye1 a1e2
    A1App a1e1 a1e2 -> dispApp req a1e1 Nothing a1e2
    A1LetTupleIn xL xR a1e1 a1e2 -> dispLetTupleIn req xL xR a1e1 a1e2
    A1Sequential a1e1 a1e2 -> dispSequential req a1e1 a1e2
    A1Tuple a1e1 a1e2 -> dispTuple a1e1 a1e2
    A1IfThenElse a1e0 a1e1 a1e2 -> dispIfThenElse req a1e0 a1e1 a1e2
    A1Escape a0e1 -> dispEscape a0e1
    A1AppType a1e1 a1tye2 -> dispAppType req a1e1 a1tye2

instance Disp AssPrimBaseType where
  dispGen _req = \case
    ATyPrimInt -> "Int"
    ATyPrimFloat -> "Float"
    ATyPrimBool -> "Bool"
    ATyPrimUnit -> "Unit"
    ATyPrimString -> "String"
    ATyPrimDevice -> "Device"
    ATyPrimActivation -> "Activation"
    ATyPrimVarStore -> "VarStore"
    ATyPrimOptimizer -> "Optimizer"

instance Disp Ass0PrimType where
  dispGen req = \case
    A0TyPrimBase tyPrimBase -> disp tyPrimBase
    A0TyTensor [n] -> dispNameWithArgs req "Vec" disp [n]
    A0TyTensor [m, n] -> dispNameWithArgs req "Mat" disp [m, n]
    A0TyTensor ns -> dispNameWithArgs req "Tensor" dispListLiteral [ns]
    A0TyDataset datasetParam -> dispNameWithArgs req "Dataset" (dispDatasetParam disp dispListLiteral) [datasetParam]

instance (Disp sv) => Disp (Ass0TypeExprF sv) where
  dispGen req = \case
    A0TyPrim a0tyPrim Nothing -> disp a0tyPrim
    A0TyPrim a0tyPrim (Just a0ePred) -> dispInternalRefinementType req a0tyPrim a0ePred
    A0TyVar atyvar -> dispTypeVar atyvar
    A0TyList a0tye Nothing -> dispListType req a0tye
    A0TyList a0tye (Just a0ePred) -> dispInternalRefinementListType req a0tye a0ePred
    A0TyProduct a0tye1 a0tye2 -> dispProductType req a0tye1 a0tye2
    A0TyArrow (xOpt, a0tye1) a0tye2 -> dispArrowType req xOpt a0tye1 a0tye2
    A0TyCode a1tye1 -> dispBracket a1tye1
    A0TyOptArrow (x, a0tye1) a0tye2 -> dispOptArrowType req x a0tye1 a0tye2
    A0TyImplicitForAll atyvar a0tye -> dispForAllType req atyvar a0tye

instance (Disp sv) => Disp (StrictAss0TypeExprF sv) where
  dispGen req = \case
    SA0TyPrim a0tyPrim Nothing -> disp a0tyPrim
    SA0TyPrim a0tyPrim (Just a0ePred) -> dispInternalRefinementType req a0tyPrim a0ePred
    SA0TyVar atyvar -> dispTypeVar atyvar
    SA0TyList sa0tye Nothing -> dispListType req sa0tye
    SA0TyList sa0tye (Just a0ePred) -> dispInternalRefinementListType req sa0tye a0ePred
    SA0TyProduct sa0tye1 sa0tye2 -> dispProductType req sa0tye1 sa0tye2
    SA0TyArrow (xOpt, sa0tye1) sa0tye2 -> dispArrowType req xOpt sa0tye1 sa0tye2
    SA0TyCode a1tye1 -> dispBracket a1tye1
    SA0TyExplicitForAll atyvar sa0tye -> dispForAllType req atyvar sa0tye

instance (Disp sv) => Disp (Ass1PrimTypeF sv) where
  dispGen req = \case
    A1TyPrimBase tyPrimBase ->
      disp tyPrimBase
    A1TyTensor a0eList ->
      case a0eList of
        A0Literal (ALitList [a0e]) -> dispNameWithArgs req "Vec" dispPersistent [a0e]
        A0Literal (ALitList [a0e1, a0e2]) -> dispNameWithArgs req "Mat" dispPersistent [a0e1, a0e2]
        _ -> dispNameWithArgs req "Tensor" dispPersistent [a0eList]
    A1TyDataset datasetParam ->
      dispNameWithArgs req "Dataset" (dispDatasetParam disp (disp . runIdentity)) [datasetParam]

instance (Disp sv) => Disp (Ass1TypeExprF sv) where
  dispGen req = \case
    A1TyPrim a1tyPrim -> dispGen req a1tyPrim
    A1TyList a1tye -> dispListType req a1tye
    A1TyVar atyvar -> dispTypeVar atyvar
    A1TyProduct a1tye1 a1tye2 -> dispProductType req a1tye1 a1tye2
    A1TyArrow a1tye1 a1tye2 -> dispNondepArrowType req a1tye1 a1tye2
    A1TyImplicitForAll atyvar a1tye2 -> dispForAllType req atyvar a1tye2

instance Disp FrontError where
  dispGen _ = \case
    FrontLexingError s ->
      disp (Text.pack s)
    FrontParseError parseErrors ->
      List.foldl' (\doc parseError -> doc <> hardline <> disp parseError) mempty parseErrors

instance Disp ParseError where
  dispGen _ ParseError {spanInFile, message} =
    disp spanInFile
      <> hardline
      <> disp message

instance Disp Matrix.ConstructionError where
  dispGen _ = \case
    Matrix.EmptyRow -> "contains an empty row"
    Matrix.InconsistencyOfRowLength row1 row2 ->
      "two rows have different lengths. one:"
        <> hardline
        <> dispRowContents row1
        <> hardline
        <> "another:"
        <> hardline
        <> dispRowContents row2

instance (Disp sv) => Disp (TypeErrorF sv) where
  dispGen _ = \case
    UnboundVar spanInFile ms x ->
      "Unbound variable" <+> dispLongName ms x <+> disp spanInFile
    UnboundTypeVar spanInFile (TypeVar a) ->
      "Unbound type variable" <+> disp a <+> disp spanInFile
    UnboundModule spanInFile m ->
      "Unbound module" <+> disp m <+> disp spanInFile
    NotAStage0Var spanInFile x ->
      "Not a stage-0 variable:" <+> disp x <+> disp spanInFile
    NotAStage1Var spanInFile x ->
      "Not a stage-1 variable:" <+> disp x <+> disp spanInFile
    UnknownTypeOrInvalidArityAtStage0 spanInFile tyName n ->
      "Unknown type or invalid arity (at stage 0):" <+> disp tyName <> "," <+> disp n <+> disp spanInFile
    UnknownTypeOrInvalidArityAtStage1 spanInFile tyName n ->
      "Unknown type or invalid arity (at stage 1):" <+> disp tyName <> "," <+> disp n <+> disp spanInFile
    NotAnIntLitArgAtStage0 spanInFile a0e ->
      "An argument expression at stage 0 is not an integer literal:" <+> stage0Style (disp a0e) <+> disp spanInFile
    NotAnIntListLitArgAtStage0 spanInFile a0e ->
      "An argument expression at stage 0 is not an integer list literal:" <+> stage0Style (disp a0e) <+> disp spanInFile
    NotAValueArg spanInFile ->
      "Expected a value argument here, but is not" <+> disp spanInFile
    NotATypeArg spanInFile ->
      "Expected a type argument here, but is not" <+> disp spanInFile
    TypeContradictionAtStage0 spanInFile a0tye1 a0tye2 ->
      "Type contradiction at stage 0"
        <+> disp spanInFile
        <> hardline
        <> "left:"
        <> nest 2 (hardline <> stage0Style (disp a0tye1))
        <> hardline
        <> "right:"
        <> nest 2 (hardline <> stage0Style (disp a0tye2))
    TypeContradictionAtStage1 spanInFile a1tye1 a1tye2 ->
      "Type contradiction at stage 1"
        <+> disp spanInFile
        <> hardline
        <> "left:"
        <> nest 2 (hardline <> stage1Style (disp a1tye1))
        <> hardline
        <> "right:"
        <> nest 2 (hardline <> stage1Style (disp a1tye2))
    NotABoolTypeForStage0 spanInFile a0tye ->
      "Not bool (at stage 0):" <+> stage1Style (disp a0tye) <+> disp spanInFile
    NotABoolTypeForStage1 spanInFile a1tye ->
      "Not bool (at stage 1):" <+> stage1Style (disp a1tye) <+> disp spanInFile
    NotAUnitTypeForStage0 spanInFile a0tye ->
      "Not unit (at stage 0):" <+> stage1Style (disp a0tye) <+> disp spanInFile
    NotAUnitTypeForStage1 spanInFile a1tye ->
      "Not unit (at stage 1):" <+> stage1Style (disp a1tye) <+> disp spanInFile
    NotACodeType spanInFile a0tye ->
      "Not a code type:" <+> stage0Style (disp a0tye) <+> disp spanInFile
    CannotUseEscapeAtStage0 spanInFile ->
      "Cannot use Escape (~) at stage 0" <+> disp spanInFile
    CannotUseBracketAtStage1 spanInFile ->
      "Cannot use Bracket (&) at stage 1" <+> disp spanInFile
    CannotUseLamOptAtStage1 spanInFile ->
      "Cannot use function with implicit parameters (fun{...} ->) at stage 1" <+> disp spanInFile
    CannotUseAppOptGivenAtStage1 spanInFile ->
      "Cannot use application for implicit parameters (... {...}) at stage 1" <+> disp spanInFile
    CannotUseAppOptOmittedAtStage1 spanInFile ->
      "Cannot use application for implicit parameters (... _) at stage 1" <+> disp spanInFile
    FunctionTypeCannotBeDependentAtStage1 spanInFile x ->
      "Function types cannot be dependent at stage 1:" <+> disp x <+> disp spanInFile
    CannotUseCodeTypeAtStage1 spanInFile ->
      "Cannot use code types at stage 1" <+> disp spanInFile
    CannotUseOptArrowTypeAtStage1 spanInFile ->
      "Cannot use implicit function types at stage 1" <+> disp spanInFile
    CannotUseRefinementTypeAtStage1 spanInFile ->
      "Cannot use refinement types at stage 1" <+> disp spanInFile
    CannotUsePersistentArgAtStage0 spanInFile ->
      "Cannot use persistent arguments at stage 0" <+> disp spanInFile
    CannotUseNormalArgAtStage1 spanInFile ->
      "Cannot use normal arguments at stage 1" <+> disp spanInFile
    CannotUseTypeVarAtStage1 spanInFile ->
      "Cannot use type variables at stage 1" <+> disp spanInFile
    VarOccursFreelyInAss0Type spanInFile x a0result ->
      "Variable" <+> disp x <+> "occurs in stage-0 type" <+> stage0Style (disp a0result) <+> disp spanInFile
    VarOccursFreelyInAss1Type spanInFile x a1result ->
      "Variable" <+> disp x <+> "occurs in stage-1 type" <+> stage1Style (disp a1result) <+> disp spanInFile
    InvalidMatrixLiteral spanInFile e ->
      "Invalid matrix literal;" <+> disp e <+> disp spanInFile
    CannotMergeTypesByConditional0 spanInFile a0tye1 a0tye2 condErr ->
      "Cannot merge stage-0 types by conditionals"
        <+> disp spanInFile
        <> hardline
        <+> "left:"
        <> nest 2 (hardline <> stage0Style (disp a0tye1))
        <> hardline
        <+> "right:"
        <> nest 2 (hardline <> stage0Style (disp a0tye2))
        <> hardline
        <> disp condErr
    CannotMergeTypesByConditional1 spanInFile a1tye1 a1tye2 condErr ->
      "Cannot merge stage-1 types by conditionals"
        <+> disp spanInFile
        <> hardline
        <+> "left:"
        <> nest 2 (hardline <> stage1Style (disp a1tye1))
        <> hardline
        <+> "right:"
        <> nest 2 (hardline <> stage1Style (disp a1tye2))
        <> hardline
        <> disp condErr
    CannotMergeResultsByConditionals spanInFile result1 result2 ->
      "Cannot merge results by conditionals"
        <+> disp spanInFile
        <> hardline
        <+> "left:"
        <> nest 2 (hardline <> disp result1)
        <> hardline
        <+> "right:"
        <> nest 2 (hardline <> disp result2)
    CannotApplyLiteral spanInFile ->
      "Cannot apply a literal" <> disp spanInFile
    CannotInstantiateGuidedByAppContext0 spanInFile appCtx a0tye ->
      "Cannot instantiate a stage-0 type guided by the application context"
        <+> disp spanInFile
        <> hardline
        <+> "application context:"
        <> nest 2 (hardline <> disps appCtx)
        <> hardline
        <+> "type:"
        <> nest 2 (hardline <> stage0Style (disp a0tye))
    CannotInstantiateGuidedByAppContext1 spanInFile appCtx a1tye ->
      "Cannot instantiate a stage-1 type guided by the application context"
        <+> disp spanInFile
        <> hardline
        <+> "application context:"
        <> nest 2 (hardline <> disps appCtx)
        <> hardline
        <+> "type:"
        <> nest 2 (hardline <> stage1Style (disp a1tye))
    CannotInferOptional spanInFile x a0tye appCtx ->
      "Cannot infer an implicit argument for"
        <+> disp x
        <+> disp spanInFile
        <> hardline
        <+> "application context:"
        <> nest 2 (hardline <> disps appCtx)
        <> hardline
        <+> "type:"
        <> nest 2 (hardline <> stage0Style (disp a0tye))
    Stage1IfThenElseRestrictedToEmptyContext spanInFile appCtx ->
      "Stage-1 if-expressions are restricted to be used at empty application contexts"
        <+> disp spanInFile
        <> hardline
        <+> "application context:"
        <> nest 2 (hardline <> disps appCtx)
    BindingOverwritten spanInFile x ->
      "value " <+> disp x <+> "is overwritten by another binding" <+> disp spanInFile
    UnknownExternalName spanInFile extName ->
      "Unknown external name" <+> disp extName <+> disp spanInFile
    InvalidPersistentType spanInFile a0tye ->
      "Invalid persistent type:" <+> stage0Style (disp a0tye) <+> disp spanInFile
    InvalidTypeForRefinement spanInFile a0tye ->
      "Invalid type for refinement:" <+> stage0Style (disp a0tye) <+> disp spanInFile
    NoBuiltInNameInExternal spanInFile ->
      "No built-in name specified for an external value" <+> disp spanInFile
    CannotApplyTuple spanInFile ->
      "Cannot apply a tuple" <> disp spanInFile
    NotATupleAtStage0 spanInFile a0tye ->
      "Not a tuple at stage 0"
        <+> disp spanInFile
        <> hardline
        <+> stage0Style (disp a0tye)
    NotATupleAtStage1 spanInFile a1tye ->
      "Not a tuple at stage 1"
        <+> disp spanInFile
        <> hardline
        <+> stage1Style (disp a1tye)
    LetRecParamsCannotStartWithOptional spanInFile ->
      "Recursive function definitions cannot have an implicit parameter as the first one" <+> disp spanInFile
    LetRecRequiresNonEmptyParams spanInFile ->
      "Recursive function definitions require at least one parameter" <+> disp spanInFile
    CannotSynthesizeTypeFromExpr spanInFile ->
      "Cannot synthesize the type of the expression; consider using `as`" <+> disp spanInFile
    CannotForceType spanInFile a0tye ->
      "Cannot force type" <+> disp a0tye <+> "on the expression" <+> disp spanInFile

instance (Disp sv) => Disp (ConditionalMergeErrorF sv) where
  dispGen _ = \case
    CannotMerge0 a0tye1 a0tye2 ->
      "types" <+> stage0Style (disp a0tye1) <+> "and" <+> stage0Style (disp a0tye2) <+> "are incompatible"
    CannotMerge1 a1tye1 a1tye2 ->
      "types" <+> stage1Style (disp a1tye1) <+> "and" <+> stage1Style (disp a1tye2) <+> "are incompatible"

instance (Disp sv) => Disp (AppContextEntryF sv) where
  dispGen _ = \case
    AppArg0 Nothing a0e a0tye -> stage0Style (disp a0e) <+> ":" <+> stage0Style (disp a0tye)
    AppArg0 (Just label) a0e a0tye -> "#" <> disp label <+> stage0Style (disp a0e) <+> ":" <+> stage0Style (disp a0tye)
    AppArg1 Nothing a1tye -> stage1Style (disp a1tye)
    AppArg1 (Just label) a1tye -> "#" <> disp label <+> stage1Style (disp a1tye)
    AppArgOptGiven0 a0e a0tye -> "{" <> stage0Style (disp a0e) <+> ":" <+> stage0Style (disp a0tye) <> "}"
    AppArgOptOmitted0 -> "_"

instance (Disp sv, Disp (af sv)) => Disp (ResultF af sv) where
  dispGen _ = \case
    Pure v -> disp v -- TODO (enhance): add `stage0Style` etc.
    Cast0 _ a0tye r -> "cast0 :" <+> stage0Style (disp a0tye) <> ";" <+> disp r
    Cast1 _ a1tye r -> "cast1 :" <+> stage1Style (disp a1tye) <> ";" <+> disp r
    CastGiven0 _ a0tye r -> "cast-given0 :" <+> stage0Style (disp a0tye) <> ";" <+> disp r
    FillInferred0 a0e r -> "fill0" <+> disp a0e <> ";" <+> disp r
    InsertInferred0 a0e r -> "insert0" <+> disp a0e <> ";" <+> disp r
    InsertInferredType0 sa0tye r -> "insert-type0" <+> disp sa0tye <> ";" <+> disp r
    InsertType1 a1tye r -> "insert-type1" <+> disp a1tye <> ";" <+> disp r

instance (Disp sv) => Disp (Ass0ValF sv) where
  dispGen req = \case
    A0ValLiteral lit -> disp lit
    A0ValTuple a1v1 a1v2 -> dispTuple a1v1 a1v2
    A0ValLam Nothing (x, a0tyv1) a0v2 _env -> dispNonrecLam req Nothing x a0tyv1 a0v2
    A0ValLam (Just (f, a0tyvRec)) (x, a0tyv1) a0v2 _env -> dispRecLam req f a0tyvRec Nothing x a0tyv1 a0v2
    A0ValBracket a1v1 -> dispBracket a1v1
    A0ValPartialBuiltInApp pba -> dispGen req pba

instance (Disp v) => Disp (Ass0PartialBuiltInApp v) where
  dispGen req = \case
    A0PartialBuiltInAppArity1 pba1 -> dispGen req pba1
    A0PartialBuiltInAppArity2 pba2 -> dispGen req pba2
    A0PartialBuiltInAppArity3 pba3 -> dispGen req pba3
    A0PartialBuiltInAppArity4 pba4 -> dispGen req pba4
    A0PartialBuiltInAppArity6 pba6 -> dispGen req pba6
    _ -> "TODO: Disp (Ass0PartialBuiltInApp v)"

instance (Disp v) => Disp (Ass0PartialBuiltInAppArity1 v) where
  dispGen req = \case
    PartialBuiltInAppArity1Nil bi1 -> disp bi1
    PartialBuiltInAppArity1Cons pba2 v -> f (disp pba2 <+> dispGen Atomic v)
    where
      f = deepenParenWhen (req <= Atomic)

instance (Disp v) => Disp (Ass0PartialBuiltInAppArity2 v) where
  dispGen req = \case
    PartialBuiltInAppArity2Nil bi2 -> disp bi2
    PartialBuiltInAppArity2Cons pba3 v -> f (disp pba3 <+> dispGen Atomic v)
    where
      f = deepenParenWhen (req <= Atomic)

instance (Disp v) => Disp (Ass0PartialBuiltInAppArity3 v) where
  dispGen req = \case
    PartialBuiltInAppArity3Nil bi3 -> disp bi3
    PartialBuiltInAppArity3Cons pba4 v -> f (disp pba4 <+> dispGen Atomic v)
    where
      f = deepenParenWhen (req <= Atomic)

instance (Disp v) => Disp (Ass0PartialBuiltInAppArity4 v) where
  dispGen req = \case
    PartialBuiltInAppArity4Cons pba5 v -> f (disp pba5 <+> dispGen Atomic v)
    where
      f = deepenParenWhen (req <= Atomic)

instance (Disp v) => Disp (Ass0PartialBuiltInAppArity5 v) where
  dispGen req = \case
    PartialBuiltInAppArity5Nil bi5 -> disp bi5
    PartialBuiltInAppArity5Cons pba6 v -> f (disp pba6 <+> dispGen Atomic v)
    where
      f = deepenParenWhen (req <= Atomic)

instance (Disp v) => Disp (Ass0PartialBuiltInAppArity6 v) where
  dispGen _req = \case
    PartialBuiltInAppArity6Cons _pba7 _v -> "TODO: Disp (Ass0PartialBuiltInAppArity6 v)"

instance Disp Ass1BuiltIn where
  dispGen _ = \case
    A1BIVadd n -> "vadd" <> param (disp n)
    A1BIVconcat m n -> "vconcat" <> param (disps [m, n])
    A1BIMtranspose m n -> "mtranspose" <> param (disps [m, n])
    A1BIMconcatVert m1 m2 n -> "mconcat_vert" <> param (disps [m1, m2, n])
    A1BITensorZeros ns1 -> "Tensor.zeros" <> param (dispListLiteral ns1)
    A1BITensorMult ns1 ns2 -> "Tensor.mult" <> param (dispListLiteral ns1 <> "," <+> dispListLiteral ns2)
    A1BITensorGrad ns1 -> "Tensor.grad" <> param (dispListLiteral ns1)
    A1BITensorZeroGrad ns1 -> "Tensor.zero_grad" <> param (dispListLiteral ns1)
    A1BITensorSubUpdate ns1 -> "Tensor.sub_update" <> param (dispListLiteral ns1)
    A1BITensorArgmax ns1 n2 -> "Tensor.argmax" <> param (dispListLiteral ns1 <> "," <+> disp n2)
    A1BITensorCrossEntropyForLogits n1 n2 -> "Tensor.cross_entropy_for_logits" <> param (disps [n1, n2])
    A1BITensorCountEqual ns -> "Tensor.count_equal" <> param (dispListLiteral ns)
    A1BITensorDropout shape -> "Tensor.dropout" <> param (dispListLiteral shape)
    A1BITensorReshape shape1 shape2 -> "Tensor.reshape" <> param (dispListLiteral shape1 <> "," <+> dispListLiteral shape2)
    A1BITensorAdd ns1 ns2 -> "Tensor.add" <> param (dispListLiteral ns1 <> "," <+> dispListLiteral ns2)
    A1BITensorMm k m n -> "Tensor.mm" <> param (disps [k, m, n])
    A1BIAdd -> "+"
    A1BISub -> "-"
    A1BIMult -> "*"
    A1BIDiv -> "//"
    A1BIFloatDiv -> "/"
    A1BIMod -> "mod"
    A1BILeq -> "<="
    A1BIEqual -> "=="
    A1BIFloat -> "float"
    A1BIPrintFloat -> "print_float"
    A1BIPrintString -> "print_string"
    A1BIListAppend -> "List.append"
    A1BIListIter -> "List.iter"
    A1BIRange -> "range"
    A1BITensorF -> "Tensor.f"
    A1BITensorBackward -> "Tensor.backward"
    A1BITensorNoGrad -> "Tensor.no_grad"
    A1BITensorFloatValue -> "Tensor.float_value"
    A1BITensorMaxPool2d k l m n padding1 padding2 ksize1 ksize2 stride1 stride2 -> "Tensor.max_pool2d" <> param (disps [k, l, m, n, padding1, padding2, ksize1, ksize2, stride1, stride2])
    A1BILayerActivationRelu -> "Layer.Activation.relu"
    A1BILayerActivationNone -> "Layer.Activation.none"
    A1BILayerLinear ns input_dim output_dim -> "Layer.linear" <> param (dispListLiteral ns <> "," <+> disps [input_dim, output_dim])
    A1BILayerForward shape1 shape2 -> "Layber.forward" <> param (dispListLiteral shape1 <> "," <+> dispListLiteral shape2)
    A1BILayerConv2d l m n ksize stride padding input_dim output_dim -> "Layer.conv2d" <> param (disps [l, m, n, ksize, stride, padding, input_dim, output_dim])
    A1BIVarStoreCreate -> "Var_store.create"
    A1BIOptimizerAdam -> "Optimizer.adam"
    A1BIOptimizerBackwardStep -> "Optimizer.backward_step"
    A1BIDatasetHelperTrainBatch ntrain ntest imgdim labeldim batchSize -> "Dataset_helper.train_batch" <> param (disp ntrain <> "," <+> disp ntest <> "," <+> dispListLiteral imgdim <> "," <+> dispListLiteral labeldim <> "," <+> disp batchSize)
    A1BIDatasetHelperBatchAccuracy ntrain ntest imgdim labeldim n batchSize -> "Dataset_helper.batch_accuracy" <> param (disp ntrain <> "," <+> disp ntest <> "," <+> dispListLiteral imgdim <> "," <+> dispListLiteral labeldim <> "," <+> disp n <> "," <+> disp batchSize)
    A1BIMnistHelperTrainImages -> "Mnist_helper.train_images"
    A1BIMnistHelperTrainLabels -> "Mnist_helper.train_labels"
    A1BIMnistHelperTestImages -> "Mnist_helper.test_images"
    A1BIMnistHelperTestLabels -> "Mnist_helper.test_labels"
    A1BuiltInOther s -> "OTHER '" <> disp s <> "'"
    where
      param doc = stagingOperatorStyle ("@{" <> doc <> "}")

instance (Disp sv) => Disp (Ass1ValF sv) where
  dispGen req = \case
    A1ValLiteral lit -> disp lit
    A1ValConst c -> disp c
    A1ValVar symb -> disp symb
    A1ValLam Nothing (symbX, a1tyv1) a1v2 ->
      dispNonrecLam req Nothing symbX a1tyv1 a1v2
    A1ValLam (Just (symbF, a1tyvRec)) (symbX, a1tyv1) a1v2 ->
      dispRecLam req symbF a1tyvRec Nothing symbX a1tyv1 a1v2
    A1ValApp a1v1 a1v2 ->
      dispApp req a1v1 Nothing a1v2
    A1ValLetTupleIn xL xR a1v1 a1v2 ->
      dispLetTupleIn req xL xR a1v1 a1v2
    A1ValSequential a1v1 a1v2 ->
      dispSequential req a1v1 a1v2
    A1ValTuple a1v1 a1v2 ->
      dispTuple a1v1 a1v2
    A1ValIfThenElse a1v0 a1v1 a1v2 ->
      dispIfThenElse req a1v0 a1v1 a1v2

instance (Disp sv) => Disp (Ass0TypeValF sv) where
  dispGen req = \case
    A0TyValPrim a0tyvPrim Nothing -> dispGen req a0tyvPrim
    A0TyValPrim a0tyvPrim (Just a0vPred) -> dispInternalRefinementType req a0tyvPrim a0vPred
    A0TyValVar atyvar -> dispTypeVar atyvar
    A0TyValList a0tyv1 Nothing -> dispListType req a0tyv1
    A0TyValList a0tyv1 (Just a0vPred) -> dispInternalRefinementListType req a0tyv1 a0vPred
    A0TyValProduct a0tyv1 a0tyv2 -> dispProductType req a0tyv1 a0tyv2
    A0TyValArrow (xOpt, a0tyv1) a0tye2 -> dispArrowType req xOpt a0tyv1 a0tye2
    A0TyValCode a1tyv1 -> dispBracket a1tyv1
    A0TyValExplicitForAll atyvar sa0tye1 -> dispForAllType req atyvar sa0tye1

instance Disp Ass0PrimTypeVal where
  dispGen req = \case
    A0TyValPrimBase tyPrimBase -> disp tyPrimBase
    A0TyValTensor [n] -> dispNameWithArgs req "Vec" disp [n]
    A0TyValTensor [m, n] -> dispNameWithArgs req "Mat" disp [m, n]
    A0TyValTensor ns -> dispNameWithArgs req "Tensor" dispListLiteral [ns]
    A0TyValDataset datasetParam -> dispNameWithArgs req "Dataset" (dispDatasetParam disp dispListLiteral) [datasetParam]

instance (Disp sv) => Disp (Ass1TypeValF sv) where
  dispGen req = \case
    A1TyValPrim a1tyvPrim -> dispGen req a1tyvPrim
    A1TyValList a1tyv -> dispListType req a1tyv
    A1TyValVar atyvar -> dispTypeVar atyvar
    A1TyValProduct a1tyv1 a1tyv2 -> dispProductType req a1tyv1 a1tyv2
    A1TyValArrow a1tyv1 a1tyv2 -> dispNondepArrowType req a1tyv1 a1tyv2
    A1TyValImplicitForAll atyvar a1tye2 -> dispForAllType req atyvar a1tye2

instance Disp Ass1PrimTypeVal where
  dispGen req = \case
    A1TyValPrimBase tyPrimBase -> disp tyPrimBase
    A1TyValTensor [n] -> dispNameWithArgs req "Vec" dispPersistent [n]
    A1TyValTensor [m, n] -> dispNameWithArgs req "Mat" dispPersistent [m, n]
    A1TyValTensor ns -> dispNameWithArgs req "Tensor" dispPersistentListLiteral [ns]
    A1TyValDataset datasetParam -> dispNameWithArgs req "Dataset" (dispDatasetParam disp dispListLiteral) [datasetParam]

instance Disp LocationInFile where
  dispGen _ (LocationInFile l c) =
    "line" <+> disp l <> ", column" <+> disp (c - 1)

instance Disp SpanInFile where
  dispGen _ (SpanInFile {startLocation, endLocation, contents}) =
    regionText <> maybe mempty makeLineText contents
    where
      regionText =
        if startLine == endLine
          then
            "(from line" <+> disp startLine <> ", columns" <+> disp startColumn <> "-" <> disp endColumn <> ")"
          else
            "(from" <+> disp startLocation <+> "to" <+> disp endLocation <> ")"

      makeLineText s =
        if startLine == endLine
          then hardline <> disp s <> hardline <> indentation <> hats
          else mempty

      LocationInFile startLine startColumn = startLocation
      LocationInFile endLine endColumn = endLocation
      indentation = disp (replicate (startColumn - 1) ' ')
      hats = disp (replicate (endColumn - startColumn) '^')

instance (Disp sv) => Disp (BugF sv) where
  dispGen _ = \case
    UnboundVarFound x ->
      "Unbound variable" <+> disp x
    NotAClosure a0v ->
      "Not a closure:" <+> disp a0v
    NotACodeValue a0v ->
      "Not a code value:" <+> disp a0v
    NotAnInteger a0v ->
      "Not an integer:" <+> disp a0v
    NotAList a0v ->
      "Not a list:" <+> disp a0v
    NotAVector a0v ->
      "Not a vector:" <+> disp a0v
    NotAMatrix a0v ->
      "Not a matrix:" <+> disp a0v
    NotABoolean a0v ->
      "Not a Boolean:" <+> disp a0v
    NotAUnit a0v ->
      "Not a unit:" <+> disp a0v
    NotAString a0v ->
      "Not a string:" <+> disp a0v
    NotATuple a0v ->
      "Not a tuple:" <+> disp a0v
    FoundSymbol x symb ->
      "Expected a stage-0 value, but found a symbol:" <+> disp symb <+> "(bound to:" <+> disp x <> ")"
    FoundAss0Val x a0v ->
      "Expected a symbol, but found a stage-0 value:" <+> disp a0v <+> "(bound to:" <+> disp x <> ")"
    InconsistentAppBuiltInArity1 bi1 a0v1 ->
      "Inconsistent application of a built-in function:"
        <+> disp bi1
        <+> disp a0v1
    InconsistentAppBuiltInArity2 bi2 a0v1 a0v2 ->
      "Inconsistent application of a built-in function:"
        <+> disp bi2
        <+> disp a0v1
        <+> disp a0v2
    BroadcastFailed ns1 ns2 ->
      "Broadcast failed:" <+> dispListLiteral ns1 <> "," <+> dispListLiteral ns2

instance (Disp sv) => Disp (EvalErrorF sv) where
  dispGen _ = \case
    Bug bug ->
      "Bug:" <+> disp bug
    AssertionFailure spanInFile a1tyv1 a1tyv2 ->
      "Assertion failure"
        <+> disp spanInFile
        <> hardline
        <> "got:"
        <> nest 2 (hardline <> disp a1tyv1)
        <> hardline
        <> "expected:"
        <> nest 2 (hardline <> disp a1tyv2)
    RefinementAssertionFailure spanInFile a0vPred a0vTarget ->
      "Assertion failure of downcast"
        <+> disp spanInFile
        <> hardline
        <> "predicate:"
        <+> stage0Style (disp a0vPred)
        <> hardline
        <> "got:"
        <+> stage0Style (disp a0vTarget)

instance Disp Bta.AnalysisError where
  dispGen _ = \case
    Bta.UnboundVar spanInFile ms x ->
      "Unbound variable" <+> disp (Text.intercalate "." (ms ++ [x])) <+> disp spanInFile
    Bta.NotAVal spanInFile ms x ->
      "Not a value:" <+> disp (Text.intercalate "." (ms ++ [x])) <+> disp spanInFile
    Bta.NotAModule spanInFile m ->
      "Not a module:" <+> disp m <+> disp spanInFile
    Bta.NotAFunction spanInFile bity ->
      "Not a function;" <+> disp bity <+> disp spanInFile
    Bta.NotAnOptFunction spanInFile bity ->
      "Not a function with implicit parameter;" <+> disp bity <+> disp spanInFile
    Bta.NotABase spanInFile bity ->
      "Not of base type;" <+> disp bity <+> disp spanInFile
    Bta.NotATuple spanInFile bity ->
      "Not a tuple;" <+> disp bity <+> disp spanInFile
    Bta.BindingTimeContradiction spanInFile ->
      "Binding-time contradiction" <+> disp spanInFile
    Bta.BITypeContradiction spanInFile bity1 bity2 bity1Local bity2Local ->
      "Basic type contradiction;"
        <+> disp bity1
        <+> "!="
        <+> disp bity2
        <+> disp spanInFile
        <> ";"
        <+> disp bity1Local
        <+> "!="
        <+> disp bity2Local
    Bta.UnknownTypeOrInvalidArgs spanInFile _tyName _args ->
      -- TODO (enhance): detailed report
      "Unknown type or invalid arguments" <+> disp spanInFile

instance Disp Bta.BindingTime where
  dispGen _req = \case
    Bta.BTConst Bta.BT0 -> "0"
    Bta.BTConst Bta.BT1 -> "1"
    Bta.BTVar (Bta.BindingTimeVar n) -> "β" <> disp n

instance (Disp bt) => Disp (Bta.BITypeF bt) where
  dispGen _req (Bta.BIType bt btMain) =
    dispGen Atomic btMain <> "^" <> dispGen Atomic bt

instance (Disp bt) => Disp (Bta.BITypeMainF bt) where
  dispGen req = \case
    Bta.BITyBase [] ->
      "●"
    Bta.BITyBase (bt0 : bts) ->
      deepenParenWhen (req <= Atomic) ("●" <+> List.foldl' (\doc bt -> doc <+> disp bt) (disp bt0) bts)
    Bta.BITyProduct bt1 bt2 ->
      deepenParenWhen (req <= Atomic) (dispGen Atomic bt1 <+> "*" <+> dispGen Atomic bt2)
    Bta.BITyArrow bt1 bt2 ->
      deepenParenWhen (req <= Atomic) (dispGen Atomic bt1 <+> "->" <+> dispGen Atomic bt2)
    Bta.BITyOptArrow bt1 bt2 ->
      deepenParenWhen (req <= Atomic) ("{" <> dispGen Atomic bt1 <> "} ->" <+> dispGen Atomic bt2)

dispWithBindingTime :: (Disp exprMain) => Bta.BindingTimeConst -> exprMain -> Doc Ann
dispWithBindingTime btc eMain =
  group (f (prefix <> "(") <> disp eMain <> f ")")
  where
    (f, prefix) =
      case btc of
        Bta.BT0 -> (bindingTime0Style, "$0")
        Bta.BT1 -> (bindingTime1Style, "$1")

instance Disp (Bta.BCExprF ann) where
  dispGen _ (Surface.Expr (btc, _ann) exprMain) =
    dispWithBindingTime btc exprMain

instance Disp (Bta.BCExprMainF ann) where
  dispGen req = \case
    Surface.Literal lit -> disp lit
    Surface.Var (ms, x) -> dispLongName ms x
    Surface.Lam Nothing (x, tye1) e2 -> dispNonrecLam req Nothing x tye1 e2
    Surface.Lam (Just (f, tyeRec)) (x, tye1) e2 -> dispRecLam req f tyeRec Nothing x tye1 e2
    Surface.App e1 e2 -> dispApp req e1 Nothing e2
    Surface.LetIn x params eBody e2 -> dispLetIn req x params eBody e2
    Surface.LetRecIn f params tyeBody eBody e2 -> dispLetRecIn req f params tyeBody eBody e2
    Surface.LetTupleIn xL xR e1 e2 -> dispLetTupleIn req xL xR e1 e2
    Surface.LetOpenIn m e -> dispLetOpenIn req m e
    Surface.Sequential e1 e2 -> dispSequential req e1 e2
    Surface.Tuple e1 e2 -> dispTuple e1 e2
    Surface.IfThenElse e0 e1 e2 -> dispIfThenElse req e0 e1 e2
    Surface.As e1 tye2 -> dispAs req e1 tye2
    Surface.LamOpt (x, tye1) e2 -> dispLamOpt req x tye1 e2
    Surface.AppOptGiven e1 e2 -> dispAppOptGiven req e1 e2
    Surface.AppOptOmitted e1 -> dispAppOptOmitted req e1

instance Disp (Bta.BCLamBinderF ann) where
  dispGen _ = \case
    Surface.MandatoryBinder (x, tye) -> "(" <> disp x <+> ":" <+> disp tye <> ")"
    Surface.OptionalBinder (x, tye) -> "{" <> disp x <+> ":" <+> disp tye <> "}"

instance Disp (Bta.BCTypeExprF ann) where
  dispGen _ (Surface.TypeExpr (btc, _ann) typeExprMain) =
    dispWithBindingTime btc typeExprMain

instance Disp (Bta.BCTypeExprMainF ann) where
  dispGen req = \case
    Surface.TyName tyName args -> dispNameWithArgs req (disp tyName) (dispGen Atomic) args
    Surface.TyArrow (xOpt, tye1) tye2 -> dispArrowType req xOpt tye1 tye2
    Surface.TyOptArrow (x, tye1) tye2 -> dispOptArrowType req x tye1 tye2
    Surface.TyProduct tye1 tye2 -> dispProductType req tye1 tye2

instance Disp (Bta.BCArgForTypeF ann) where
  dispGen req = \case
    Surface.ExprArg e -> dispGen req e
    Surface.TypeArg tye -> dispGen req tye
