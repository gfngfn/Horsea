{-# LANGUAGE TemplateHaskell #-}

module Common.Formatter
  ( Disp (..),
    render,
    putRenderedLines,
    renderAtStage0,
    putRenderedLinesAtStage0,
    putRenderedLinesAtStage1,
  )
where

import Common.FrontError (FrontError (..))
import Common.LocationInFile (LocationInFile (LocationInFile), SpanInFile (..))
import Common.ParserUtil (ParseError (..))
import Data.Functor.Identity
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.TwoOrMore (TwoOrMore)
import Data.List.TwoOrMore qualified as TwoOrMore
import Data.Tensor.Matrix qualified as Matrix
import Data.Tensor.Vector qualified as Vector
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Tuple.Extra (first)
import Prettyprinter
import Prettyprinter.Render.Terminal
import Staged.BuiltIn.CompileTime (deriveDisp)
import Staged.BuiltIn.Core
import Staged.BuiltIn.Definitions (definitions)
import Staged.Core
import Staged.EvalError
import Staged.SrcSyntax
import Staged.Syntax
import Staged.TypeError
import Staged.Typechecker.Monad (InferableArgLogF (..))
import Surface.BindingTime.Analyzer qualified as Bta
import Surface.BindingTime.Core qualified as Bta
import Surface.BindingTime.Stager qualified as Bta
import Surface.Syntax qualified as Surface
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

appendWithComma :: Doc Ann -> Doc Ann -> Doc Ann
appendWithComma d1 d2 = d1 <> "," <+> d2

appendWithAsterisk :: Doc Ann -> Doc Ann -> Doc Ann
appendWithAsterisk d1 d2 = d1 <+> "*" <+> d2

dispQualified :: (Disp modName, Disp name) => [modName] -> name -> Doc Ann
dispQualified mods x = foldr (\modName d -> disp modName <> "." <> d) (disp x) mods

disps :: (Disp a) => [a] -> Doc Ann
disps = disps' disp

disps' :: (a -> Doc Ann) -> [a] -> Doc Ann
disps' f = \case
  [] -> mempty
  first' : rest -> foldl' (\doc x -> doc <> "," <+> f x) (f first') rest

deepenParenWhen :: Bool -> Doc Ann -> Doc Ann
deepenParenWhen b doc = if b then "(" <> nest 2 doc <> ")" else doc

dispBool :: Bool -> Doc Ann
dispBool b = if b then "true" else "false"

dispMaybe :: (Disp a) => Maybe a -> Doc Ann
dispMaybe = \case
  Nothing -> "Nothing"
  Just v -> "Just" <+> dispGen Atomic v

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

dispLamOms :: (Disp var, Disp ty, Disp expr) => Associativity -> Text -> var -> ty -> expr -> Doc Ann
dispLamOms req label x tye1 e2 =
  deepenParenWhen (req <= FunDomain) $
    group ("λ" <+> "?" <> disp label <+> disp x <+> ":" <+> disp tye1 <> "." <> nest 2 (line <> disp e2))

dispLamInf :: (Disp var, Disp ty, Disp expr) => Associativity -> var -> ty -> expr -> Doc Ann
dispLamInf req x tye1 e2 =
  deepenParenWhen (req <= FunDomain) $
    group ("λ{" <> disp x <+> ":" <+> disp tye1 <> "}." <> nest 2 (line <> disp e2))

dispLamInfType :: (Disp tyvar, Disp ty) => Associativity -> tyvar -> ty -> Doc Ann
dispLamInfType req tyvar1 tye2 =
  deepenParenWhen (req <= FunDomain) $
    group ("Λ{" <> disp tyvar1 <> "}." <> nest 2 (line <> disp tye2))

dispLamType :: (Disp tyvar, Disp ty) => Associativity -> tyvar -> ty -> Doc Ann
dispLamType req tyvar1 tye2 =
  deepenParenWhen (req <= FunDomain) $
    group ("Λ" <> disp tyvar1 <> "." <> nest 2 (line <> disp tye2))

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

dispAppOms :: (Disp expr) => Associativity -> expr -> Label -> expr -> Doc Ann
dispAppOms req e1 label e2 =
  deepenParenWhen (req <= Atomic) $
    group (dispGen FunDomain e1 <+> "?" <> disp label <> nest 2 (line <> dispGen Atomic e2))

dispAppInfGiven :: (Disp expr) => Associativity -> expr -> expr -> Doc Ann
dispAppInfGiven req e1 e2 =
  deepenParenWhen (req <= Atomic) $
    group (dispGen FunDomain e1 <> nest 2 (line <> "{" <> disp e2 <> "}"))

dispAppInfOmitted :: (Disp expr) => Associativity -> expr -> Doc Ann
dispAppInfOmitted req e1 =
  deepenParenWhen (req <= Atomic) $
    group (dispGen FunDomain e1 <> nest 2 (line <> "_"))

dispAppInfType :: (Disp expr, Disp ty) => Associativity -> expr -> ty -> Doc Ann
dispAppInfType req e1 tye2 =
  deepenParenWhen (req <= Atomic) $
    group (dispGen FunDomain e1 <> nest 2 (line <> "{type" <+> disp tye2 <> "}"))

dispAppType :: (Disp expr, Disp ty) => Associativity -> expr -> ty -> Doc Ann
dispAppType req e1 tye2 =
  deepenParenWhen (req <= Atomic) $
    group (dispGen FunDomain e1 <> nest 2 (line <> dispGen Atomic tye2))

dispLetIn :: (Disp var, Disp param, Disp ty, Disp expr) => Associativity -> var -> [param] -> Maybe ty -> expr -> expr -> Doc Ann
dispLetIn req x params tyeOpt e1 e2 =
  deepenParenWhen (req <= FunDomain) $
    group ("let" <+> dp <+> dt <+> "=" <> nest 2 (line <> disp e1) <+> "in" <> line <> disp e2)
  where
    dp = sep (disp x : map disp params)
    dt =
      case tyeOpt of
        Just tye -> ":" <+> disp tye
        Nothing -> mempty

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

dispLetInWithoutAnnot :: (Disp var, Disp expr) => Associativity -> var -> expr -> expr -> Doc Ann
dispLetInWithoutAnnot req x e1 e2 =
  deepenParenWhen (req <= FunDomain) $
    group ("let" <+> disp x <+> "=" <> nest 2 (line <> disp e1) <+> "in" <> line <> disp e2)

dispLetOpenIn :: (Disp var, Disp expr) => Associativity -> var -> expr -> Doc Ann
dispLetOpenIn req m e =
  deepenParenWhen (req <= FunDomain) $
    group ("let open" <+> disp m <+> "in" <> line <> disp e)

dispLetTupleIn :: (Disp var, Disp expr) => Associativity -> TwoOrMore var -> expr -> expr -> Doc Ann
dispLetTupleIn req xs e1 e2 =
  deepenParenWhen (req <= FunDomain) $
    group ("let (" <> TwoOrMore.foldl1 appendWithComma (fmap disp xs) <> ") =" <+> disp e1 <+> "in" <> line <> disp e2)

dispSequential :: (Disp expr) => Associativity -> expr -> expr -> Doc Ann
dispSequential req e1 e2 =
  deepenParenWhen (req <= FunDomain) $
    group (disp e1 <> ";" <> line <> disp e2)

dispTuple :: (Disp expr) => TwoOrMore expr -> Doc Ann
dispTuple es =
  "(" <> nest 2 (foldl1 appendWithComma (fmap disp es)) <> ")"

dispConstructorApp :: (Disp expr) => Associativity -> ConstructorName -> [expr] -> Doc Ann
dispConstructorApp req ctor args =
  case args of
    [] -> disp ctor
    _ : _ -> deepenParenWhen (req <= Atomic) (foldl' (<+>) (disp ctor) (map disp args))

dispIfThenElse :: (Disp expr) => Associativity -> expr -> expr -> expr -> Doc Ann
dispIfThenElse req e0 e1 e2 =
  deepenParenWhen (req <= FunDomain) $
    group (docIf <+> docThen <+> docElse)
  where
    docIf = "if" <> nest 2 (line <> disp e0)
    docThen = "then" <> nest 2 (line <> disp e1)
    docElse = "else" <> nest 2 (line <> disp e2)

dispCase :: (Disp expr, Disp br) => Associativity -> expr -> NonEmpty br -> Doc Ann
dispCase req e0 branches =
  deepenParenWhen (req <= FunDomain) $
    group (docCase <+> docBranches <+> "end")
  where
    docCase = "case" <> nest 2 (line <> disp e0)
    docBranches = "of" <> foldl1 (<>) (fmap (\br -> nest 2 (line <> disp br)) branches)

dispBranch :: (Disp pat, Disp expr) => pat -> expr -> Doc Ann
dispBranch pat e = "|" <+> disp pat <+> "->" <+> nest 4 (line <> disp e)

dispStringLiteral :: Text -> Doc Ann
dispStringLiteral t = "\"" <> disp t <> "\"" -- TODO (enhance): escape special characters

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

dispForAllType :: (Disp ty) => Associativity -> AssTypeVar -> ty -> Doc Ann
dispForAllType req atyvar tye =
  deepenParenWhen (req <= Atomic) $
    group ("forall" <+> disp atyvar <> "." <+> disp tye)

dispListType :: (Disp ty) => Associativity -> ty -> Doc Ann
dispListType req tye =
  deepenParenWhen (req <= Atomic) $
    group ("List" <+> dispGen Atomic tye)

dispMaybeType :: (Disp ty) => Associativity -> ty -> Doc Ann
dispMaybeType req tye =
  deepenParenWhen (req <= Atomic) $
    group ("Maybe" <+> dispGen Atomic tye)

dispProduct :: (Disp ty) => Associativity -> ty -> NonEmpty (Text, ty) -> Doc Ann
dispProduct req tye1 rest =
  deepenParenWhen (req <= Atomic) $
    group (dispGen Atomic tye1 <+> foldl1 (<+>) (fmap (\(op, tye) -> disp op <+> dispGen Atomic tye) rest))

dispProductType :: (Disp ty) => Associativity -> TwoOrMore ty -> Doc Ann
dispProductType req tyes =
  dispProduct req a0tye1 (fmap ("*",) a0tyesRest)
  where
    (a0tye1, a0tyesRest) = TwoOrMore.decompose1 tyes

dispArrowType :: (Disp var, Disp ty1, Disp ty2) => Associativity -> Maybe Label -> Maybe var -> ty1 -> ty2 -> Doc Ann
dispArrowType req labelOpt xOpt tye1 tye2 =
  deepenParenWhen (req <= FunDomain) $
    group (docDom <+> "->" <> line <> disp tye2)
  where
    docDom =
      case labelOpt of
        Just label -> "#" <> disp label <+> docDom'
        Nothing -> docDom'

    docDom' =
      case xOpt of
        Just x -> "(" <> disp x <+> ":" <+> disp tye1 <> ")"
        Nothing -> dispGen FunDomain tye1

dispNondepArrowType :: (Disp ty) => Associativity -> Maybe Label -> ty -> ty -> Doc Ann
dispNondepArrowType req labelOpt =
  dispArrowType req labelOpt (Nothing :: Maybe Text)

dispInfArrowType :: (Disp var, Disp ty1, Disp ty2) => Associativity -> var -> ty1 -> ty2 -> Doc Ann
dispInfArrowType req x tye1 tye2 =
  deepenParenWhen (req <= FunDomain) $
    group (docDom <+> "->" <> line <> disp tye2)
  where
    docDom = "{" <> disp x <+> ":" <+> disp tye1 <> "}"

dispOmsArrowType :: (Disp var, Disp ty) => Associativity -> Text -> Maybe var -> ty -> ty -> Doc Ann
dispOmsArrowType req label xOpt tye1 tye2 =
  deepenParenWhen (req <= FunDomain) $
    group ("?" <> disp label <+> docDom <+> "->" <> line <> disp tye2)
  where
    docDom =
      case xOpt of
        Just x -> "(" <> disp x <+> ":" <+> disp tye1 <+> "}"
        Nothing -> disp tye1

dispRefinementType :: (Disp var, Disp ty, Disp expr) => Associativity -> var -> ty -> expr -> Doc Ann
dispRefinementType _req x tye eProp =
  "{" <> disp x <+> ":" <+> disp tye <+> "|" <+> disp eProp <> "}"

dispInternalRefinementType :: (Disp ty, Disp expr) => Associativity -> ty -> expr -> Doc Ann
dispInternalRefinementType _req tye ePred =
  "{" <> disp tye <+> "|" <+> disp ePred <> "}"

dispInternalRefinementListType :: (Disp ty, Disp expr) => Associativity -> ty -> expr -> Doc Ann
dispInternalRefinementListType _req tye ePred =
  "{" <> dispListType Outermost tye <+> "|" <+> disp ePred <> "}"

dispListLiteral :: (Disp e) => [e] -> Doc Ann
dispListLiteral es =
  "[" <> disps es <> "]"

dispStringListLiteral :: [Text] -> Doc Ann
dispStringListLiteral es =
  "[" <> disps' dispStringLiteral es <> "]"

dispPairLiteral :: (Disp e) => (e, e) -> Doc Ann
dispPairLiteral (e1, e2) =
  "(" <> disp e1 <> "," <+> disp e2 <> ")"

dispVectorLiteral :: [Int] -> Doc Ann
dispVectorLiteral ns =
  encloseSep ("[|" <> space) (space <> "|]") (";" <> softline) (disp <$> ns)

dispMatrixLiteral :: [[Int]] -> Doc Ann
dispMatrixLiteral nss =
  encloseSep ("[#" <> space) (space <> "#]") (";" <> softline) (dispRowContents <$> nss)

dispRowContents :: (Disp a) => [a] -> Doc Ann
dispRowContents row =
  commaSep (map disp row)

dispNameWithArgs :: Associativity -> Doc Ann -> (arg -> Doc Ann) -> [arg] -> Doc Ann
dispNameWithArgs req name dispArg args =
  case args of
    [] -> name
    _ : _ -> deepenParenWhen (req <= Atomic) (foldl' (<+>) name (map dispArg args))

dispDatasetParam :: (a -> Doc Ann) -> (f a -> Doc Ann) -> DatasetParam f a -> Doc Ann
dispDatasetParam dispElem dispList DatasetParam {numTrain, numTest, image, label} =
  dispElem numTrain <> " " <> dispElem numTest <> " " <> dispList image <> " " <> dispList label

dispDatasetParam0 :: DatasetParam [] Int -> Doc Ann
dispDatasetParam0 = dispDatasetParam disp dispListLiteral

dispLongName :: (Disp var) => [var] -> var -> Doc Ann
dispLongName ms x =
  foldr (\m doc -> disp m <> "." <> doc) (disp x) ms

instance Disp Text where
  dispGen _ = pretty

instance Disp String where
  dispGen _ = pretty

instance Disp Int where
  dispGen _ = pretty

instance Disp Double where
  dispGen _ = pretty

instance (Disp sv) => Disp (AssVarF sv) where
  dispGen _ (AssVarStatic x) = disp x
  dispGen _ (AssVarDynamic n) = "#S" <> disp n

instance Disp Symbol where
  dispGen _ (Symbol n) = "#S" <> disp n

instance Disp AssTypeVar where
  dispGen _ (AssTypeVar n) = "'a" <> disp n

instance (Disp e) => Disp (Literal e) where
  dispGen _ = \case
    LitInt n -> pretty n
    LitFloat r -> pretty r
    LitUnit -> "()"
    LitBool b -> dispBool b
    LitString t -> dispStringLiteral t
    LitList es -> dispListLiteral es
    LitVec ns -> dispVectorLiteral ns
    LitMat nss -> dispMatrixLiteral nss

instance Disp (ExprF ann) where
  dispGen req (Expr _ann exprMain) = dispGen req exprMain

instance Disp (ExprMainF ann) where
  dispGen req = \case
    Literal lit -> dispGen req lit
    Constructor (ms, ctor) -> dispLongName ms ctor
    Var (ms, x) -> dispLongName ms x
    Lam Nothing labelOpt (x, tye1) e2 -> dispNonrecLam req labelOpt x tye1 e2
    Lam (Just (f, tyeRec)) labelOpt (x, tye1) e2 -> dispRecLam req f tyeRec labelOpt x tye1 e2
    App e1 labelOpt e2 -> dispApp req e1 labelOpt e2
    LamOms label (x, tye1) e2 -> dispLamOms req label x tye1 e2
    AppOms e1 label e2 -> dispAppOms req e1 label e2
    LamInf (x, tye1) e2 -> dispLamInf req x tye1 e2
    AppInfGiven e1 e2 -> dispAppInfGiven req e1 e2
    AppInfOmitted e1 -> dispAppInfOmitted req e1
    LetIn x params tyeOpt e1 e2 -> dispLetIn req x params tyeOpt e1 e2
    LetRecIn x params tye e1 e2 -> dispLetRecIn req x params tye e1 e2
    LetTupleIn xs e1 e2 -> dispLetTupleIn req xs e1 e2
    LetOpenIn m e -> dispLetOpenIn req m e
    Sequential e1 e2 -> dispSequential req e1 e2
    Tuple es -> dispTuple es
    IfThenElse e0 e1 e2 -> dispIfThenElse req e0 e1 e2
    Case e0 branches -> dispCase req e0 branches
    As e1 tye2 -> dispAs req e1 tye2
    Bracket e1 -> dispBracket e1
    Escape e1 -> dispEscape e1
    LamInfType tyvar1 e2 -> dispLamInfType req tyvar1 e2
    AppInfType e1 tye2 -> dispAppInfType req e1 tye2
    Persistent e1 -> dispPersistent e1
    TyVar tyvar -> disp tyvar
    TyArrow labelOpt (xOpt, tye1) tye2 -> dispArrowType req labelOpt xOpt tye1 tye2
    TyOmsArrow label (xOpt, tye1) tye2 -> dispOmsArrowType req label xOpt tye1 tye2
    TyInfArrow (x, tye1) tye2 -> dispInfArrowType req x tye1 tye2
    TyRefinement x tye1 e2 -> "(" <> disp x <+> ":" <+> disp tye1 <+> "|" <+> disp e2 <+> ")"
    Product tye1 rest -> dispProduct req tye1 (fmap (first snd) rest)
    TyForAll tyvar tye -> "forall" <+> disp tyvar <+> "->" <+> disp tye

instance Disp (LamBinderF ann) where
  dispGen _ = \case
    MandatoryBinder Nothing (x, tye) -> "(" <> disp x <+> ":" <+> disp tye <> ")"
    MandatoryBinder (Just label) (x, tye) -> "#" <> disp label <+> "(" <> disp x <+> ":" <+> disp tye <> ")"
    OmissibleBinder label (x, tye) -> "?" <> disp label <+> "(" <> disp x <+> ":" <+> disp tye <> ")"
    InferableBinder (x, tye) -> "{" <> disp x <+> ":" <+> disp tye <> "}"
    TypeBinder tyvar -> "{type" <+> disp tyvar <+> "}"

instance Disp (BranchF ann) where
  dispGen _ (Branch pat e) = dispBranch pat e

instance Disp (PatternF ann) where
  dispGen req (Pattern _ann patMain) = dispGen req patMain

instance Disp (PatternMainF ann) where
  dispGen req = \case
    PatConstructor (mods, ctor) -> dispQualified mods ctor
    PatApp pat1 pat2 -> dispApp req pat1 Nothing pat2
    PatVar x -> disp x
    PatBool b -> dispBool b
    PatListNil -> "[]"

instance Disp TypeVar where
  dispGen _ (TypeVar a) = "'" <> disp a

$(deriveDisp definitions)

instance Disp BuiltIn where
  dispGen req = \case
    BuiltInArity1 bi1 -> dispGen req bi1
    BuiltInArity2 bi2 -> dispGen req bi2
    BuiltInArity3 bi3 -> dispGen req bi3
    BuiltInArity4 bi4 -> dispGen req bi4
    BuiltInArity5 bi5 -> dispGen req bi5
    BuiltInArity6 bi6 -> dispGen req bi6
    BuiltInArity7 bi7 -> dispGen req bi7
    BuiltInOther s -> "OTHER '" <> disp s <> "'"

instance (Disp e) => Disp (Surface.Literal e) where
  dispGen _ = \case
    Surface.LitInt n -> pretty n
    Surface.LitFloat r -> pretty r
    Surface.LitUnit -> "()"
    Surface.LitBool b -> dispBool b
    Surface.LitString t -> dispStringLiteral t
    Surface.LitList es -> dispListLiteral es
    Surface.LitVec ns -> dispVectorLiteral ns
    Surface.LitMat nss -> dispMatrixLiteral nss

instance Disp Surface.Expr where
  dispGen req (Surface.Expr _ann exprMain) = dispGen req exprMain

instance Disp Surface.ExprMain where
  dispGen req = \case
    Surface.Literal lit -> dispGen req lit
    Surface.Constructor (ms, ctor) -> dispLongName ms ctor
    Surface.Var (ms, x) -> dispLongName ms x
    Surface.Lam Nothing labelOpt (x, tye1) e2 -> dispNonrecLam req labelOpt x tye1 e2
    Surface.Lam (Just (f, tyeRec)) labelOpt (x, tye1) e2 -> dispRecLam req f tyeRec labelOpt x tye1 e2
    Surface.App e1 labelOpt e2 -> dispApp req e1 labelOpt e2
    Surface.LetIn x params tyeBodyOpt eBody e2 -> dispLetIn req x params tyeBodyOpt eBody e2
    Surface.LetRecIn f params tyeBody eBody e2 -> dispLetRecIn req f params tyeBody eBody e2
    Surface.LetTupleIn xs e1 e2 -> dispLetTupleIn req xs e1 e2
    Surface.LetOpenIn m e -> dispLetOpenIn req m e
    Surface.Sequential e1 e2 -> dispSequential req e1 e2
    Surface.Tuple es -> dispTuple es
    Surface.IfThenElse e0 e1 e2 -> dispIfThenElse req e0 e1 e2
    Surface.As e1 tye2 -> dispAs req e1 tye2
    Surface.LamOms label (x, tye1) e2 -> dispLamOms req label x tye1 e2
    Surface.AppOms e1 label e2 -> dispAppOms req e1 label e2
    Surface.LamInf (x, tye1) e2 -> dispLamInf req x tye1 e2
    Surface.AppInfGiven e1 e2 -> dispAppInfGiven req e1 e2
    Surface.AppInfOmitted e1 -> dispAppInfOmitted req e1
    Surface.TyArrow labelOpt (xOpt, tye1) tye2 -> dispArrowType req labelOpt xOpt tye1 tye2
    Surface.TyOmsArrow label (xOpt, tye1) tye2 -> dispOmsArrowType req label xOpt tye1 tye2
    Surface.TyInfArrow (x, tye1) tye2 -> dispInfArrowType req x tye1 tye2
    Surface.TyRefinement x tye1 e2 -> dispRefinementType req x tye1 e2
    Surface.Product tye1 rest -> dispProduct req tye1 (fmap (first snd) rest)

instance Disp Surface.LamBinder where
  dispGen _ = \case
    Surface.MandatoryBinder Nothing (x, tye) -> "(" <> disp x <+> ":" <+> disp tye <> ")"
    Surface.MandatoryBinder (Just label) (x, tye) -> "#" <> disp label <+> "(" <> disp x <+> ":" <+> disp tye <> ")"
    Surface.OmissibleBinder label (x, tye) -> "?" <> disp label <+> "(" <> disp x <+> ":" <+> disp tye <> ")"
    Surface.InferableBinder (x, tye) -> "{" <> disp x <+> ":" <+> disp tye <> "}"

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
    A0LetTupleIn xs a0e1 a0e2 -> dispLetTupleIn req xs a0e1 a0e2
    A0Sequential a0e1 a0e2 -> dispSequential req a0e1 a0e2
    A0Tuple a0es -> dispTuple a0es
    A0Constructor ctor a0es -> dispConstructorApp req ctor a0es
    A0Bracket a1e1 -> dispBracket a1e1
    A0IfThenElse a0e0 a0e1 a0e2 -> dispIfThenElse req a0e0 a0e1 a0e2
    A0Case a0e0 a0branches -> dispCase req a0e0 a0branches
    A0TyEqAssert _loc ty1eq ->
      let (a1tye1, a1tye2) = decomposeType1Equation ty1eq
       in group (assertionStyle ("{" <> dispBracket a1tye1 <+> "=>" <+> dispBracket a1tye2 <> "}"))
    A0RefinementAssert _loc a0ePred a0eTarget ->
      deepenParenWhen (req <= Atomic) $
        "ASSERT" <+> disp a0ePred <+> "FOR" <+> disp a0eTarget
    A0LamType atyvar1 a0e2 ->
      dispLamType req atyvar1 a0e2
    A0AppType a0e1 sa0tye2 ->
      dispAppType req a0e1 sa0tye2

instance (Disp sv) => Disp (Ass0BranchF sv) where
  dispGen _ (A0Branch a0pat a0e) = dispBranch a0pat a0e

instance (Disp sv) => Disp (Ass0PatternF sv) where
  dispGen req = \case
    A0PatConstructor ctor a0pats -> dispConstructorApp req ctor a0pats
    A0PatVar x -> disp x
    A0PatBool b -> dispBool b
    A0PatListNil -> "[]"
    A0PatListCons a0pat1 a0pat2 -> dispConstructorApp req "::" [a0pat1, a0pat2]

instance (Disp sv) => Disp (Ass1ExprF sv) where
  dispGen req = \case
    A1Literal lit -> disp lit
    A1Var x -> disp x
    A1BuiltInName a1builtInName -> disp a1builtInName
    A1Lam Nothing (x, a1tye1) a1e2 -> dispNonrecLam req Nothing x a1tye1 a1e2
    A1Lam (Just (f, a1tyeRec)) (x, a1tye1) a1e2 -> dispRecLam req f a1tyeRec Nothing x a1tye1 a1e2
    A1App a1e1 a1e2 -> dispApp req a1e1 Nothing a1e2
    A1LetIn (x, a1tye0) a1e1 a1e2 -> dispLetInWithAnnot req x a1tye0 a1e1 a1e2
    A1LetTupleIn xs a1e1 a1e2 -> dispLetTupleIn req xs a1e1 a1e2
    A1Sequential a1e1 a1e2 -> dispSequential req a1e1 a1e2
    A1Tuple a1es -> dispTuple a1es
    A1Constructor ctor a1es -> dispConstructorApp req ctor a1es
    A1IfThenElse a1e0 a1e1 a1e2 -> dispIfThenElse req a1e0 a1e1 a1e2
    A1Case a1e0 a1branches -> dispCase req a1e0 a1branches
    A1Escape a0e1 -> dispEscape a0e1
    A1LamType atyvar1 a1e2 -> dispLamType req atyvar1 a1e2
    A1AppType a1e1 a1tye2 -> dispAppType req a1e1 a1tye2

instance (Disp sv) => Disp (Ass1BranchF sv) where
  dispGen _ (A1Branch a1pat a1e) = dispBranch a1pat a1e

instance (Disp sv) => Disp (Ass1PatternF sv) where
  dispGen req = \case
    A1PatConstructor ctor a0pats -> dispConstructorApp req ctor a0pats
    A1PatVar x -> disp x
    A1PatBool b -> dispBool b
    A1PatListNil -> "[]"
    A1PatListCons a1pat1 a1pat2 -> dispConstructorApp req "::" [a1pat1, a1pat2]

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
    ATyPrimChar -> "Char"
    ATyPrimClipGrad -> "ClipGrad"
    ATyPrimOutChannel -> "OutChannel"
    ATyPrimVarStoreInit -> "VarStoreInit"

instance Disp Ass0PrimType where
  dispGen req = \case
    A0TyPrimBase tyPrimBase -> disp tyPrimBase
    A0TyTensor [n] -> dispNameWithArgs req "Vec" disp [n]
    A0TyTensor [m, n] -> dispNameWithArgs req "Mat" disp [m, n]
    A0TyTensor ns -> dispNameWithArgs req "Tensor" dispListLiteral [ns]
    A0TyDataset datasetParam -> dispNameWithArgs req "Dataset" dispDatasetParam0 [datasetParam]
    A0TyLstm i h -> dispNameWithArgs req "Lstm" disp [i, h]
    A0TyTextHelper labels -> dispNameWithArgs req "TextHelper" disp [labels]

instance (Disp sv) => Disp (Ass0TypeExprF sv) where
  dispGen req = \case
    A0TyPrim a0tyPrim Nothing -> disp a0tyPrim
    A0TyPrim a0tyPrim (Just a0ePred) -> dispInternalRefinementType req a0tyPrim a0ePred
    A0TyVar atyvar -> disp atyvar
    A0TyList a0tye Nothing -> dispListType req a0tye
    A0TyList a0tye (Just a0ePred) -> dispInternalRefinementListType req a0tye a0ePred
    A0TyMaybe a0tye -> dispMaybeType req a0tye
    A0TyProduct a0tyes -> dispProductType req a0tyes
    A0TyArrow labelOpt (xOpt, a0tye1) a0tye2 -> dispArrowType req labelOpt xOpt a0tye1 a0tye2
    A0TyCode a1tye1 -> dispBracket a1tye1
    A0TyInfArrow (x, a0tye1) a0tye2 -> dispInfArrowType req x a0tye1 a0tye2
    A0TyOmsArrow label (xOpt, a0tye1) a0tye2 -> dispOmsArrowType req label xOpt a0tye1 a0tye2
    A0TyForAll atyvar a0tye -> dispForAllType req atyvar a0tye

instance (Disp sv) => Disp (StrictAss0TypeExprF sv) where
  dispGen req = \case
    SA0TyPrim a0tyPrim Nothing -> disp a0tyPrim
    SA0TyPrim a0tyPrim (Just a0ePred) -> dispInternalRefinementType req a0tyPrim a0ePred
    SA0TyVar atyvar -> disp atyvar
    SA0TyList sa0tye Nothing -> dispListType req sa0tye
    SA0TyList sa0tye (Just a0ePred) -> dispInternalRefinementListType req sa0tye a0ePred
    SA0TyMaybe sa0tye -> dispMaybeType req sa0tye
    SA0TyProduct sa0tyes -> dispProductType req sa0tyes
    SA0TyArrow (xOpt, sa0tye1) sa0tye2 -> dispArrowType req Nothing xOpt sa0tye1 sa0tye2
    SA0TyCode a1tye1 -> dispBracket a1tye1
    SA0TyForAll atyvar sa0tye -> dispForAllType req atyvar sa0tye

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
    A1TyLstm a0eInputSize a0eHiddenSize ->
      dispNameWithArgs req "Lstm" disp [a0eInputSize, a0eHiddenSize]
    A1TyTextHelper a0eLabels ->
      dispNameWithArgs req "TextHelper" disp [a0eLabels]

instance (Disp sv) => Disp (Ass1TypeExprF sv) where
  dispGen req = \case
    A1TyPrim a1tyPrim -> dispGen req a1tyPrim
    A1TyList a1tye -> dispListType req a1tye
    A1TyMaybe a1tye -> dispMaybeType req a1tye
    A1TyVar atyvar -> disp atyvar
    A1TyProduct a1tyes -> dispProductType req a1tyes
    A1TyArrow labelOpt a1tye1 a1tye2 -> dispNondepArrowType req labelOpt a1tye1 a1tye2
    A1TyOmsArrow label a1tye1 a1tye2 -> dispOmsArrowType req label (Nothing :: Maybe Text) a1tye1 a1tye2
    A1TyForAll atyvar a1tye2 -> dispForAllType req atyvar a1tye2

instance Disp FrontError where
  dispGen _ = \case
    FrontLexingError s ->
      disp (Text.pack s)
    FrontParseError parseErrors ->
      foldl' (\doc parseError -> doc <> hardline <> disp parseError) mempty parseErrors

instance Disp ParseError where
  dispGen _ = \case
    ParseError spanInFile message ->
      disp spanInFile
        <> hardline
        <> disp message
    UnexpectedEndOfInput ->
      "Unexpected end of input"

instance (Disp a) => Disp (Matrix.ConstructionError a) where
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
    Unsupported spanInFile detail ->
      "Unsupported feature" <+> disp spanInFile <> hardline <+> disp detail
    InvalidSyntaxAsExpr spanInFile ->
      "Invalid syntax as expression" <+> disp spanInFile
    InvalidSyntaxAsPattern spanInFile ->
      "Invalid syntax as pattern" <+> disp spanInFile
    InvalidSyntaxAsTypeExpr spanInFile ->
      "Invalid syntax as type expression" <+> disp spanInFile
    UnboundVar spanInFile ms x ->
      "Unbound variable" <+> dispLongName ms x <+> disp spanInFile
    UnboundTypeVar spanInFile tyvar ->
      "Unbound type variable" <+> disp tyvar <+> disp spanInFile
    UnboundModule spanInFile m ->
      "Unbound module" <+> disp m <+> disp spanInFile
    NotAStage0Var spanInFile x ->
      "Not a stage-0 variable:" <+> disp x <+> disp spanInFile
    NotAStage1Var spanInFile x ->
      "Not a stage-1 variable:" <+> disp x <+> disp spanInFile
    UnboundConstructor spanInFile mods ctor ->
      "Unbound constructor" <+> dispQualified mods ctor <+> disp spanInFile
    UnboundConstructorOrInvalidArity spanInFile mods ctor n ->
      "Unbound constructor or invalid arity:" <+> dispQualified mods ctor <> "," <+> disp n <+> disp spanInFile
    UnknownTypeOrInvalidArityAtStage0 spanInFile mods tyName n ->
      "Unknown type or invalid arity (at stage 0):" <+> dispQualified mods tyName <> "," <+> disp n <+> disp spanInFile
    UnknownTypeOrInvalidArityAtStage1 spanInFile mods tyName n ->
      "Unknown type or invalid arity (at stage 1):" <+> dispQualified mods tyName <> "," <+> disp n <+> disp spanInFile
    NotAnIntLitArgAtStage0 spanInFile a0e ->
      "An argument expression at stage 0 is not an integer literal:" <+> stage0Style (disp a0e) <+> disp spanInFile
    NotAnIntListLitArgAtStage0 spanInFile a0e ->
      "An argument expression at stage 0 is not an integer list literal:" <+> stage0Style (disp a0e) <+> disp spanInFile
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
    CannotUseLamInfAtStage1 spanInFile ->
      "Cannot use function with implicit parameters (fun{...} ->) at stage 1" <+> disp spanInFile
    CannotUseAppInfGivenAtStage1 spanInFile ->
      "Cannot use application for implicit parameters (... {...}) at stage 1" <+> disp spanInFile
    CannotUseAppInfOmittedAtStage1 spanInFile ->
      "Cannot use application for implicit parameters (... _) at stage 1" <+> disp spanInFile
    FunctionTypeCannotBeDependentAtStage1 spanInFile x ->
      "Function types cannot be dependent at stage 1:" <+> disp x <+> disp spanInFile
    CannotUseCodeTypeAtStage1 spanInFile ->
      "Cannot use code types at stage 1" <+> disp spanInFile
    CannotUseInfArrowTypeAtStage1 spanInFile ->
      "Cannot use implicit function types at stage 1" <+> disp spanInFile
    CannotUseRefinementTypeAtStage1 spanInFile ->
      "Cannot use refinement types at stage 1" <+> disp spanInFile
    CannotUsePersistent spanInFile ->
      "Cannot use persistence here" <+> disp spanInFile
    CannotUseNormalArgAtStage1 spanInFile ->
      "Cannot use normal arguments at stage 1" <+> disp spanInFile
    VarOccursFreelyInAss0Type spanInFile x a0result ->
      "Variable" <+> disp x <+> "occurs in stage-0 type" <+> stage0Style (disp a0result) <+> disp spanInFile
    VarOccursFreelyInAss1Type spanInFile x a1result ->
      "Variable" <+> disp x <+> "occurs in stage-1 type" <+> stage1Style (disp a1result) <+> disp spanInFile
    InvalidMatrixLiteral spanInFile e ->
      "Invalid matrix literal;" <+> disp e <+> disp spanInFile
    CannotMergeTypesByConditional0 spanInFile pairs condErr ->
      "Cannot merge stage-0 types by conditionals"
        <+> disp spanInFile
        <> foldl1 (<>) (fmap (\(_a0pat, a0tye) -> nest 2 (hardline <> stage0Style (disp a0tye))) pairs)
        <> hardline
        <> disp condErr
    CannotMergeTypesByConditional1 spanInFile pairs condErr ->
      "Cannot merge stage-1 types by conditionals"
        <+> disp spanInFile
        <> foldl1 (<>) (fmap (\(_a0pat, a1tye) -> nest 2 (hardline <> stage1Style (disp a1tye))) pairs)
        <> hardline
        <> disp condErr
    CannotMergeResultsByConditionals spanInFile pairs ->
      "Cannot merge results by conditionals"
        <+> disp spanInFile
        <> foldl1 (<>) (fmap (\(_a0pat, result) -> nest 2 (hardline <> disp result)) pairs)
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
    CannotInferImplicit spanInFile x a0tye appCtx ->
      "Cannot infer an implicit argument for"
        <+> stage0Style (disp x)
        <+> disp spanInFile
        <> hardline
        <+> "application context:"
        <> nest 2 (hardline <> disps appCtx)
        <> hardline
        <+> "type:"
        <> nest 2 (hardline <> stage0Style (disp a0tye))
    CannotInferTypeVariableInstance0 spanInFile atyvar appCtx a0tye ->
      "Cannot infer an instance for type variable"
        <+> stage0Style (disp atyvar)
        <+> disp spanInFile
        <> hardline
        <+> "application context:"
        <> nest 2 (hardline <> disps appCtx)
        <> hardline
        <+> "type:"
        <> nest 2 (hardline <> stage0Style (disp a0tye))
    CannotInferTypeVariableInstance1 spanInFile atyvar appCtx a1tye ->
      "Cannot infer an instance for type variable"
        <+> stage1Style (disp atyvar)
        <+> disp spanInFile
        <> hardline
        <+> "application context:"
        <> nest 2 (hardline <> disps appCtx)
        <> hardline
        <+> "type:"
        <> nest 2 (hardline <> stage1Style (disp a1tye))
    CannotInstantiateTypeVariableGuidedByAssertion0 spanInFile atyvar a0tye1 a0tye2 ->
      "Cannot instantiate type variable"
        <+> stage0Style (disp atyvar)
        <+> disp spanInFile
        <> hardline
        <+> "left:"
        <> nest 2 (hardline <> stage0Style (disp a0tye1))
        <> hardline
        <+> "right:"
        <> nest 2 (hardline <> stage0Style (disp a0tye2))
    Stage1IfThenElseRestrictedToEmptyContext spanInFile appCtx ->
      "Stage-1 if-expressions are restricted to be used at empty application contexts"
        <+> disp spanInFile
        <> hardline
        <+> "application context:"
        <> nest 2 (hardline <> disps appCtx)
    Stage1CaseRestrictedToEmptyContext spanInFile appCtx ->
      "Stage-1 case-expressions are restricted to be used at empty application contexts"
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
    LetRecParamsCannotStartWithImplicit spanInFile ->
      "Recursive function definitions cannot have an implicit parameter as the first one" <+> disp spanInFile
    LetRecRequiresNonEmptyParams spanInFile ->
      "Recursive function definitions require at least one parameter" <+> disp spanInFile
    CannotSynthesizeTypeFromExpr spanInFile ->
      "Cannot synthesize the type of the expression; consider using `as`" <+> disp spanInFile
    CannotForceType0 spanInFile a0tye ->
      "Cannot force type" <+> stage0Style (disp a0tye) <+> "on the expression" <+> disp spanInFile
    CannotForceType1 spanInFile a1tye ->
      "Cannot force type" <+> stage1Style (disp a1tye) <+> "on the expression" <+> disp spanInFile
    CannotForceTypeOnPattern0 spanInFile a0tye ->
      "Cannot force type" <+> stage0Style (disp a0tye) <+> "on the pattern" <+> disp spanInFile
    CannotForceTypeOnPattern1 spanInFile a1tye ->
      "Cannot force type" <+> stage1Style (disp a1tye) <+> "on the pattern" <+> disp spanInFile
    ApplicationLabelMismatch spanInFile appCtx labelOptGot labelOptExpected ->
      "Label mismatch"
        <+> disp spanInFile
        <> hardline
        <+> "application context:"
        <> nest 2 (hardline <> disps appCtx)
        <> hardline
        <+> "expected"
        <+> labelExpected
        <+> "but got"
        <+> labelGot
      where
        labelExpected = maybe "no label" quote labelOptExpected
        labelGot = maybe "no label" quote labelOptGot
        quote t = "'#" <> disp t <> "'"
    NotAStage0TypeVar spanInFile tyvar ->
      "Not a stage-0 type variable:" <+> disp tyvar <+> disp spanInFile
    NotAStage1TypeVar spanInFile tyvar ->
      "Not a stage-1 type variable:" <+> disp tyvar <+> disp spanInFile
    LetTupleLengthMismatch0 spanInFile xs a0tyes ->
      "Tuple length mismatch"
        <+> disp spanInFile
        <> hardline
        <> ("expected tuples of length" <+> disp (length xs) <> ":")
        <> nest 2 (dispTuple xs)
        <> ("but got tuples of length" <+> disp (length a0tyes) <> ":")
        <> nest 2 (dispProductType Outermost a0tyes)
    LetTupleLengthMismatch1 spanInFile xs a1tyes ->
      "Tuple length mismatch"
        <+> disp spanInFile
        <> hardline
        <> ("expected tuples of length" <+> disp (length xs) <> ":")
        <> nest 2 (dispTuple xs)
        <> ("but got tuples of length" <+> disp (length a1tyes) <> ":")
        <> nest 2 (dispProductType Outermost a1tyes)
    NonMaybeAnnotForLamOms0 spanInFile a0tye ->
      "The type annotation for an omissible parameter is not Maybe"
        <+> disp spanInFile
        <> hardline
        <> nest 2 (hardline <> stage0Style (disp a0tye))
    NonMaybeAnnotForLamOms1 spanInFile a1tye ->
      "The type annotation for an omissible parameter is not Maybe"
        <+> disp spanInFile
        <> hardline
        <> nest 2 (hardline <> stage0Style (disp a1tye))

instance (Disp sv) => Disp (ConditionalMergeErrorF sv) where
  dispGen _ = \case
    CannotMerge0 pairs ->
      "the following types are incompatible:"
        <> foldl1 (<>) (fmap (\(_a0pat, a0tye) -> nest 2 (hardline <> stage0Style (disp a0tye))) pairs)
    CannotMerge1 pairs ->
      "the following types are incompatible:"
        <> foldl1 (<>) (fmap (\(_a0pat, a1tye) -> nest 2 (hardline <> stage1Style (disp a1tye))) pairs)

instance (Disp sv) => Disp (UnsupportedF sv) where
  dispGen _ = \case
    CannotBindPersistentValue x ->
      "Cannot bind persistent values other than built-in functions:" <+> disp x
    HigherRankPolymorphism a0tye1 atyvar a0tye2 ->
      "Higher-rank polymorphism; we must judge that"
        <+> stage0Style (disp a0tye1)
        <+> "be more general than"
        <+> stage0Style (disp (A0TyForAll atyvar a0tye2))
        <> ", but this has not been supported so far"
    AsWithArguments appCtx ->
      "Function with an as-coercion applied to argument(s); consider let-binding it to a variable"
        <> hardline
        <+> "application context:"
        <> nest 2 (hardline <> disps appCtx)
    LamWithArguments appCtx ->
      "Lambda abstraction directly applied to argument(s); consider using let-expressions"
        <> hardline
        <+> "application context:"
        <> nest 2 (hardline <> disps appCtx)
    LamOmsWithArguments appCtx ->
      "Lambda abstraction for an omissible parameter directly applied to argument(s); consider using let-expressions"
        <> hardline
        <+> "application context:"
        <> nest 2 (hardline <> disps appCtx)
    LamInfWithArguments appCtx ->
      "Lambda abstraction for an inferrable parameter directly applied to argument(s); consider using let-expressions"
        <> hardline
        <+> "application context:"
        <> nest 2 (hardline <> disps appCtx)
    PersistentFunWithOms ->
      "persistent function with an omissible parameter"

instance (Disp sv) => Disp (AppContextEntryF sv) where
  dispGen _ = \case
    AppArg0 Nothing a0e a0tye -> stage0Style (disp a0e) <+> ":" <+> stage0Style (disp a0tye)
    AppArg0 (Just label) a0e a0tye -> "#" <> disp label <+> stage0Style (disp a0e) <+> ":" <+> stage0Style (disp a0tye)
    AppArg1 Nothing a1tye -> stage1Style (disp a1tye)
    AppArg1 (Just label) a1tye -> "#" <> disp label <+> stage1Style (disp a1tye)
    AppArgOmsGiven0 label a0e a0tye -> "?" <> disp label <+> stage0Style (disp a0e) <+> ":" <+> stage0Style (disp a0tye)
    AppArgOmsGiven1 label a1tye -> "?" <> disp label <+> stage1Style (disp a1tye)
    AppArgInfGiven0 a0e a0tye -> "{" <> stage0Style (disp a0e) <+> ":" <+> stage0Style (disp a0tye) <> "}"
    AppArgInfOmitted0 -> "_"
    AppArgInfTypeGiven0 a0tye -> "{type" <+> stage0Style (disp a0tye) <> "}"
    AppArgInfTypeGiven1 a1tye -> "{type" <+> stage1Style (disp a1tye) <> "}"

instance (Disp sv, Disp (af sv)) => Disp (ResultF af sv) where
  dispGen _ = \case
    Pure v -> disp v -- TODO (enhance): add `stage0Style` etc.
    Cast0 _ a0tye r -> "cast0 :" <+> stage0Style (disp a0tye) <> ";" <+> disp r
    Cast1 _ a1tye r -> "cast1 :" <+> stage1Style (disp a1tye) <> ";" <+> disp r
    CastOmsGiven0 _ a0tye r -> "cast-oms-given0 :" <+> stage0Style (disp a0tye) <> ";" <+> disp r
    InsertOmitted0 r -> "insert-omitted0;" <+> disp r
    CastOmsGiven1 _ a1tye r -> "cast-oms-given1 :" <+> stage1Style (disp a1tye) <> ";" <+> disp r
    InsertOmitted1 r -> "insert-omitted1;" <+> disp r
    CastInfGiven0 _ a0tye r -> "cast-inf-given0 :" <+> stage0Style (disp a0tye) <> ";" <+> disp r
    FillInferred0 a0e r -> "fill-inferred0" <+> disp a0e <> ";" <+> disp r
    InsertInferred0 a0e r -> "insert-inferred0" <+> disp a0e <> ";" <+> disp r
    Instantiated0 r -> "instantiated0;" <+> disp r
    InsertInferredType0 sa0tye r -> "insert-inferred-type0" <+> disp sa0tye <> ";" <+> disp r
    Instantiated1 r -> "instantiated1;" <+> disp r
    InsertInferredType1 a1tye r -> "insert-inferred-type1" <+> disp a1tye <> ";" <+> disp r

instance (Disp sv) => Disp (Ass0ValF sv) where
  dispGen req = \case
    A0ValLiteral lit -> disp lit
    A0ValTuple a0vs -> dispTuple a0vs
    A0ValConstructor ctor a0vs -> dispConstructorApp req ctor a0vs
    A0ValLam Nothing (x, a0tyv1) a0v2 _env -> dispNonrecLam req Nothing x a0tyv1 a0v2
    A0ValLam (Just (f, a0tyvRec)) (x, a0tyv1) a0v2 _env -> dispRecLam req f a0tyvRec Nothing x a0tyv1 a0v2
    A0ValBracket a1v1 -> dispBracket a1v1
    A0ValPartialBuiltInApp pba -> dispGen req pba
    A0ValLamType atyvar1 a0e2 _env -> dispLamType req atyvar1 a0e2

instance (Disp v) => Disp (Ass0PartialBuiltInApp v) where
  dispGen req = \case
    A0PartialBuiltInAppArity1 pba1 -> dispGen req pba1
    A0PartialBuiltInAppArity2 pba2 -> dispGen req pba2
    A0PartialBuiltInAppArity3 pba3 -> dispGen req pba3
    A0PartialBuiltInAppArity4 pba4 -> dispGen req pba4
    A0PartialBuiltInAppArity5 pba5 -> dispGen req pba5
    A0PartialBuiltInAppArity6 pba6 -> dispGen req pba6
    A0PartialBuiltInAppArity7 pba7 -> dispGen req pba7

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
    PartialBuiltInAppArity4Nil bi4 -> disp bi4
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
  dispGen req = \case
    PartialBuiltInAppArity6Nil bi6 -> disp bi6
    PartialBuiltInAppArity6Cons pba7 v -> f (disp pba7 <+> dispGen Atomic v)
    where
      f = deepenParenWhen (req <= Atomic)

instance (Disp v) => Disp (Ass0PartialBuiltInAppArity7 v) where
  dispGen _req = \case
    PartialBuiltInAppArity7Nil bi7 -> disp bi7

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
    A1ValLetIn (x, a1tyv0) a1v1 a1v2 ->
      dispLetInWithAnnot req x a1tyv0 a1v1 a1v2
    A1ValLetTupleIn xs a1v1 a1v2 ->
      dispLetTupleIn req xs a1v1 a1v2
    A1ValSequential a1v1 a1v2 ->
      dispSequential req a1v1 a1v2
    A1ValTuple a1vs ->
      dispTuple a1vs
    A1ValConstructor ctor a1vs ->
      dispConstructorApp req ctor a1vs
    A1ValIfThenElse a1v0 a1v1 a1v2 ->
      dispIfThenElse req a1v0 a1v1 a1v2
    A1ValCase a1v0 a1branchVs ->
      dispCase req a1v0 a1branchVs
    A1ValLamType atyvar1 a1v2 ->
      dispLamType req atyvar1 a1v2
    A1ValAppType a1v1 a1tyv2 ->
      dispAppType req a1v1 a1tyv2

instance (Disp sv) => Disp (Ass1BranchValF sv) where
  dispGen _ (A1ValBranch a1pat a1e) = dispBranch a1pat a1e

instance (Disp sv) => Disp (Ass0TypeValF sv) where
  dispGen req = \case
    A0TyValPrim a0tyvPrim Nothing -> dispGen req a0tyvPrim
    A0TyValPrim a0tyvPrim (Just a0vPred) -> dispInternalRefinementType req a0tyvPrim a0vPred
    A0TyValList a0tyv1 Nothing -> dispListType req a0tyv1
    A0TyValList a0tyv1 (Just a0vPred) -> dispInternalRefinementListType req a0tyv1 a0vPred
    A0TyValMaybe a0tyv1 -> dispMaybeType req a0tyv1
    A0TyValProduct a0tyvs ->
      let (a0tyv1, a0tyvsRest) = TwoOrMore.decompose1 a0tyvs
       in dispProduct req a0tyv1 (fmap ("*",) a0tyvsRest)
    A0TyValArrow (xOpt, a0tyv1) a0tye2 -> dispArrowType req Nothing xOpt a0tyv1 a0tye2
    A0TyValCode a1tyv1 -> dispBracket a1tyv1
    A0TyValForAll atyvar sa0tye1 -> dispForAllType req atyvar sa0tye1

instance (Disp sv) => Disp (Ass1TypeValF sv) where
  dispGen req = \case
    A1TyValPrim a1tyvPrim -> dispGen req a1tyvPrim
    A1TyValList a1tyv -> dispListType req a1tyv
    A1TyValMaybe a1tyv -> dispMaybeType req a1tyv
    A1TyValVar atyvar -> disp atyvar
    A1TyValProduct a1tyvs ->
      let (a1tyv1, a1tyvsRest) = TwoOrMore.decompose1 a1tyvs
       in dispProduct req a1tyv1 (fmap ("*",) a1tyvsRest)
    A1TyValArrow labelOpt a1tyv1 a1tyv2 -> dispNondepArrowType req labelOpt a1tyv1 a1tyv2
    A1TyValOmsArrow label a1tyv1 a1tyv2 -> dispOmsArrowType req label (Nothing :: Maybe Text) a1tyv1 a1tyv2
    A1TyValForAll atyvar a1tye2 -> dispForAllType req atyvar a1tye2

instance Disp Ass1PrimTypeVal where
  dispGen req = \case
    A1TyValPrimBase tyPrimBase -> disp tyPrimBase
    A1TyValTensor [n] -> dispNameWithArgs req "Vec" dispPersistent [n]
    A1TyValTensor [m, n] -> dispNameWithArgs req "Mat" dispPersistent [m, n]
    A1TyValTensor ns -> dispNameWithArgs req "Tensor" dispPersistentListLiteral [ns]
    A1TyValDataset datasetParam -> dispNameWithArgs req "Dataset" (dispDatasetParam disp dispListLiteral) [datasetParam]
    A1TyValLstm i h -> dispNameWithArgs req "Lstm" disp [i, h]
    A1TyValTextHelper labels -> dispNameWithArgs req "TextHelper" disp [labels]

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

instance (Disp sv) => Disp (InferableArgLogF sv) where
  dispGen _ = \case
    LogGivenArg spanInFile a0e ->
      "- given" <+> disp spanInFile <> nest 4 (hardline <> stage0Style (disp a0e))
    LogInferredArg spanInFile a0e ->
      "* inferred" <+> disp spanInFile <> nest 4 (hardline <> stage0Style (disp a0e))

instance (Disp sv) => Disp (BugF sv) where
  dispGen _ = \case
    UnboundVarFound x ->
      "Unbound variable" <+> disp x
    UnboundTypeVarFound atyvar ->
      "Unbound type variable" <+> disp atyvar
    NotAClosure a0v ->
      "Not a closure:" <+> disp a0v
    NotATypeClosure a0v ->
      "Not a type closure:" <+> disp a0v
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
    NotABoolean msg a0v ->
      "Not a Boolean:" <+> disp a0v <+> "(" <> disp msg <> ")"
    NotAFloat a0v ->
      "Not a float:" <+> disp a0v
    NotAUnit a0v ->
      "Not a unit:" <+> disp a0v
    NotAString a0v ->
      "Not a string:" <+> disp a0v
    NotATuple a0v ->
      "Not a tuple:" <+> disp a0v
    NotAPair a0v ->
      "Not a pair:" <+> disp a0v
    NotAMaybe a0v ->
      "Not a Maybe:" <+> disp a0v
    TupleLengthMismatch xs a0vs ->
      "Tuple length mismatch:" <+> dispTuple xs <> "," <+> dispTuple a0vs
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
    GeneralBuiltInError msg ->
      "Error raised by a built-in function:" <+> disp msg

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
    Bta.InvalidSyntaxAsExpr spanInFile ->
      "Invalid syntax as expression" <+> disp spanInFile
    Bta.InvalidSyntaxAsTypeExpr spanInFile ->
      "Invalid syntax as type expression" <+> disp spanInFile
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
    Bta.TupleLengthMismatch spanInFile xs bitys ->
      "Tuple length mismatch;" <+> dispTuple xs <+> "and" <+> dispProductType Outermost bitys <+> disp spanInFile
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
    Bta.BITypeInclusionLeft spanInFile bity1 bity2 bitv1 bity2Local ->
      "Basic type contradiction;"
        <+> disp bity1
        <+> "!="
        <+> disp bity2
        <+> disp spanInFile
        <> ";"
        <+> disp bitv1
        <+> "is included in"
        <+> disp bity2Local
    Bta.BITypeInclusionRight spanInFile bity1 bity2 bity1Local bitv2 ->
      "Basic type contradiction;"
        <+> disp bity1
        <+> "!="
        <+> disp bity2
        <+> disp spanInFile
        <> ";"
        <+> disp bity1Local
        <+> "includes"
        <+> disp bitv2
    Bta.UnknownTypeOrInvalidArity spanInFile mods tyName arity ->
      "Unknown type or invalid arguments:" <+> dispQualified mods tyName <> "," <+> disp arity <+> disp spanInFile
    Bta.LetRecParamsCannotStartWithImplicit spanInFile ->
      "Recursive function definitions cannot have an implicit parameter as the first one" <+> disp spanInFile
    Bta.LetRecRequiresNonEmptyParams spanInFile ->
      "Recursive function definitions require at least one parameter" <+> disp spanInFile
    Bta.NoOmissibleParameter spanInFile label ->
      "No omissible parameter expected with label" <+> disp label <+> disp spanInFile

instance Disp Bta.BindingTime where
  dispGen _req = \case
    Bta.BTConst Bta.BT0 -> "0"
    Bta.BTConst Bta.BT1 -> "1"
    Bta.BTVar (Bta.BindingTimeVar n) -> "β" <> disp n

instance Disp Bta.BITypeVar where
  dispGen _req (Bta.BITypeVar j) = "α" <> disp j

instance (Disp bt, Disp tv) => Disp (Bta.BITypeF bt tv) where
  dispGen _req (Bta.BIType bt btMain) =
    dispGen Atomic btMain <> "^" <> dispGen Atomic bt

instance (Disp bt, Disp tv) => Disp (Bta.BITypeMainF bt tv) where
  dispGen req = \case
    Bta.BITyVar bitv ->
      disp bitv
    Bta.BITyBase [] ->
      "●"
    Bta.BITyBase (bt0 : bts) ->
      deepenParenWhen (req <= Atomic) ("●" <+> foldl' (\doc bt -> doc <+> disp bt) (disp bt0) bts)
    Bta.BITyProduct bts ->
      deepenParenWhen (req <= Atomic) (foldl1 appendWithAsterisk (fmap (dispGen Atomic) bts))
    Bta.BITyArrow bt1 bt2 ->
      deepenParenWhen (req <= Atomic) (dispGen Atomic bt1 <+> "->" <+> dispGen Atomic bt2)
    Bta.BITyOmsArrow label bt1 bt2 ->
      deepenParenWhen (req <= Atomic) ("?" <> disp label <+> dispGen Atomic bt1 <+> "->" <+> dispGen Atomic bt2)
    Bta.BITyInfArrow bt1 bt2 ->
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
  dispGen _ (Bta.BExpr (btc, _ann) exprMain) =
    dispWithBindingTime btc exprMain

instance Disp (Bta.BCExprMainF ann) where
  dispGen req = \case
    Bta.BLiteral lit -> disp lit
    Bta.BConstructor (ms, ctor) -> dispLongName ms ctor
    Bta.BVar (ms, x) -> dispLongName ms x
    Bta.BLam Nothing labelOpt (x, tye1) e2 -> dispNonrecLam req labelOpt x tye1 e2
    Bta.BLam (Just (f, tyeRec)) labelOpt (x, tye1) e2 -> dispRecLam req f tyeRec labelOpt x tye1 e2
    Bta.BApp e1 labelOpt e2 -> dispApp req e1 labelOpt e2
    Bta.BLetIn x eBody e2 -> dispLetInWithoutAnnot req x eBody e2
    Bta.BLetTupleIn xs e1 e2 -> dispLetTupleIn req xs e1 e2
    Bta.BLetOpenIn m e -> dispLetOpenIn req m e
    Bta.BSequential e1 e2 -> dispSequential req e1 e2
    Bta.BTuple es -> dispTuple es
    Bta.BIfThenElse e0 e1 e2 -> dispIfThenElse req e0 e1 e2
    Bta.BAs e1 tye2 -> dispAs req e1 tye2
    Bta.BLamOms label (x, tye1) e2 -> dispLamOms req label x tye1 e2
    Bta.BAppOms e1 label e2 -> dispAppOms req e1 label e2
    Bta.BLamInf (x, tye1) e2 -> dispLamInf req x tye1 e2
    Bta.BAppInfGiven e1 e2 -> dispAppInfGiven req e1 e2
    Bta.BAppInfOmitted e1 -> dispAppInfOmitted req e1

instance Disp (Bta.BCTypeExprF ann) where
  dispGen _ (Bta.BTypeExpr (btc, _ann) typeExprMain) =
    dispWithBindingTime btc typeExprMain

instance Disp (Bta.BCTypeExprMainF ann) where
  dispGen req = \case
    Bta.BTyName (_, tyName) args -> dispNameWithArgs req (disp tyName) (dispGen Atomic) args
    Bta.BTyArrow labelOpt (xOpt, tye1) tye2 -> dispArrowType req labelOpt xOpt tye1 tye2
    Bta.BTyOmsArrow label (xOpt, tye1) tye2 -> dispOmsArrowType req label xOpt tye1 tye2
    Bta.BTyInfArrow (x, tye1) tye2 -> dispInfArrowType req x tye1 tye2
    Bta.BTyRefinement x tye1 e2 -> dispRefinementType req x tye1 e2
    Bta.BTyProduct tye1 rest -> dispProduct req tye1 (fmap (first (const "*")) rest)

instance Disp (Bta.BCArgForTypeF ann) where
  dispGen req = \case
    Bta.BExprArg e -> dispGen req e
    Bta.BTypeExprArg tye -> dispGen req tye
