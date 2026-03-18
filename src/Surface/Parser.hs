module Surface.Parser
  ( parseExpr,
    parseTypeExpr,
  )
where

import Common.FrontError (FrontError (..))
import Common.LocationInFile (SourceSpec)
import Common.ParserUtil
import Common.TokenUtil (Located (..), Span, mergeSpan)
import Control.Lens ((^?))
import Data.Either.Extra
import Data.Functor
import Data.Generics.Labels ()
import Data.List.Extra qualified as List
import Data.List.NonEmpty (NonEmpty (..), nonEmpty)
import Data.List.NonEmpty qualified as NonEmpty
import Data.List.TwoOrMore qualified as TwoOrMore
import Data.Text (Text)
import Surface.Syntax
import Surface.Token (Token (..))
import Surface.Token qualified as Token
import Prelude hiding (or)

type P a = GenP Token a

parenGen :: Token -> Token -> P a -> P (Located a)
parenGen tokLeft tokRight p =
  make <$> token tokLeft <*> p <*> token tokRight
  where
    make locLeft v locRight = Located (mergeSpan locLeft locRight) v

paren :: P a -> P (Located a)
paren = parenGen TokLeftParen TokRightParen

brace :: P a -> P (Located a)
brace = parenGen TokLeftBrace TokRightBrace

lower :: P (Located Text)
lower = expectToken (^? #_TokLower)

upper :: P (Located Text)
upper = expectToken (^? #_TokUpper)

label :: P (Located Text)
label = expectToken (^? #_TokLabel)

longOrShortLower :: P (Located ([Text], Text))
longOrShortLower =
  expectToken (^? #_TokLongLower)
    <|> (fmap ([],) <$> lower)

standaloneOp :: P (Located Text)
standaloneOp = paren (noLoc operator)

boundIdent :: P (Located Text)
boundIdent = lower <|> standaloneOp

int :: P (Located Int)
int = expectToken (^? #_TokInt)

float :: P (Located Double)
float = expectToken (^? #_TokFloat)

string :: P (Located Text)
string = expectToken (^? #_TokString)

list :: P a -> P (Located [a])
list = genVec TokLeftSquare TokRightSquare TokComma

vec :: P (Located [Int])
vec = genVec TokVecLeft TokVecRight TokSemicolon (noLoc int)

mat :: P (Located [[Int]])
mat = genMat TokMatLeft TokMatRight TokSemicolon TokComma (noLoc int)

operator :: P (Located Var)
operator = orOp <|> andOp <|> compOp <|> addOp <|> multOp <|> consOp

consOp :: P (Located Var)
consOp = fmap (const "::") <$> expectToken (^? #_TokColonColon)

-- | Parses multiplicative operators, including the exact `"*"`.
multOp :: P (Located Var)
multOp =
  (fmap (const "*") <$> expectToken (^? #_TokProd))
    <|> expectToken (^? #_TokOpMult)

addOp :: P (Located Var)
addOp = expectToken (^? #_TokOpAdd)

compOp :: P (Located Var)
compOp = expectToken (^? #_TokOpComp)

andOp :: P (Located Var)
andOp = expectToken (^? #_TokOpAnd)

orOp :: P (Located Var)
orOp = expectToken (^? #_TokOpOr)

makeBinOpApp :: Expr -> Located Var -> Expr -> Expr
makeBinOpApp e1@(Expr loc1 _) (Located locBinOp binOp) e2@(Expr loc2 _) =
  Expr (mergeSpan locLeft loc2) (App (Expr locLeft (App eOp Nothing e1)) Nothing e2)
  where
    locLeft = mergeSpan loc1 locBinOp
    eOp = Expr locBinOp (Var ([], binOp))

data FunArg
  = FunArgMandatory (Maybe (Located Text)) Expr
  | FunArgImpGiven (Located Expr)
  | FunArgImpOmitted Span

data DomainSpec
  = DomMandatory (Maybe (Located Text)) (Maybe (Span, Var), TypeExpr)
  | DomImplicit ((Span, Var), TypeExpr)

expr :: P Expr
expr = letin
  where
    atom :: P Expr
    atom =
      (located (Literal . LitInt) <$> int)
        <|> (located (Literal . LitFloat) <$> float)
        <|> (located (Literal . LitString) <$> string)
        <|> (located (Literal . LitList) <$> list expr)
        <|> (located (Literal . LitVec) <$> vec)
        <|> (located (Literal . LitMat) <$> mat)
        <|> (makeBool True <$> token TokTrue)
        <|> (makeBool False <$> token TokFalse)
        <|> (located Var <$> longOrShortLower)
        <|> (makeConstructor <$> upper)
        <|> try (located (\x -> Var ([], x)) <$> standaloneOp)
        <|> try (makeLitUnit <$> token TokLeftParen <*> token TokRightParen)
        <|> (makeEnclosed <$> paren ((,) <$> expr <*> many (token TokComma *> expr)))
        <|> (makeRefinement <$> try (brace ((,,) <$> (noLoc boundIdent <* token TokColon) <*> (typeExpr <* token TokBar) <*> expr)))
      where
        located constructor (Located loc e) = Expr loc (constructor e)
        makeLitUnit loc1 loc2 = Expr (mergeSpan loc1 loc2) (Literal LitUnit)
        makeEnclosed (Located loc (e1@(Expr _ eMain), esRest')) =
          Expr loc $
            case nonEmpty esRest' of
              Nothing -> eMain
              Just esRest -> Tuple (TwoOrMore.make1 e1 esRest)
        makeBool b loc = Expr loc (Literal (LitBool b))
        makeConstructor (Located loc t) = Expr loc (Constructor ([], t))
        makeRefinement (Located loc (x, tye, e)) = Expr loc (TyRefinement x tye e)

    app :: P Expr
    app =
      some arg >>= makeApp
      where
        arg :: P FunArg
        arg =
          (FunArgImpOmitted <$> token TokUnderscore)
            <|> (FunArgImpGiven <$> try (brace expr))
            <|> (FunArgMandatory . Just <$> label <*> atom)
            <|> (FunArgMandatory Nothing <$> atom)

        makeApp :: NonEmpty FunArg -> P Expr
        makeApp (FunArgMandatory Nothing eFun :| args) = pure $ List.foldl' makeAppSingle eFun args
        makeApp (FunArgMandatory (Just (Located loc lab)) _ :| _) = failure (Located loc (TokLabel lab))
        makeApp (FunArgImpGiven (Located loc _e) :| _) = failure (Located loc TokLeftBrace)
        makeApp (FunArgImpOmitted loc :| _) = failure (Located loc TokUnderscore)

        makeAppSingle :: Expr -> FunArg -> Expr
        makeAppSingle e1@(Expr loc1 _) = \case
          FunArgMandatory Nothing e2@(Expr loc2 _) -> Expr (mergeSpan loc1 loc2) (App e1 Nothing e2)
          FunArgMandatory (Just (Located _ l)) e2@(Expr loc2 _) -> Expr (mergeSpan loc1 loc2) (App e1 (Just l) e2)
          FunArgImpGiven (Located loc2 e2) -> Expr (mergeSpan loc1 loc2) (AppImpGiven e1 e2)
          FunArgImpOmitted loc2 -> Expr (mergeSpan loc1 loc2) (AppImpOmitted e1)

    as :: P Expr
    as =
      makeAs <$> app <*> optional (token TokAs *> app)
      where
        makeAs :: Expr -> Maybe TypeExpr -> Expr
        makeAs e1@(Expr loc1 _) = \case
          Nothing -> e1
          Just tye2@(Expr loc2 _) -> Expr (mergeSpan loc1 loc2) (As e1 tye2)

    con :: P Expr
    con = binSep makeBinOpApp consOp as

    mult :: P Expr
    mult =
      makeProduct <$> con <*> many ((\(Located locOp op) e -> ((locOp, op), e)) <$> multOp <*> con)
      where
        makeProduct :: Expr -> [((Span, Var), Expr)] -> Expr
        makeProduct e1@(Expr locFirst _) rest' =
          case nonEmpty rest' of
            Nothing ->
              e1
            Just rest ->
              let (_, Expr locLast _) = NonEmpty.last rest
               in Expr (mergeSpan locFirst locLast) (Product e1 rest)

    add :: P Expr
    add = binSep makeBinOpApp addOp mult

    comp :: P Expr
    comp = binSep makeBinOpApp compOp add

    ands :: P Expr
    ands = binSep makeBinOpApp andOp comp

    ors :: P Expr
    ors = binSep makeBinOpApp orOp ands

    flipApp :: P Expr
    flipApp = makeFlipApp <$> ors <*> many (token TokOpFlipApp *> ors)
      where
        makeFlipApp =
          List.foldl'
            ( \eArg@(Expr locArg _) eFun@(Expr locFun _) ->
                Expr (mergeSpan locArg locFun) (App eFun Nothing eArg)
            )

    arrow :: P TypeExpr
    arrow =
      try (makeTyArrow <$> arrowDom <*> (token TokArrow *> arrow))
        <|> flipApp
      where
        makeTyArrow domSpec tye2@(Expr loc2 _) =
          case domSpec of
            DomMandatory locLabelOpt (varOpt, tye1@(Expr locTye1 _)) ->
              let loc1 =
                    case locLabelOpt of
                      Just (Located locLabel _) ->
                        locLabel
                      Nothing ->
                        case varOpt of
                          Just (locDom, _) -> locDom
                          Nothing -> locTye1
                  xOpt = fmap snd varOpt
                  labelOpt = fmap (\(Located _ l) -> l) locLabelOpt
               in Expr (mergeSpan loc1 loc2) (TyArrow labelOpt (xOpt, tye1) tye2)
            DomImplicit ((loc1, x), tye1) ->
              Expr (mergeSpan loc1 loc2) (TyImpArrow (x, tye1) tye2)

    arrowDom :: P DomainSpec
    arrowDom =
      (DomMandatory . Just <$> label <*> mandatoryArrowDom)
        <|> (DomMandatory Nothing <$> mandatoryArrowDom)
        <|> (DomImplicit <$> implicitArrowDom)
      where
        mandatoryArrowDom :: P (Maybe (Span, Var), TypeExpr)
        mandatoryArrowDom =
          try (makeArrowDom <$> paren ((,) <$> (noLoc lower <* token TokColon) <*> typeExpr))
            <|> ((Nothing,) <$> flipApp)
          where
            makeArrowDom (Located loc (x, tyeDom)) = (Just (loc, x), tyeDom)

        implicitArrowDom :: P ((Span, Var), TypeExpr)
        implicitArrowDom =
          makeArrowDom <$> brace ((,) <$> (noLoc lower <* token TokColon) <*> typeExpr)
          where
            makeArrowDom (Located loc (x, tyeDom)) = ((loc, x), tyeDom)

    lam :: P Expr
    lam =
      (makeNonrecLam <$> token TokFun <*> (lamBinder <* token TokArrow) <*> expr)
        <|> (makeRecLam <$> token TokRec <*> (mandatoryBinder <* token TokArrow <* token TokFun) <*> (mandatoryBinder <* token TokArrow) <*> expr)
        <|> (makeIf <$> token TokIf <*> expr <*> (token TokThen *> expr) <*> (token TokElse *> expr))
        <|> arrow
      where
        makeNonrecLam locFirst xBinder' e@(Expr locLast _) =
          Expr (mergeSpan locFirst locLast) $
            case xBinder' of
              MandatoryBinder labelOpt xBinder -> Lam Nothing labelOpt xBinder e
              ImplicitBinder xBinder -> LamImp xBinder e

        makeRecLam locFirst fBinder xBinder e@(Expr locLast _) =
          Expr (mergeSpan locFirst locLast) (Lam (Just fBinder) Nothing xBinder e)

        makeIf locFirst e0 e1 e2@(Expr locLast _) =
          Expr (mergeSpan locFirst locLast) (IfThenElse e0 e1 e2)

    lamBinder :: P LamBinder
    lamBinder =
      (MandatoryBinder . Just <$> noLoc label <*> mandatoryBinder)
        <|> (MandatoryBinder Nothing <$> mandatoryBinder)
        <|> (ImplicitBinder <$> implicitBinder)

    mandatoryBinder, implicitBinder :: P (Var, TypeExpr)
    mandatoryBinder = noLoc (paren ((,) <$> noLoc lower <*> (token TokColon *> typeExpr)))
    implicitBinder = noLoc (brace ((,) <$> noLoc lower <*> (token TokColon *> typeExpr)))

    letin :: P Expr
    letin =
      (makeLet <$> token TokLet <*> letInMain)
        <|> (makeSequential <$> lam <*> optional (token TokSemicolon *> letin))
      where
        makeLet locFirst (eMain, locLast) =
          Expr (mergeSpan locFirst locLast) eMain

        makeSequential e1@(Expr loc1 _) = \case
          Nothing -> e1
          Just e2@(Expr loc2 _) -> Expr (mergeSpan loc1 loc2) (Sequential e1 e2)

    letInMain :: P (ExprMain, Span)
    letInMain =
      try (makeLetTupleIn <$> paren ((,) <$> noLoc boundIdent <*> some (token TokComma *> noLoc boundIdent)) <*> (token TokEqual *> expr) <*> (token TokIn *> expr))
        <|> (makeLetIn <$> noLoc boundIdent <*> many lamBinder <*> optional (token TokColon *> typeExpr) <*> (token TokEqual *> expr) <*> (token TokIn *> expr))
        <|> (makeLetRecIn <$> (token TokRec *> noLoc boundIdent) <*> many lamBinder <*> (token TokColon *> typeExpr) <*> (token TokEqual *> expr) <*> (token TokIn *> expr))
        <|> (makeLetOpenIn <$> (token TokOpen *> noLoc upper) <*> (token TokIn *> expr))
      where
        makeLetTupleIn (Located _ (x1, xsRest)) e1 e2@(Expr locLast _) = (LetTupleIn (TwoOrMore.make1 x1 xsRest) e1 e2, locLast)
        makeLetIn x params tyeBodyOpt e1 e2@(Expr locLast _) = (LetIn x params tyeBodyOpt e1 e2, locLast)
        makeLetRecIn x params tye e1 e2@(Expr locLast _) = (LetRecIn x params tye e1 e2, locLast)
        makeLetOpenIn m e@(Expr locLast _) = (LetOpenIn m e, locLast)

typeExpr :: P TypeExpr
typeExpr = expr

parse :: P a -> SourceSpec -> Text -> Either FrontError a
parse p sourceSpec source = do
  locatedTokens <- mapLeft FrontLexingError $ Token.lex source
  mapLeft FrontParseError $ runParser p sourceSpec locatedTokens

parseExpr :: SourceSpec -> Text -> Either FrontError Expr
parseExpr = parse (expr <* eof)

parseTypeExpr :: SourceSpec -> Text -> Either FrontError TypeExpr
parseTypeExpr = parse (typeExpr <* eof)
