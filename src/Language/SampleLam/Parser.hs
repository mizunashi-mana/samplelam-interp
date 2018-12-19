module Language.SampleLam.Parser where

import           SampleLam.Prelude

import           Control.Applicative
import           Control.Monad
import qualified Data.HashSet                as HashSet
import           Data.HFunctor.OpenUnion
import           Data.HigherOrder
import           Data.HFunctor.Cofree
import           Data.HFunctor.Unsafe
import           Data.Annotation
import           Data.Functor
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Language.SampleLam.Syntax
import           Data.Semigroup.Reducer (Reducer)
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Token           (IdentifierStyle (..),
                                              TokenParsing (..))
import qualified Text.Parser.Token           as Tok
import qualified Text.Parser.Token.Highlight as Highlight
import           Text.Parser.Token.Style     (buildSomeSpaceParser,
                                              haskellCommentStyle)
import qualified Text.Trifecta               as Trifecta
import           Text.Trifecta.Combinators
import           Text.Trifecta.Delta
import           Text.Trifecta.Indentation   (IndentationParserT,
                                              Token, IndentationParsing,
                                              IndentationState)
import qualified Text.Trifecta.Indentation   as Trifecta


type MonadicTokenParsing m =
  ( Parsing m
  , TokenParsing m
  , CharParsing m
  , DeltaParsing m
  , MonadPlus m
  )

attachDelta :: MonadicTokenParsing m => m (Delta -> a) -> m a
attachDelta p = p <*> position


newtype SampleLamParser m a = SampleLamParser
  { getSampleLamParser :: IndentationParserT Token m a
  } deriving
    ( Functor
    , Applicative
    , Monad
    , Alternative
    , MonadPlus
    , Parsing
    , CharParsing
    , DeltaParsing
    , IndentationParsing
    )

coerceSampleLamParser
  :: (IndentationParserT Token m a -> IndentationParserT Token m b)
  -> SampleLamParser m a -> SampleLamParser m b
coerceSampleLamParser = coerce

instance DeltaParsing m => TokenParsing (SampleLamParser m) where
  someSpace = SampleLamParser $ buildSomeSpaceParser someSpace haskellCommentStyle
  nesting = coerceSampleLamParser nesting
  semi = SampleLamParser semi
  highlight h = coerceSampleLamParser $ highlight h
  token p = coerceSampleLamParser token p <* Tok.whiteSpace

emptyIndentationState :: IndentationState
emptyIndentationState = Trifecta.mkIndentationState 0 Trifecta.infIndentation True Trifecta.Ge

runSampleLamParser :: Reducer t Trifecta.Rope
  => SampleLamParser Trifecta.Parser a -> IndentationState -> Delta -> t -> Trifecta.Result a
runSampleLamParser (SampleLamParser p) = Trifecta.runParser . Trifecta.evalIndentationParserT p

parseSampleLamString :: SampleLamParser Trifecta.Parser a -> String -> Trifecta.Result a
parseSampleLamString p = runSampleLamParser p emptyIndentationState mempty


type ParsedAstAnn = Ann
  '[ "sourceDelta" ':> Const Delta
  ]
type ParsedAstF = AstWithAnnF ParsedAstAnn

attachParsedAnn :: (MonadicTokenParsing m, Member AstF f)
  => Compose m (f r) :~> Compose m (ParsedAstF r)
attachParsedAnn = attachHCofree . hfmap injectU
  where
    attachHCofree = Nat \(Compose m) -> Compose . attachDelta $ toHCofree <$> m

    toParsedAstAnn d = annBuild
      $  #sourceDelta @= Const d
      <: nil

    toHCofree x d = HCofreeF (toParsedAstAnn d) x


programParser :: MonadicTokenParsing m => m (HFix ParsedAstF 'ExprTag)
programParser = program grammarUnits

grammarUnits :: MonadicTokenParsing m => GrammarUnitsF m (HFix ParsedAstF)
grammarUnits = fix $ grammarUnitsF (hfmapCoerce . attachParsedAnn)


type ExprUnits m r = m (r 'ExprTag)
type ExprUnitsF m r =
  m (r 'ExprTag) -> ExprUnits m r -> ExprUnits m r

fixExprUnits
  :: m (r 'ExprTag) -> [(ExprUnitsF m r, ExprUnits m r)] -> [ExprUnitsF m r]
  -> [(ExprUnitsF m r, ExprUnits m r)]
fixExprUnits = go
  where
    go _  _  []     = []
    go pe es (f:fs) =
      let
        ce = snd $ head es
      in (f, f pe ce) : go ce (tail es) fs

{-

Lexical Token:

<reserved>   ::= "let" | "in" | "if" | "then" | "else"
<identifier> ::= ([a-z_] [a-zA-Z_']*) - (<reserved>)
<bool>       ::= "True" | "False"
<integer>    ::= [0-9]+ (("e"|"E") ("+"|"-")? [0-9]*)?


Grammar:

<program> ::= <expr>


<expr> ::= <expr7>

<expr7> ::= \ <arg> <args> -> <expr>
          | let <decls> in <expr>
          | if <expr> then <expr> else <expr>
          | <expr6>

<expr6> ::= <expr5> || <expr6>
          | <expr5>

<expr5> ::= <expr4> && <expr5>
          | <expr4>

<expr4> ::= <expr3> == <expr4>
          | <expr3> /= <expr4>
          | <expr3>

<expr3> ::= <expr2> <= <expr3>
          | <expr2> <  <expr3>
          | <expr2> >= <expr3>
          | <expr2> >  <expr3>
          | <expr2>

<expr2> ::= <expr1> + <expr2>
          | <expr1> - <expr2>
          | <expr1>

<expr1> ::= <expr0> * <expr1>
          | <expr0> / <expr1>
          | <expr0> % <expr1>
          | <expr0>

<expr0> ::= <fexpr>

<fexpr> ::= <factor> <fexpr>
          | <factor>

<factor> ::= <evar>
           | <lit>
           | ( <expr> )

<evar> ::= <var>


<decls> ::= { <semidecls> }

<semidecls> ::= <decl> <semidecls1>
              | ; <semidecls>
              |

<semidecls1> ::= ; <semidecls>
               |

<decl> ::= <var> <args> = <expr>

<args> ::= <arg> <args>
         |

<arg> ::= <var>


<var> ::= <identifier>


<lit> ::= <bool>
        | <integer>

-}

data GrammarUnitsF m r = GrammarUnitsF
  { identifier :: m String
  , reserved   :: String -> m ()
  , bool       :: m Bool
  , integer    :: m Integer

  , program    :: m (r 'ExprTag)
  , expr       :: m (r 'ExprTag)
  , exprs      :: [(ExprUnitsF m r, m (r 'ExprTag))]
  , expr0      :: m (r 'ExprTag)
  , fexpr      :: m (NonEmpty (r 'ExprTag))
  , factor     :: m (r 'ExprTag)
  , evar       :: m (r 'ExprTag)

  , decls :: m [r 'DeclTag]
  , decl :: m (r 'DeclTag)
  , args :: m [r 'VarTag]
  , arg :: m (r 'VarTag)

  , var :: m (r 'VarTag)

  , lit :: m (r 'LitTag)
  }

identStyle :: MonadicTokenParsing m => IdentifierStyle m
identStyle = IdentifierStyle
  { _styleName = "SampleLam"
  , _styleStart = lower <|> oneOf "_"
  , _styleLetter = alphaNum <|> oneOf "_'"
  , _styleReserved = HashSet.fromList
    [ "let", "in"
    , "if", "then", "else"
    , "True", "False"
    ]
  , _styleHighlight = Highlight.Identifier
  , _styleReservedHighlight = Highlight.ReservedIdentifier
  }

grammarUnitsF :: forall m r. MonadicTokenParsing m
  => (forall f. Member AstF f => Compose m (f r) :~> Compose m r)
  -> GrammarUnitsF m r -> GrammarUnitsF m r
grammarUnitsF inj ~us@GrammarUnitsF{..} = GrammarUnitsF
  { identifier = Tok.ident identStyle

  , reserved = Tok.reserve identStyle

  , bool
      =   reserved "True" *> pure True
      <|> reserved "False" *> pure False

  , integer = Tok.natural


  , program = expr <* eof

  , expr = NonEmpty.last $ expr0 :| fmap snd exprs

  , expr0 = lackInj $ fexpr <&> \case
    e :| es -> AppF e es

  , exprs = fixExprUnits expr0 exprs $ reverse
      [ \p _ ->
        lackInj
          (   lamAbsF us
          <|> letF us
          <|> ifF us
          )
          <|> p
      , \p c -> do
        x <- p
        lackInj
          (   InfixAppF x <$> lackInj (varSymF us "||") <*> c
          )
          <|> pure x
      , \p c -> do
        x <- p
        lackInj
          (   InfixAppF x <$> lackInj (varSymF us "&&") <*> c
          )
          <|> pure x
      , \p c -> do
        x <- p
        lackInj
          (   InfixAppF x <$> lackInj (varSymF us "==") <*> c
          <|> InfixAppF x <$> lackInj (varSymF us "/=") <*> c
          )
          <|> pure x
      , \p c -> do
        x <- p
        lackInj
          (   InfixAppF x <$> lackInj (varSymF us "<=") <*> c
          <|> InfixAppF x <$> lackInj (varSymF us "<")  <*> c
          <|> InfixAppF x <$> lackInj (varSymF us ">=") <*> c
          <|> InfixAppF x <$> lackInj (varSymF us ">")  <*> c
          )
          <|> pure x
      , \p c -> do
        x <- p
        lackInj
          (   InfixAppF x <$> lackInj (varSymF us "+") <*> c
          <|> InfixAppF x <$> lackInj (varSymF us "-") <*> c
          )
          <|> pure x
      , \p c -> do
        x <- p
        lackInj
          (   InfixAppF x <$> lackInj (varSymF us "*") <*> c
          <|> InfixAppF x <$> lackInj (varSymF us "/") <*> c
          <|> InfixAppF x <$> lackInj (varSymF us "%") <*> c
          )
          <|> pure x
      ]

  , fexpr = do
    x <- factor
    (x :|) <$>
      (   NonEmpty.toList <$> fexpr
      <|> pure []
      )

  , factor
      =   evar
      <|> lackInj (litExprF us)
      <|> Tok.symbol "(" *> expr <* Tok.symbol ")"

  , evar = lackInj $ varExprF us


  , decls = undefined

  , decl = lackInj $ declF us

  , args
      =   (:) <$> arg <*> args
      <|> pure []

  , arg = var


  , var = lackInj $ varF us


  , lit = lackInj
      $   boolLitF us
      <|> intLitF us
  }
  where
    lackInj :: forall f i. Member AstF f => m (f r i) -> m (r i)
    lackInj = coerce $ unNat (inj @f) @i


lamAbsF :: MonadicTokenParsing m => GrammarUnitsF m r -> m (ExprF r 'ExprTag)
lamAbsF GrammarUnitsF{..} = Tok.symbol "\\" $> LamAbsF
  <*> args
  <*> (Tok.symbol "->" *> expr)

letF :: MonadicTokenParsing m => GrammarUnitsF m r -> m (ExprF r 'ExprTag)
letF GrammarUnitsF{..} = reserved "let" $> LetF
  <*> decls
  <*> (reserved "in" *> expr)

ifF :: MonadicTokenParsing m => GrammarUnitsF m r -> m (ExprF r 'ExprTag)
ifF GrammarUnitsF{..} = reserved "if" $> IfF
  <*> expr
  <*> (reserved "then" *> expr)
  <*> (reserved "else" *> expr)

varExprF :: MonadicTokenParsing m => GrammarUnitsF m r -> m (ExprF r 'ExprTag)
varExprF GrammarUnitsF{..} = VarExprF <$> var

litExprF :: MonadicTokenParsing m => GrammarUnitsF m r -> m (ExprF r 'ExprTag)
litExprF GrammarUnitsF{..} = LitExprF <$> lit


declF :: MonadicTokenParsing m => GrammarUnitsF m r -> m (DeclF r 'DeclTag)
declF GrammarUnitsF{..} = DeclF
  <$> var <*> args
  <*> (Tok.symbol "=" *> expr)


varF :: MonadicTokenParsing m => GrammarUnitsF m r -> m (VarF r 'VarTag)
varF GrammarUnitsF{..} = VarF <$> identifier

varSymF :: MonadicTokenParsing m => GrammarUnitsF m r -> String -> m (VarF r 'VarTag)
varSymF GrammarUnitsF{..} s = VarF <$> (Tok.symbol s *> pure s)


boolLitF :: MonadicTokenParsing m => GrammarUnitsF m r -> m (LitF r 'LitTag)
boolLitF GrammarUnitsF{..} = BoolLitF <$> bool

intLitF :: MonadicTokenParsing m => GrammarUnitsF m r -> m (LitF r 'LitTag)
intLitF GrammarUnitsF{..} = IntLitF <$> integer
