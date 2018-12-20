module Language.SampleLam.Parser where

import           SampleLam.Prelude

import           Language.SampleLam.Parser.Common
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
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Token           (IdentifierStyle (..),
                                              TokenParsing (..))
import qualified Text.Parser.Token           as Tok
import qualified Text.Parser.Token.Highlight as Highlight
import           Text.Trifecta.Combinators
import           Text.Trifecta.Indentation (IndentationParsing (..))
import qualified Text.Trifecta.Indentation as Trifecta


type MonadicTokenParsing m =
  ( Parsing m
  , CharParsing m
  , TokenParsing m
  , IndentationParsing m
  , DeltaParsing m
  , MonadPlus m
  )


type ParsedAstAnn = Ann
  '[ "sourcePosSpan" ':> Const Span
  ]
type ParsedAstF = AstWithAnnF ParsedAstAnn

attachParsedAnn :: (MonadicTokenParsing m, Member AstF f)
  => Compose m (f r) :~> Compose m (ParsedAstF r)
attachParsedAnn = attachHCofree . hfmap injectU
  where
    attachHCofree = Nat \case
      Compose m -> Compose . attachSpan $ toHCofree <$> m

    toParsedAstAnn sp = annBuild
      $  #sourcePosSpan @= Const sp
      <: nil

    toHCofree x d = HCofreeF (toParsedAstAnn d) x


programParser :: MonadicTokenParsing m => m (HFix ParsedAstF 'ExprTag)
programParser = Tok.whiteSpace *> program grammarUnits <* eof

grammarUnits :: MonadicTokenParsing m => GrammarUnits m (HFix ParsedAstF)
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
<integer>    ::= <natural>
<natural>    ::= octal / hex / decimal natural number


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

data GrammarUnits m r = GrammarUnits
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
  , semidecls :: m [r 'DeclTag]
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
  -> GrammarUnits m r -> GrammarUnits m r
grammarUnitsF inj ~us@GrammarUnits{..} = GrammarUnits
  { identifier = Tok.ident identStyle

  , reserved = Tok.reserve identStyle

  , bool
      =   reserved "True" *> pure True
      <|> reserved "False" *> pure False

  , integer = Tok.natural


  , program = expr

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


  , decls
    =   Tok.symbol "{" *> semidecls <* Tok.symbol "}"
    <|> localIndentation Trifecta.Gt (many $ absoluteIndentation decl)

  , semidecls
    =   (:) <$> decl <*> many (some (Tok.symbol ";") *> decl)
    <|> pure []

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


lamAbsF :: MonadicTokenParsing m => GrammarUnits m r -> m (ExprF r 'ExprTag)
lamAbsF GrammarUnits{..} = Tok.symbol "\\" $> LamAbsF
  <*> args
  <*> (Tok.symbol "->" *> expr)

letF :: MonadicTokenParsing m => GrammarUnits m r -> m (ExprF r 'ExprTag)
letF GrammarUnits{..} = reserved "let" $> LetF
  <*> decls
  <*> (reserved "in" *> expr)

ifF :: MonadicTokenParsing m => GrammarUnits m r -> m (ExprF r 'ExprTag)
ifF GrammarUnits{..} = reserved "if" $> IfF
  <*> expr
  <*> (reserved "then" *> expr)
  <*> (reserved "else" *> expr)

varExprF :: MonadicTokenParsing m => GrammarUnits m r -> m (ExprF r 'ExprTag)
varExprF GrammarUnits{..} = VarExprF <$> var

litExprF :: MonadicTokenParsing m => GrammarUnits m r -> m (ExprF r 'ExprTag)
litExprF GrammarUnits{..} = LitExprF <$> lit


declF :: MonadicTokenParsing m => GrammarUnits m r -> m (DeclF r 'DeclTag)
declF GrammarUnits{..} = DeclF
  <$> var <*> args
  <*> (Tok.symbol "=" *> localIndentation Trifecta.Gt expr)


varF :: MonadicTokenParsing m => GrammarUnits m r -> m (VarF r 'VarTag)
varF GrammarUnits{..} = VarF <$> identifier

varSymF :: MonadicTokenParsing m => GrammarUnits m r -> String -> m (VarF r 'VarTag)
varSymF GrammarUnits{..} s = VarF <$> (Tok.symbol s *> pure s)


boolLitF :: MonadicTokenParsing m => GrammarUnits m r -> m (LitF r 'LitTag)
boolLitF GrammarUnits{..} = BoolLitF <$> bool

intLitF :: MonadicTokenParsing m => GrammarUnits m r -> m (LitF r 'LitTag)
intLitF GrammarUnits{..} = IntLitF <$> integer
