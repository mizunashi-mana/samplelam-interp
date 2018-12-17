module Language.SampleLam.Parser where

import           SampleLam.Prelude

import           Control.Applicative
import           Control.Monad
import qualified Data.HashSet                as HashSet
import           Data.HFunctor.OpenUnion
import           Data.HigherOrder
import           Data.HFunctor.Cofree
import           Data.Annotation
import qualified Data.List.NonEmpty          as NonEmpty
import           Language.SampleLam.Syntax
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Token           (IdentifierStyle (..),
                                              TokenParsing (..))
import qualified Text.Parser.Token           as Tok
import qualified Text.Parser.Token.Highlight as Highlight
import           Text.Parser.Token.Style     (buildSomeSpaceParser,
                                              haskellCommentStyle)
import           Text.Trifecta.Combinators
import           Text.Trifecta.Delta
import           Text.Trifecta.Indentation   (IndentationParserT,
                                              Token, IndentationParsing)


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
  { runSampleLamParser :: IndentationParserT Token m a
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

{-

Lexical Token:

<reserved>   ::= "let" | "in" | "if" | "then" | "else"
<identifier> ::= ([a-z_] [a-zA-Z_']*) - (<reserved>)
<bool>       ::= "True" | "False"
<integer>    ::= ("+"|"-")? [0-9]+ (("e"|"E") ("+"|"-")? [0-9]*)?


Grammar:

<program> ::= <expr>


<expr> ::= <expr8>

<expr8> ::= \ <arg> <args> -> <expr>
          | let <decls> in <expr>
          | if <expr> then <expr> else <expr>
          | <expr7>

<expr7> ::= <expr6> || <expr7>
          | <expr6>

<expr6> ::= <expr5> && <expr6>
          | <expr5>

<expr5> ::= <expr4> == <expr5>
          | <expr4> /= <expr5>
          | <expr4>

<expr4> ::= <expr3> <= <expr4>
          | <expr3> <  <expr4>
          | <expr3> >= <expr4>
          | <expr3> >  <expr4>
          | <expr3>

<expr3> ::= <expr2> + <expr3>
          | <expr2> - <expr3>
          | <expr2>

<expr2> ::= <expr1> * <expr2>
          | <expr1> / <expr2>
          | <expr1> % <expr2>
          | <expr1>

<expr1> ::= <expr1> <expr0>
          | <expr0>

<expr0> ::= <factor>

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
  , exprs      :: NonEmpty.NonEmpty (m (r 'ExprTag))
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
grammarUnitsF inj us = GrammarUnitsF {..}
  where
    lackInj :: forall f i. Member AstF f => m (f r i) -> m (r i)
    lackInj = coerce (unNat (inj @f) @i)


    identifier = Tok.ident identStyle

    reserved   = Tok.reserve identStyle

    bool
      =   reserved "True" *> pure True
      <|> reserved "False" *> pure False

    integer = Tok.integer


    program = getField @"expr" us

    expr = NonEmpty.head $ getField @"exprs" us

    exprs = undefined

    factor
      =   getField @"evar" us
      <|> lackInj (litExprF us)
      <|> Tok.parens (getField @"expr" us)

    evar = lackInj $ varExprF us


    decls = undefined

    decl = lackInj $ declF us

    args
      =   (:) <$> getField @"arg" us <*> getField @"args" us
      <|> pure []

    arg = getField @"var" us


    var = lackInj $ varF us


    lit = lackInj $ litF us


lamAbsF :: MonadicTokenParsing m => GrammarUnitsF m r -> m (ExprF r 'ExprTag)
lamAbsF GrammarUnitsF{..} = Tok.symbol "\\" $> LamAbsF
  <*> args
  <*> (Tok.symbol "->" *> expr)

appF :: MonadicTokenParsing m => GrammarUnitsF m r -> m (ExprF r 'ExprTag)
appF GrammarUnitsF{..} = AppF <$> expr <*> expr

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


litF :: MonadicTokenParsing m => GrammarUnitsF m r -> m (LitF r 'LitTag)
litF GrammarUnitsF{..}
  =   BoolLitF <$> bool
  <|> IntLitF <$> integer
