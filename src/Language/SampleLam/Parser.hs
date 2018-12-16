module Language.SampleLam.Parser where

import           SampleLam.Prelude

import           Control.Applicative
import           Control.Monad
import qualified Data.HashSet                as HashSet
import           Data.HFunctor.OpenUnion
import           Data.HigherOrder
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
import           Text.Trifecta.Indentation


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

instance DeltaParsing m => TokenParsing (SampleLamParser m) where
  someSpace = SampleLamParser $ buildSomeSpaceParser someSpace haskellCommentStyle
  nesting = SampleLamParser . nesting . runSampleLamParser
  semi = SampleLamParser semi
  highlight h = SampleLamParser . highlight h . runSampleLamParser
  token p = (SampleLamParser . token . runSampleLamParser) p <* Tok.whiteSpace


{-

Lexical Token:

<reserved>   ::= "let" | "in" | "if" | "then" | "else"
<identifier> ::= ([a-z_] [a-zA-Z_']*) - (<reserved>)
<bool>       ::= "True" | "False"
<integer>    ::= ("+"|"-")? [0-9]+ (("e"|"E") ("+"|"-")? [0-9]*)?


Grammar:

<program> ::= <expr>


<expr> ::= <expr7>

<expr7> ::= \ <args> -> <expr>
          | let <decls> in <expr>
          | if <expr> then <expr> else <expr>
          | <expr6>

<args> ::= <var> <args1>

<args1> ::= <var> <args1>
          |

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

<decl> ::= <decllhs> = <expr>

<decllhs> ::= <var> <decllhs1>

<decllhs1> ::= <var> <decllhs1>
             |


<var> ::= <identifier>


<lit> ::= <bool>
        | <integer>

-}

data GrammarUnitsF m r = GrammarUnitsF
  { identifier :: m String
  , reserved   :: String -> m ()
  , bool       :: m Bool
  , integer    :: m Integer

  , program    :: r 'ExprTag
  , expr       :: r 'ExprTag
  , exprs      :: NonEmpty.NonEmpty (r 'ExprTag)
  }


identStyle :: MonadicTokenParsing m => IdentifierStyle m
identStyle = IdentifierStyle
  { _styleName = "SampleLam"
  , _styleStart = lower <|> oneOf "_"
  , _styleLetter = alphaNum <|> oneOf "_'"
  , _styleReserved = HashSet.fromList
    [ "let", "in", "if", "then", "else"
    ]
  , _styleHighlight = Highlight.Identifier
  , _styleReservedHighlight = Highlight.ReservedIdentifier
  }

grammarUnitsF :: MonadicTokenParsing m
  => (forall f. Member AstF f => Compose m (f r) :~> Compose m r)
  -> GrammarUnitsF m r -> GrammarUnitsF m r
grammarUnitsF _ us = GrammarUnitsF {..}
  where
    identifier = Tok.ident identStyle

    reserved   = Tok.reserve identStyle

    bool
      =   string "True" *> pure True
      <|> string "False" *> pure False

    integer = Tok.integer


    program = getField @"expr" us

    expr = NonEmpty.head $ getField @"exprs" us

    exprs = undefined


varF :: MonadicTokenParsing m => GrammarUnitsF m r -> m (VarF r 'VarTag)
varF GrammarUnitsF{..} = VarF <$> identifier

litF :: MonadicTokenParsing m => GrammarUnitsF m r -> m (LitF r 'LitTag)
litF GrammarUnitsF{..}
  =   BoolLitF <$> bool
  <|> IntLitF <$> integer
