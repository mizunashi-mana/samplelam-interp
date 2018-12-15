module Language.SampleLam.Parser where

import           SampleLam.Prelude

import           Control.Applicative
import           Control.Monad
import qualified Data.HashSet                as HashSet
import           Language.SampleLam.Syntax
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Token           (IdentifierStyle (..),
                                              TokenParsing)
import qualified Text.Parser.Token           as Tok
import qualified Text.Parser.Token.Highlight as Highlight
import           Text.Trifecta.Combinators
import           Text.Trifecta.Delta


type MonadicTokenParsing m =
  ( Parsing m
  , TokenParsing m
  , CharParsing m
  , DeltaParsing m
  , MonadPlus m
  )

attachDelta :: MonadicTokenParsing m => m (Delta -> a) -> m a
attachDelta p = p <*> position


{-

Lexical Token:

<reserved>   ::= "let" | "in" | "if" | "then" | "else"
<identifier> ::= ([a-z_] [a-zA-Z_']*) - (<reserved>)
<bool>       ::= "True" | "False"
<integer>    ::= ("+"|"-")? [0-9]+ (("e"|"E") ("+"|"-")? [0-9]*)?


Grammar:

<program> ::= <expr>


<expr> ::= <expr0>

<expr0> ::= \ <var>_1 ... <var>_n -> <expr>
          | let <decls> in <expr>
          | if <expr> then <expr> else <expr>
          | <expr1>

<expr1> ::= <expr2> + <expr1>
          | <expr2> - <expr1>
          | <expr2>

<expr2> ::= <expr3> * <expr2>
          | <expr3> / <expr2>
          | <expr3>

<expr3> ::= <expr4> % <expr3>
          | <expr4>

<expr4> ::= <factor>

<factor> ::= <evar>
           | <lit>
           | ( <expr> )

<evar> ::= <var>


<decls> ::= { <semidecls> }

<semidecls> ::= <decl> <semidecls1>
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

data AstParsingR m r = AstParsingR
  { identifier :: m String
  , reserved   :: String -> m ()
  , bool       :: m Bool
  , integer    :: m Integer

  , program    :: m (ExprF r 'ExprTag)
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

astParsingR :: MonadicTokenParsing m => AstParsingR m r
astParsingR = AstParsingR {..}
  where
    identifier = Tok.ident identStyle

    reserved   = Tok.reserve identStyle

    bool
      =   string "True" *> pure True
      <|> string "False" *> pure False

    integer = Tok.integer

    program = empty


varF :: MonadicTokenParsing m => AstParsingR m r -> m (VarF r 'VarTag)
varF AstParsingR{..} = VarF <$> identifier

litF :: MonadicTokenParsing m => AstParsingR m r -> m (LitF r 'LitTag)
litF AstParsingR{..}
  =   BoolLitF <$> bool
  <|> IntLitF <$> integer
