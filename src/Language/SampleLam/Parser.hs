module Language.SampleLam.Parser where

import SampleLam.Prelude

import Control.Applicative
import Control.Monad
import qualified Data.HashSet as HashSet
import           Text.Parser.Char
import           Text.Parser.Combinators
import qualified Text.Parser.Token as Tok
import           Text.Parser.Token hiding (reserve, ident)
import qualified Text.Parser.Token.Highlight as Highlight
import           Text.Trifecta.Combinators
import           Text.Trifecta.Delta
import Language.SampleLam.Syntax


type MonadicTokenParsing m =
  ( Parsing m
  , TokenParsing m
  , CharParsing m
  , DeltaParsing m
  , MonadPlus m
  )

identStyle :: MonadicTokenParsing m => IdentifierStyle m
identStyle = IdentifierStyle
  { _styleName = "SampleLam"
  , _styleStart = lower <|> oneOf "_"
  , _styleLetter = alphaNum <|> oneOf "_'"
  , _styleReserved = HashSet.fromList
    [ "let", "in"
    ]
  , _styleHighlight = Highlight.Identifier
  , _styleReservedHighlight = Highlight.ReservedIdentifier
  }

identifier :: MonadicTokenParsing m => m String
identifier = Tok.ident identStyle

reserved :: MonadicTokenParsing m => String -> m ()
reserved = Tok.reserve identStyle

attachDelta :: MonadicTokenParsing m => m (Delta -> a) -> m a
attachDelta p = p <*> position


{-

<program> ::= <expr>


<expr> ::= <expr0>

<expr0> ::= \ <var>_1 ... <var>_n -> <expr>
          | let <decls> in <expr>
          | <expr1>

<factor> ::= <evar>
           | <lit>
           | ( <expr> )

<evar> ::= <var>


<var> ::= <identifier>


<lit> ::= <integer>

-}


varF :: MonadicTokenParsing m => m (VarF r 'VarTag)
varF = VarF <$> identifier

litF :: MonadicTokenParsing m => m (LitF r 'LitTag)
litF = IntLitF <$> integer
