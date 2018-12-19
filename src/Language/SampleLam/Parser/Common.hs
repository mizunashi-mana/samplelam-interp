module Language.SampleLam.Parser.Common where

import           SampleLam.Prelude

import           Control.Applicative
import           Control.Monad
import qualified Data.ByteString                            as BS
import qualified Data.ByteString.UTF8                       as BS
import           Data.Functor
import           Data.Semigroup.Reducer                     (Reducer)
import qualified Data.Text.Prettyprint.Convert.AnsiWlPprint as ConvertDoc
import           Data.Text.Prettyprint.Doc                  (Doc, Pretty (..),
                                                             unAnnotate)
import           Data.Text.Prettyprint.Doc.Render.Terminal  (AnsiStyle)
import           Text.Parser.Char
import           Text.Parser.Combinators
import           Text.Parser.Token
import           Text.Parser.Token.Style                    (buildSomeSpaceParser,
                                                             haskellCommentStyle)
import           Text.Trifecta
import           Text.Trifecta.Delta
import           Text.Trifecta.Indentation


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
  token p = coerceSampleLamParser token p <* whiteSpace

initialIndentationState :: IndentationState
initialIndentationState = mkIndentationState 0 infIndentation True Ge


data ParseError = ParseError
  { parseErrDoc    :: Doc AnsiStyle
  , parseErrDeltas :: [Delta]
  } deriving (Show)

instance Pretty ParseError where
  pretty = unAnnotate . parseErrDoc

runSampleLamParser :: Reducer t Rope
  => SampleLamParser Parser a -> IndentationState -> Delta -> t -> Either ParseError a
runSampleLamParser (SampleLamParser p) i d s =
  case runParser (evalIndentationParserT p i) d s of
    Success x -> Right x
    Failure e -> Left $ ParseError
      { parseErrDoc = ConvertDoc.fromAnsiWlPprint $ _errDoc e
      , parseErrDeltas = _errDeltas e
      }

parseSampleLamString :: SampleLamParser Parser a -> String -> Either ParseError a
parseSampleLamString p = runSampleLamParser p initialIndentationState mempty

parseSampleLamFile :: SampleLamParser Parser a -> FilePath -> IO (Either ParseError a)
parseSampleLamFile p fn = BS.readFile fn <&>
  runSampleLamParser p initialIndentationState (Directed (BS.fromString fn) 0 0 0 0)


attachDelta :: DeltaParsing m => m (Delta -> a) -> m a
attachDelta p = p <*> position
