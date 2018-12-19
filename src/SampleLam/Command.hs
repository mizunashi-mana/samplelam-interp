module SampleLam.Command where

import SampleLam.Prelude

import Language.SampleLam.Parser
import Language.SampleLam.Printer
import Data.Text.Prettyprint.Doc.Render.Terminal
import System.Environment


sampleMain :: IO ()
sampleMain = do
  fn:_ <- getArgs
  r <- parseSampleLamFile programParser fn
  let doc = case r of
        Right x  -> prettySyntax x
        Left err -> parseErrDoc err
  putDoc doc
