module Main where

import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T
import qualified Parse.SExpression as SExpr
import System.Environment (getArgs)

-- NOTE: https://www.snoyman.com/blog/2016/12/beware-of-readfile
-- Use ByteString.Lazy if it is a *really* big file.

main :: IO ()
main = do
  (arg : _) <- getArgs
  sourceFile <- BS.readFile arg
  print $ SExpr.parse <$> (SExpr.nest . SExpr.tokenize . T.decodeUtf8 $ sourceFile)
