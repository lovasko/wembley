import Data.List.Split (splitOn)
import qualified System.FilePath as F
import qualified System.FilePath.Find as F
import qualified Data.ByteString.Char8 as C

import Options
import Markdown

-- | Filter predicate to determine relevant file extensions.
acceptExtensions :: Options           -- ^ command-line options
                 -> F.FindClause Bool -- ^ filter predicate
acceptExtensions exts
  | null exts = F.FindClause False
  | otherwise = foldr (F.||?) (F.FindClause False) clauses
  where
    clauses  = map (F.extension F.==?) withDots
    withDots = map ('.' :) $ filter (not . null) $ (splitOn "," exts)

-- | Pretty-printing of a whole codebase into a document.
main :: IO ()
main = do
  options  <- execParser Options.parser
  files    <- F.find F.always (acceptExtensions options) (getRootDir options)
  contents <- mapM C.readFile files
  C.writeFile (getOutputFile options) (Markdown.decorate )

