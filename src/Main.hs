import Data.List
import Data.List.Split (splitOn)
import Options.Applicative (execParser)
import qualified System.FilePath as F
import qualified System.FilePath.Find as F
import qualified Data.ByteString.Char8 as C

import Latex
import Markdown
import Options


-- | Convert paths to be relative to the project root.
relativePaths :: Options    -- ^ command-line options
              -> [FilePath] -- ^ absolute file paths
              -> [FilePath] -- ^ relative file paths
relativePaths options = map (F.makeRelative (getRootDir options))

-- | Generate the contents of the resuling document.
generateOutput :: Options                  -- ^ command-line options
               -> [(String, C.ByteString)] -- ^ file names & contents
               -> C.ByteString             -- ^ final document
generateOutput options entries = run (getOutputFormat options)
  where
    run FmtLatex    = Latex.render name entries
    run FmtMarkdown = Markdown.render name entries
    name            = getProjectName options

-- | Sort the file names alphabetically and by the number of path
-- components.
sortNames :: [FilePath] -- ^ file names
          -> [FilePath] -- ^ sorted file names
sortNames names = sortBy compSort (sort names)
  where
    compSort a b = compare (compCount a) (compCount b)
    compCount    = length . F.splitPath

-- | Filter predicate to determine relevant file extensions.
acceptExtensions :: Options           -- ^ command-line options
                 -> F.FindClause Bool -- ^ filter predicate
acceptExtensions options
  | null exts = return False
  | otherwise = foldr (F.||?) (return False) clauses
  where
    clauses  = map (F.extension F.==?) withDots
    withDots = map ('.' :) $ filter (not . null) $ (splitOn "," exts)
    exts     = getExtensions options

-- | Find all files that are relevant to the provided options.
findFiles :: Options       -- ^ options
          -> IO [FilePath] -- ^ file names
findFiles options = do
  let recurse = acceptExtensions options
  files <- F.find F.always recurse (getRootDir options)
  return $ sortNames files

-- | Pretty-printing of a whole codebase into a document.
main :: IO ()
main = do
  options  <- execParser Options.parser
  names    <- findFiles options
  contents <- mapM C.readFile names
  let entries = zip (relativePaths options names) contents
  C.writeFile (getOutputFile options) (generateOutput options entries)

