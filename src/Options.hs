module Options
( Format(..)
, Options(..)
, parser
) where

import Data.Monoid
import Options.Applicative

-- | Output formats.
data Format = FmtLatex    -- ^ LaTeX
            | FmtMarkdown -- ^ GitHub Flavoured Markdown

-- | Command-line options.
data Options = Options { getExtensions   :: String
                       , getProjectName  :: String
                       , getOutputFormat :: Format
                       , getOutputFile   :: String
                       , getRootDir      :: String }

-- | Textual representation of the Format type.
instance Show Format where
  show FmtLatex    = "latex"
  show FmtMarkdown = "markdown"

-- | Parse a output format name.
formatReader :: String               -- ^ input
             -> Either String Format -- ^ error message | format
formatReader "latex"    = Right FmtLatex
formatReader "markdown" = Right FmtMarkdown
formatReader _          = Left "Format not supported"

-- | Relevant file extensions options.
optionExtensions :: Parser String -- ^ parser
optionExtensions = strOption
   $ short   'e'
  <> long    "extensions"
  <> value   "hs"
  <> metavar "EXTS"
  <> help    "Comma-separated list of relevant file extensions"
  <> showDefault

-- | Project name option.
optionProjectName :: Parser String -- ^ parser
optionProjectName = strOption
   $ short   'n'
  <> long    "name"
  <> metavar "NAME"
  <> help    "Name of the codebase. Appears in footers and as a title"

-- | Output file location option.
optionOutputFile :: Parser String -- ^ parser
optionOutputFile = strOption
   $ short   'o'
  <> long    "output"
  <> metavar "PATH"
  <> help    "Location of the resulting document"

-- | Output format option.
optionOutputFormat :: Parser Format -- ^ parser
optionOutputFormat = option (eitherReader formatReader)
   $ short   'f'
  <> long    "format"
  <> value   FmtLatex
  <> metavar "FORMAT"
  <> help    "Format of the resulting document, supported: latex, markdown"
  <> showDefault

-- | Root source directory option.
optionRootDir :: Parser String -- ^ parser
optionRootDir = strOption
   $ short   'd'
  <> long    "root-dir"
  <> value   "."
  <> metavar "PATH"
  <> help    "Root source directory path"
  <> showDefault

-- | Command-line user interface.
optionsParser :: Parser Options -- ^ parser
optionsParser = Options
  <$> optionExtensions
  <*> optionProjectName
  <*> optionOutputFormat
  <*> optionOutputFile
  <*> optionRootDir

-- | Description of the utility.
optionsDescription :: InfoMod Options -- ^ parser description
optionsDescription =
     header "wembley - pretty-print a whole codebase into a document"
  <> fullDesc

-- | Parser of the command-line options.
parser :: ParserInfo Options -- ^ parser
parser = info (helper <*> optionsParser) optionsDescription

