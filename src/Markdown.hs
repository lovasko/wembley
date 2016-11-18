module Markdown
( decorate
) where

import Data.Monoid
import qualified Data.ByteString.Char8 as C
import qualified System.FilePath as F

-- | Translate an extension into a GitHub markdown language name. Note
-- that extensions that are not recognized will be rendered without any
-- syntax highlighting.
translateExt :: String -- ^ file extension
             -> String -- ^ Markdown language
translateExt ext
  | ext == "c" || ext == "h" = "c"
  | ext == "sh"              = "sh"
  | ext == "hs"              = "haskell"
  | ext == "pl"              = "perl"
  | ext == "py"              = "python"
  | ext == "rb"              = "ruby"
  | otherwise                = ""

-- | Apply decoration to a single file and its contents.
decorateFile :: (String, C.ByteString) -- ^ file name & content
             -> C.ByteString           -- ^ decorated file
decorateFile (name, content) =
  C.unlines [ "## " <> C.pack name
            , "```" <> C.pack $ translateExt (F.takeExtensions name)
            , content
            , "```"
            , "\n" ]

-- | Generate the "table of contents" section
generateTOC :: [String]     -- ^ file names
            -> C.ByteString -- ^ section content
generateTOC files = "### Table of contents" <> map convert files
  where
    convert file = "* [" <> file <> "](" <> linkify file <> ")\n"
    linkify      = map toLower

-- | Apply decoration to a whole codebase in order to create a document.
decorate :: String                   -- ^ project name
         -> [(String, C.ByteString)] -- ^ file names & contents
         -> C.ByteString             -- ^ final document
decorate name =
  C.unlines ["# " <> name
            , generateTOC names
            , C.concat $ map decorateFile files ]

