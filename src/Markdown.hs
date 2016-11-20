{-# LANGUAGE OverloadedStrings #-}

module Markdown
( render
) where

import Data.Char
import Data.Monoid
import qualified Data.ByteString.Char8 as C
import qualified System.FilePath as F


-- | Translate an extension into a GitHub markdown language name. Note
-- that extensions that are not recognized will be rendered without any
-- syntax highlighting.
translateExt :: String       -- ^ file extension
             -> C.ByteString -- ^ Markdown language
translateExt ext
  | ext == "c" || ext == "h" = "c"
  | ext == "sh"              = "sh"
  | ext == "hs"              = "haskell"
  | ext == "pl"              = "perl"
  | ext == "py"              = "python"
  | ext == "rb"              = "ruby"
  | ext == "java"            = "java"
  | otherwise                = ""

-- | Append newline as the last character of a string if it does not end
-- with one.
ensureNewline :: C.ByteString -- ^ old string
              -> C.ByteString -- ^ new string
ensureNewline str
  | C.null str         = str
  | C.last str == '\n' = str
  | otherwise          = str <> "\n"

-- | Apply decoration to a single file and its contents.
decorateFile :: (String, C.ByteString) -- ^ file name & content
             -> C.ByteString           -- ^ decorated file
decorateFile (name, content) = C.unlines
  [ "## " <> C.pack name
  , "```" <> translateExt (tail $ F.takeExtensions name)
  , ensureNewline content <> "```" ]

-- | Generate the "table of contents" section
generateTOC :: [String]     -- ^ file names
            -> C.ByteString -- ^ section content
generateTOC names = C.unlines (header : map (convert . C.pack) names)
  where
    header       = "### Files"
    convert name = "* [" <> name <> "](#" <> linkify name <> ")"
    linkify      = C.map toLower . C.filter isAlpha

-- | Apply decoration to a whole codebase in order to create a document.
render :: String                   -- ^ project name
       -> [(String, C.ByteString)] -- ^ file names & contents
       -> C.ByteString             -- ^ final document
render name entries = C.unlines
  ["# " <> C.pack name
  , generateTOC $ map fst entries
  , C.unlines $ map decorateFile entries ]
