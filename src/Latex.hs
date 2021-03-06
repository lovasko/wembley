{-# LANGUAGE OverloadedStrings #-}

module Latex
( render
) where

import Data.Monoid
import qualified Data.ByteString.Char8 as C
import qualified System.FilePath as F

-- | Translate an extension into a  Minted language name. Note that
-- extensions that are not recognized will be rendered without any
-- syntax highlighting.
translateExt :: String       -- ^ file extension
             -> C.ByteString -- ^ Minted language
translateExt ext
  | ext == "c" || ext == "h" = "c"
  | ext == "sh"              = "bash"
  | ext == "hs"              = "haskell"
  | ext == "pl"              = "perl"
  | ext == "py"              = "python"
  | ext == "rb"              = "ruby"
  | ext == "java"            = "java"
  | ext == "pro"             = "prolog"
  | otherwise                = "text"

-- | Append newline as the last character of a string if it does not end
-- with one.
ensureNewline :: C.ByteString -- ^ old string
              -> C.ByteString -- ^ new string
ensureNewline str
  | C.null str         = str
  | C.last str == '\n' = str
  | otherwise          = str <> "\n"

-- | All underscores must be escaped in order to prevent the subscript
-- redenring.
escapeUnderscore :: String -- ^ old string
                 -> String -- ^ new string
escapeUnderscore [] = []
escapeUnderscore (x:xs)
  | x == '_'  = '\\':x:escapeUnderscore xs
  | otherwise = x:escapeUnderscore xs

-- | Apply decoration to a single file and its contents.
decorateFile :: (String, C.ByteString) -- ^ file name & content
             -> C.ByteString           -- ^ decorated file
decorateFile (name, content) = C.unlines
  [ "\\section*{" <> name' <> "}"
  , "\\addcontentsline{toc}{subsection}{" <> name' <> "}"
  , "\\begin{minted}{" <> translateExt (tail $ F.takeExtensions name) <> "}"
  , ensureNewline content <> "\\end{minted}"
  , "\n" ]
  where name' = C.pack $ escapeUnderscore name

-- | Wrap the document in a standard envelope.
envelope :: String       -- ^ project name
         -> C.ByteString -- ^ content
         -> C.ByteString -- ^ finished document
envelope name content = C.unlines
  [ "\\documentclass{article}"
  , "\\usepackage{minted}"
  , "\\usepackage{fullpage}"
  , "\\begin{document}"
  , "\\centerline{\\bf{\\Huge{" <> name' <> "}}}"
  , "\\bigskip"
  , "\\tableofcontents"
  , "\\bigskip"
  , content
  , "\\end{document}" ]
  where name' = C.pack $ escapeUnderscore name

-- | Apply decoration to a whole codebase in order to create a document.
render :: String                   -- ^ project name
       -> [(String, C.ByteString)] -- ^ file names & contents
       -> C.ByteString             -- ^ final document
render name entries = envelope name (C.unlines $ map decorateFile entries)
