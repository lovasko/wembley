module Latex
( decorate
) where

import qualified Data.ByteString.Char8 as C
import qualified System.FilePath as F

-- | Translate an extension into a  Minted language name. Note that
-- extensions that are not recognized will be rendered without any
-- syntax highlighting.
translateExt :: String -- ^ file extension
             -> String -- ^ Minted language
translateExt ext
  | ext == "c" || ext == "h" = "c"
	| ext == "sh"              = "bash"
	| ext == "hs"              = "haskell"
	| ext == "pl"              = "perl"
	| ext == "py"              = "python"
	| ext == "rb"              = "ruby"
	| otherwise                = "text"

-- | Apply decoration to a single file and its contents.
decorateFile :: String       -- ^ file name
             -> C.ByteString -- ^ file content
						 -> C.ByteString -- ^ decorated file
decorateFile name content =
  C.unlines [ "\\section*{" ++ C.pack name ++ "}"
	          , "\\begin{minted}{" ++ lang ++ "}"
						, content
						, "\\end{minted}"
						, "\n" ]
  where
    lang = C.pack $ translateExt (F.takeExtensions name)

-- | Wrap the document in a standard envelope.
envelope :: C.ByteString -- ^ content
         -> C.ByteString -- ^ finished document
envelope content =
  C.unlines [ "\\documentclass{article}"
            , "\\usepackage{minted}"
            , "\\usepackage{fullpage}"
            , "\\begin{document}"
            , "\\tableofcontents"
            , "\\pagebreak"
						, content
						, "\\end{document}" ]

