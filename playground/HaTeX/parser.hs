{-# LANGUAGE OverloadedStrings #-}

import Text.LaTeX
import Text.LaTeX.Base.Parser
import Text.LaTeX.Base.Syntax
import Data.Text (unlines)
import qualified Data.Text.IO as T
import Data.Maybe

extractCommandArgs :: String -> LaTeX -> Maybe [TeXArg]
extractCommandArgs s (TeXSeq lt rt) = if isJust lst then lst else rst
  where lst = extractCommandArgs s lt
        rst = extractCommandArgs s rt
extractCommandArgs s (TeXComm name args)
  | name == s = Just args
  | otherwise = Nothing
extractCommandArgs _ _ = Nothing

extractFromArgs :: [TeXArg] -> Text
extractFromArgs ((FixArg (TeXRaw s)):xs) = s

main :: IO ()
main = case parseLaTeX example of
    Left err -> print err
    Right l  -> do
    putStrLn "Printing LaTeX AST..."
    print l
    putStrLn "Checking that (render . parse == id)..."
    let t = render l
    print $ example == t
    putStrLn "Finding date from file"
    print $ (fmap extractFromArgs . extractCommandArgs "date") l
    putStrLn "All done."

example :: Text
example = Data.Text.unlines
          [ "\\documentclass{article}"
          , "\\usepackage[utf8]{inputenc}"
          , "\\author{Daniel Díaz}"
          , "\\date{May 2014}"
          , "\\title{LaTeX parser}"
          , "\\begin{document}"
          , "\\maketitle"
          , "This is an example of how to parse LaTeX using the"
          , "\\HaTeX library."
          , "\\end{document}"
          ]

{--
TeXSeq
  (TeXComm "documentclass" [FixArg (TeXRaw "article")])
  (TeXSeq
   (TeXRaw "\n")
   (TeXSeq
    (TeXComm "usepackage" [OptArg (TeXRaw "utf8"),FixArg (TeXRaw "inputenc")])
    (TeXSeq
     (TeXRaw "\n")
     (TeXSeq
      (TeXComm "author" [FixArg (TeXRaw "Daniel D\237az")])
      (TeXSeq
       (TeXRaw "\n")
       (TeXSeq
        (TeXComm "title" [FixArg (TeXRaw "LaTeX parser")])
        (TeXSeq (TeXRaw "\n") (TeXSeq (TeXEnv "document" [] (TeXSeq (TeXRaw "\n")(TeXSeq (TeXCommS "maketitle") (TeXSeq (TeXRaw "\nThis is an example of how to parse LaTeX using the\n") (TeXSeq (TeXCommS "HaTeX") (TeXRaw " library.\n")))))) (TeXRaw "\n")))))))))  
--}
