--Example File
example :: Text
example = Data.Text.unlines
          [ "\\documentclass{article}"
          , "\\usepackage[utf8]{inputenc}"
          , "\\author{Daniel Diaz}"
          , "\\date{May 2014}"
          , "\\title{LaTeX parser}"
          , "\\begin{document}"
          , "\\maketitle"
          , "This is an example of how to parse LaTeX using the"
          , "\\HaTeX library."
          , "\\end{document}"
          ]
--Abstract Syntax Tree
TeXSeq
  (TeXComm "documentclass" [FixArg (TeXRaw "article")])
  (TeXSeq
   (TeXRaw "\n")
   (TeXSeq
    (TeXComm "usepackage" [OptArg (TeXRaw "utf8"),FixArg (TeXRaw "inputenc")])
    (TeXSeq
     (TeXRaw "\n")
     (TeXSeq
      (TeXComm "author" [FixArg (TeXRaw "Daniel Diaz")])
      (TeXSeq
       (TeXRaw "\n")
       (TeXSeq
        (TeXComm "title" [FixArg (TeXRaw "LaTeX parser")])
        (TeXSeq (TeXRaw "\n") (TeXSeq (TeXEnv "document" [] (TeXSeq (TeXRaw "\n")(TeXSeq (TeXCommS "maketitle") (TeXSeq (TeXRaw "\nThis is an example of how to parse LaTeX using the\n") (TeXSeq (TeXCommS "HaTeX") (TeXRaw " library.\n")))))) (TeXRaw "\n")))))))))  