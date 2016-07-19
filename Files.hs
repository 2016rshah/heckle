module Files where


exampleTeXPost :: String
exampleTeXPost = unlines
                 [ "\\documentclass{article}"
                 , "\\author{Rushi Shah}"
                 , "\\date{1 January 2015}"
                 , "\\title{Example Post}"
                 , "\\begin{document}"
                 , "\\maketitle"
                 , "This is an example LaTeX/PDF post."
                 , "\\end{document}"
                 ]

exampleMDPost :: String
exampleMDPost = unlines 
                [ "% Example MD Post"
                , "% Rushi Shah"
                , "% 1 January 2016"
                , ""
                , "This is an example MD/HTML post"
                ]
 
exampleBltxFile :: String
exampleBltxFile = unlines
                  [ "<ul id='blog-posts'></ul>"]