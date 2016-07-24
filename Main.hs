import System.Directory 
import Data.Either
import Data.List
import System.Environment (getArgs)
import System.Process (readProcess)
import Files
import Heckle

main = do
  args <- getArgs
  case args of
    ["build"] -> do
      
      putStrLn "Reading directory and turning into native posts"
      postsToBeCreated <- mapM fileToPost =<< (getDirectoryContents "posts")
      let posts = (reverse . sort . rights) (postsToBeCreated)

      putStrLn "Writing markdown files into template HTML"
      template <- readFile "template.html.hkl"
      mapM_ (writeHTML template) posts

      putStrLn "Creating HTML <ul> element for index file"
      let generatedHtml = postsToHtml posts

      putStrLn "Inserting HTML <ul> element into layout file"
      layoutFile <- readFile "index.html.hkl"
      let injectedOutput = layoutFile `injectIndex` generatedHtml
      case injectedOutput of
        (Left e) -> putStrLn e
        (Right s) -> do
          writeFile "index.html" s
          putStrLn "Success building!"


    ["init"] -> do
      --Create the basic layout file
      writeFile "index.html.hkl" exampleIndexFile --Change to layout when testing, index when deploying"
      writeFile "template.html.hkl" exampleTemplateFile
      --Create directory for posts and basic post
      createDirectoryIfMissing True "posts"
      setCurrentDirectory "posts"
      writeFile "example-latex.tex" exampleTeXPost
      writeFile "example-markdown.md" exampleMDPost
      --Compile the LaTeX file into a PDF
      --Could do this for every .tex file if wanted to
      readProcess "pdflatex" ["example-latex.tex"] ""
      print "Success initializing!"
      
    _ -> print "That's not a valid command"
