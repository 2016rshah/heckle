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
      --get all pdf files from directory
      putStrLn "Getting directory contents"
      latexFileNames <- fmap (rights . map getPDF) (getDirectoryContents "posts")
      mdFileNames <- fmap (rights . map getMD) (getDirectoryContents "posts")
      --print brokenFiles
      --print fileNames

      --turn the list of files into a list of posts
      putStrLn "Turning directory contents into posts"
      latexPostsToBeCreated <- (mapM latexToPost latexFileNames)
      mdPostsToBeCreated <- (mapM mdToPost mdFileNames)
      --print mdPostsToBeCreated
      let posts = (reverse . sort . rights) (mdPostsToBeCreated ++ latexPostsToBeCreated)
      --print posts
      --let brokenPosts = lefts postsToBeCreated
      --if length (brokenPosts) > 1 
      --  then print brokenPosts
      --  else putStrLn "All posts are well formed"
      --print posts

      --convert posts to their html if needed
      mapM_ writeHTML posts

      --generate a ul from the list of posts
      putStrLn "Turning posts into an HTML element"
      let generatedHtml = postsToHtml posts
      --print generatedHtml

      --read the layout file
      putStrLn "Reading the layout file"
      layoutFile <- readFile "index.html.hkl"

      --put the ul into the layout file
      putStrLn "Inserting HTML element into layout file"
      let injectedOutput = layoutFile `injectPosts` generatedHtml
      case injectedOutput of
        (Left e) -> putStrLn e
        (Right s) -> do
          --put the results into the index.html file
          putStrLn "Writing resulting file into index.html"
          writeFile "index.html" s
          putStrLn "Success building!"


    ["init"] -> do
      --Create the basic layout file
      writeFile "index.html.hkl" exampleHklFile --Change to layout when testing, index when deploying"
      --Create directory for posts and basic post
      createDirectoryIfMissing True "posts"
      setCurrentDirectory "posts"
      writeFile "example-latex.tex" exampleTeXPost
      writeFile "example-markdown.md" exampleMDPost
      --Compile the LaTeX file into a PDF
      readProcess "pdflatex" ["example-latex.tex"] ""
      print "Success initializing!"
      
    _ -> print "That's not a valid command"
