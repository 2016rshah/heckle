module Main where

import Control.Exception
import Data.Either
import Data.List
import Data.Maybe
import System.Directory
--import System.FilePath
import System.Environment (getArgs)
import System.Process     (readProcess)

import Paths_heckle (version)
import Data.Version (showVersion)
import Data.Monoid ((<>))


import Heckle
import Files


buildSite :: IO ()
buildSite = do
  putStrLn "Reading directory and turning into native posts"
  postsToBeCreated <- mapM fileToPost =<< getDirectoryContents "posts"
  let posts = reverse . sort . rights $ postsToBeCreated
  putStrLn $ "Number of posts found: " ++ show (length posts)

  putStrLn "Writing markdown files into template HTML"
  template <- readFile "template.html.hkl"
  sequence $ mapMaybe (writeHTML template) posts

  putStrLn "Creating HTML <ul> element for index file"
  let generatedHtml = postsToHtml posts

  putStrLn "Inserting HTML <ul> element into layout file"
  layoutFile <- readFile "index.html.hkl"
  let injectedOutput = layoutFile `injectIndex` generatedHtml
  case injectedOutput of
    Nothing -> putStrLn "Error with templating"
    Just s  -> do
      writeFile "index.html" s
      putStrLn "Success building!"

initSite :: IO ()
initSite = do

  -- Create the basic layout file
  writeFile "index.html.hkl" exampleIndexFile -- Change to layout when testing, index when deploying"
  writeFile "template.html.hkl" exampleTemplateFile
  writeFile "index.html" exampleResFile

  -- Create directory for posts and basic post
  createDirectoryIfMissing True "posts"
  setCurrentDirectory "posts"
  writeFile "example-markdown.md" exampleMDPost

  -- Compile the LaTeX file into a PDF
  -- Could do this for every .tex file if wanted to
  writeFile "example-latex.tex" exampleTeXPost
  x <- try (readProcess "pdflatex" ["example-latex.tex"] "")
  case x of
    Left e -> do
      let err = show (e :: IOException)
      putStrLn "Warning: LaTeX installation not found, removing LaTeX post"
      removeFile "example-latex.tex"
      return "LaTeX not found"
    Right r -> return "Success"

  putStrLn "Success initializing!"

versionOfSite :: IO ()
versionOfSite = putStrLn ("heckle " ++ showVersion version)

main = do
    --command <- execParser mainFlags
  args <- getArgs
  case args of
      "build":[] -> buildSite
      "init":[] -> initSite
      "version":[] -> versionOfSite
      otherwise -> putStrLn "Command not recognized"
