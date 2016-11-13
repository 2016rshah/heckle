module Main where

import Control.Exception
import Data.Either
import Data.List
import Data.Maybe
import Options.Applicative
import System.Directory
import System.FilePath
import System.Environment (getArgs)
import System.Process     (readProcess)

import Heckle
import Files

data Command
  = Build
  | Init

withInfo :: InfoMod a -> Parser a -> ParserInfo a
withInfo im p = info (helper <*> p) im

infixr 1 ==>
(==>) = withInfo . progDesc

mainFlags :: ParserInfo Command
mainFlags = withInfo (fullDesc <> progDesc "heckle: a simple, configurable static site generator") parser
  where
    parser :: Parser Command
    parser = subparser $ mconcat
      [ command "build" $ "Generate site"
           ==> pure Build
      , command "init" $ "Make new site template"
           ==> pure Init
      ]

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

main = do
    command <- execParser mainFlags
    case command of
      Build -> buildSite
      Init  -> initSite
