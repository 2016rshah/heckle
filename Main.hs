{-# LANGUAGE OverloadedStrings #-}

import Text.LaTeX
import Text.LaTeX.Base.Parser
import Text.LaTeX.Base.Syntax
import Data.Text (unlines, pack)
import qualified Data.Text.IO as T

import System.Directory
import Data.List.Split
import Data.Maybe
import Data.Monoid

type LI = String

type PDF = String -- Is just the name of a pdf enough?
-- data PDF = PDF String
--          deriving (Show, Eq)

getPDF :: FilePath -> Maybe PDF
--getPDF xs = if splitUp !! 1 == "pdf" then Just (PDF (splitUp !! 0)) else Nothing
getPDF xs = if splitUp !! 1 == "pdf" then Just (splitUp !! 0) else Nothing
  where splitUp = splitOn "." xs

makeLI :: FilePath -> LI
makeLI s = "<li class='blog-post-link'><a href='posts/"++s++".pdf'>"++s++"</a></li>"

compileUL :: [LI] -> String
compileUL xs = "<ul class='blog-posts'>" ++ (mconcat xs) ++ "</ul>"

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

main = do
  inp <- fmap (catMaybes . map getPDF) (getDirectoryContents "posts")
  print inp
  let lis = (map makeLI inp)
  print lis
  let ul = compileUL lis
  print ul
  writeFile "index.html" ul

  latexFile <- fmap pack (readFile "posts/post1.tex")
  case parseLaTeX latexFile of
   Left err -> print err
   Right l -> do
     putStrLn "Finding date from file"
     print $ (fmap extractFromArgs . extractCommandArgs "date") l
     putStrLn "All done."

