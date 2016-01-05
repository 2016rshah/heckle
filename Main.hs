{-# LANGUAGE OverloadedStrings #-}

import Text.LaTeX
import Text.LaTeX.Base.Parser
import Text.LaTeX.Base.Syntax
import Data.Text (unlines, pack, unpack)
import qualified Data.Text.IO as T

import System.Directory
import Data.List.Split
import Data.Maybe
import Data.Monoid
import Control.Applicative

type LI = String

type PDF = String -- Is just the name of a pdf enough?
-- data PDF = PDF String
--          deriving (Show, Eq)

data Post = Post {
  fileName :: String
  , author :: String
  , date :: String
  , syntaxTree :: LaTeX
    }

instance Show Post where
  show (Post fn a d _) = fn ++ " written by " ++ (a) ++ " on " ++ (d)

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

getCommandValue :: String -> LaTeX -> Maybe Text
getCommandValue s = (fmap extractFromArgs . extractCommandArgs s) 

createPost :: String -> Either ParseError LaTeX -> Maybe Post
createPost _ (Left err) = Nothing
createPost s (Right t) = Post <$> pure s <*> author <*> date <*> pure t
  where 
    date = fmap unpack (getCommandValue "date" t)
    author = fmap unpack (getCommandValue "author" t)

main = do
  inp <- fmap (catMaybes . map getPDF) (getDirectoryContents "posts")
  print inp
  let lis = (map makeLI inp)
  print lis
  let ul = compileUL lis
  print ul
  writeFile "index.html" ul

  latexFile <- fmap (parseLaTeX . pack) (readFile "posts/post1.tex")
  print $ (createPost "post1" latexFile)
