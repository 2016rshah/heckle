{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

--Stuff for BlazeHTML
import Control.Monad (forM_)

import Text.Blaze.Html5 as H hiding (main, map)
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.String

--Stuff for HaTeX
import Text.LaTeX
import Text.LaTeX.Base.Parser
import Text.LaTeX.Base.Syntax
import Data.Text (unlines, pack, unpack)
import qualified Data.Text.IO as T

--Other stuff I'm using
import System.Directory
import Data.List.Split
import Data.Maybe
import Control.Applicative

instance Show Html where
  show = renderHtml

postsToHtml :: [Post] -> Html
postsToHtml xs = do
  ul ! class_ "blog-posts" $
    forM_ xs h
  where
    h s = li ! class_ "blog-post" $
          a ! href (stringValue ("posts/"++fileName s++".pdf")) $
          toHtml (postTitle s)
          
data Post = Post {
  fileName :: String
  , postTitle :: String
  , postAuthor :: String
  , postDate :: String
  , syntaxTree :: LaTeX
    }

instance Show Post where
  show (Post fn t a d _) = t ++ " written by " ++ (a) ++ " on " ++ (d)

getPDF :: FilePath -> Maybe String
--getPDF xs = if splitUp !! 1 == "pdf" then Just (PDF (splitUp !! 0)) else Nothing
getPDF xs = if splitUp !! 1 == "pdf" then Just (splitUp !! 0) else Nothing
  where splitUp = splitOn "." xs

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
createPost s (Right t) = Post <$> pure s <*> title <*> author <*> date <*> pure t
  where 
    date = fmap unpack (getCommandValue "date" t)
    author = fmap unpack (getCommandValue "author" t)
    title = fmap unpack (getCommandValue "title" t)

fileNameToPost :: String -> IO (Maybe Post) 
fileNameToPost fn = do
  latexFile <- fmap (parseLaTeX . pack) (readFile ("posts/"++fn++".tex"))
  return (createPost "post1" latexFile)

main = do
  fileNames <- fmap (catMaybes . map getPDF) (getDirectoryContents "posts")
  posts <- fmap (catMaybes) (mapM fileNameToPost fileNames)
  print posts
  let generatedHtml = postsToHtml posts
  print generatedHtml
  writeFile "index.html" (show generatedHtml)
