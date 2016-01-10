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

--Stuff for TagSoup
import Text.HTML.TagSoup

--Other stuff I'm using
import System.Directory (getDirectoryContents) 
import Data.List.Split
import Data.Maybe
import Control.Applicative
import System.Environment (getArgs)

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

injectPosts :: String -> Html -> String 
injectPosts layout ul = renderTags (beginning ++ parseTags (show ul) ++ end)
  where
    splitFile = splitOn [(TagOpen "ul" [("id","blog-posts")]), (TagClose "ul")] (parseTags layout)
    beginning = splitFile !! 0
    end = splitFile !! 1

main = do
  args <- getArgs
  case args of
    ["build"] -> do
      --get all pdf files from directory
      putStrLn "Getting directory contents"
      fileNames <- fmap (catMaybes . map getPDF) (getDirectoryContents "posts")

      --turn the list of files into a list of posts
      putStrLn "Turning directory contents into posts"
      posts <- fmap (catMaybes) (mapM fileNameToPost fileNames)

      --generate a ul from the list of posts
      putStrLn "Turning posts into an HTML element"
      let generatedHtml = postsToHtml posts

      --read the layout file
      putStrLn "Reading the layout file"
      layoutFile <- readFile "index.html.bltx"

      --put the ul into the layout file
      putStrLn "Inserting HTML element into layout file"
      let outputFile = layoutFile `injectPosts` generatedHtml

      --put the results into the index.html file
      putStrLn "Writing resulting file into index.html"
      writeFile "index.html" outputFile

      putStrLn "Success!"
    ["init"] -> print "Not implemented yet..."
    _ -> print "That's not a valid command"
