{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module BlaTeX where

--Stuff for BlazeHTML
import Control.Monad (forM_)

import Text.Blaze.Html5 as H hiding (main, map)
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty

--Stuff for HaTeX
import Text.LaTeX hiding (unlines)
import Text.LaTeX.Base.Parser
import Text.LaTeX.Base.Syntax
import Data.Text (pack, unpack)
import qualified Data.Text.IO as T

--Stuff for TagSoup
import Text.HTML.TagSoup

--Stuff for Dates
import Data.Dates

--Other stuff I'm using
import Data.List.Split
import Data.Either
import Control.Applicative

instance Show Html where
  show = renderHtml

--Post {fileName = "example-post", postTitle = "Example Post", postAuthor = "Rushi Shah", postDate = 1 January 2015, 0:0:0, syntaxTree = TeXSeq (TeXComm "documentclass" [FixArg (TeXRaw "article")]) (TeXSeq (TeXRaw "\n") (TeXSeq (TeXComm "author" [FixArg (TeXRaw "Rushi Shah")]) (TeXSeq (TeXRaw "\n") (TeXSeq (TeXComm "date" [FixArg (TeXRaw "1 January 2015")]) (TeXSeq (TeXRaw "\n") (TeXSeq (TeXComm "title" [FixArg (TeXRaw "Example Post")]) (TeXSeq (TeXRaw "\n") (TeXSeq (TeXEnv "document" [] (TeXSeq (TeXRaw "\n") (TeXSeq (TeXCommS "maketitle") (TeXRaw "\nThis is an example LaTeX/PDF post.\n")))) (TeXRaw "\n")))))))))}

-- | 12 months names.
months :: [String]
months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]

-- | Show name of given month
showMonth ::  Int -> String
showMonth i = months !! (i-1)

displayDate :: DateTime -> String
displayDate (DateTime y m d h mins s) = show d ++ " " ++ showMonth m ++ " " ++ show y

postsToHtml :: [Post] -> Html
postsToHtml xs = do
  ul ! A.id "blog-posts" $
    forM_ xs h
  where
    h s = li ! class_ "blog-post" $ do
            a ! class_ "post-link" ! href (stringValue ("posts/"++fileName s++".pdf")) $ toHtml (postTitle s)
            H.div ! class_ "post-date" $ toHtml ((displayDate . postDate) s)
          
data Post = Post {
  fileName :: String
  , postTitle :: String
  , postAuthor :: String
  , postDate :: DateTime
  , syntaxTree :: LaTeX
    }
    deriving (Eq, Show)

instance Ord Post where
  compare (Post _ _ _ d1 _) (Post _ _ _ d2 _) = compare d1 d2

--instance Show Post where
--  show (Post fn t a d _) = fn ++ " is a post called " ++ t ++ " written by " ++ (a)

getPDF :: FilePath -> Either String String
getPDF xs = if length splitUp == 2 
            then 
              if splitUp !! 1 == "pdf" 
              then Right (splitUp !! 0) 
              else Left "Not a pdf file"
            else Left "Not a file (probably a folder)"
  where splitUp = splitOn "." xs

extractFromArgs :: String -> [[TeXArg]] -> Either String Text
extractFromArgs _ (((FixArg (TeXRaw s)):_):_) = Right s
extractFromArgs s [] = Left ("Command not found: " ++ s)
extractFromArgs s _ = Left ("Could not parse arguments passed to " ++ s ++ " command")

getCommandValue :: String -> LaTeX -> Either String Text
getCommandValue s = (extractFromArgs s . lookForCommand s) 

--Converts either to maybe (for use by maybe applicative)
--eToM :: Either a a -> Maybe a
--eToM e = case e of
--   Left _ -> Nothing
--   Right d -> (Just d)

hackyTypeMagic :: Either String (Either b c) -> Either String c
hackyTypeMagic (Left e) = Left e
hackyTypeMagic (Right (Left _)) = Left "Could not parse date" 
hackyTypeMagic (Right (Right r)) = Right r

createPost :: String -> Either ParseError LaTeX -> DateTime -> Either String Post
createPost _ (Left err) _ = Left (show err) --This is sketchy, I don't like just "show"ing the ParseError
createPost s (Right t)  time = Post <$> pure s <*> title <*> author <*> date <*> pure t
  where 
    date = hackyTypeMagic (fmap (parseDate time) (unpack <$> (getCommandValue "date" t)))
    author = unpack <$> (getCommandValue "author" t)
    title = unpack <$> (getCommandValue "title" t)

fileNameToPost :: String -> IO (Either String Post) 
fileNameToPost fn = do
  latexFile <- fmap (parseLaTeX . pack) (readFile ("posts/"++fn++".tex"))
  t <- getCurrentDateTime
  return (createPost fn latexFile t)

injectPosts :: String -> Html -> Either String String 
injectPosts layout ul = if length splitFile == 2
                        then Right (renderTags (beginning ++ parseTags (show ul) ++ end))
                        else Left "Broken layout file"
  where
    splitFile = splitOn [(TagOpen "ul" [("id","blog-posts")]), (TagClose "ul")] (parseTags layout)
    beginning = splitFile !! 0
    end = splitFile !! 1 --safe indexing?