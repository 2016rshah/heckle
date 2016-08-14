{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Heckle where

--Stuff for BlazeHTML
import Text.Blaze.Html5 as H hiding (main, map)
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty

--Stuff for TagSoup
import Text.HTML.TagSoup

--Stuff for Dates
import Data.Dates

--Pandoc
import Text.Pandoc.Options (def)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Readers.LaTeX (readLaTeX)
import Text.Pandoc.Definition
import Text.Pandoc.Writers.HTML (writeHtmlString)
import Text.Pandoc.Shared (stringify)

--Other stuff I'm using
import Data.List.Split (splitOn)
import Data.Either
import Control.Applicative
import Control.Monad
import Data.Monoid

instance Show Html where
  show = renderHtml

showMonth ::  Int -> String
showMonth i = months !! (i-1)
              where months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]

displayDate :: DateTime -> String
displayDate (DateTime y m d h mins s) = show d ++ " " ++ showMonth m ++ " " ++ show y

postsToHtml :: [Post] -> Html
postsToHtml xs = do
  ul ! A.id "blog-posts" $
    forM_ xs postToHtml

postToHtml :: Post -> Html
postToHtml p = li ! class_ "blog-post" $ do
        a ! class_ "post-link" ! href (stringValue ("posts/"++fileName p++ext)) $ toHtml (postTitle p)
        H.div ! class_ "post-date" $ toHtml ((displayDate . postDate) p)
        where
          ext = case p of
            (MD _ _ _ _) -> ".html"
            (LaTeX _ _ _ _) -> ".pdf"
        
data Post 
  = LaTeX {
  fileName :: String
  , postTitle :: String
  , postDate :: DateTime
  , pd :: Pandoc
    }
  | MD {
  fileName :: String
  , postTitle :: String
  , postDate :: DateTime
  , pd :: Pandoc
    }
    deriving (Eq, Show)

instance Ord Post where
  compare p1 p2 = compare (postDate p1) (postDate p2)

{-
Relative dates aren't supported by BlaTeX
(it makes no sense for a post to always be written "yesterday", a specific date should be given)
However parsing the date requires the current datetime to be given to parse relative dates
Originally I went through the IO hurdles of getting current datetime
But that introduced unnecessary sideeffects
So this is just a cleaner function to parse absolute dates
(It will give nonsensical results for relative dates: use carefully!)
I also wanted to stick with strings for error messages, so this just shows the ParseErrors from parseDate
-}
parseAbsoluteDate :: String -> Either String DateTime
parseAbsoluteDate s = case parseDate mempty s of
 (Left e) -> Left (show e)
 (Right res) -> (Right res)

getMeta :: (Meta -> [Inline]) -> Pandoc -> Either String String
getMeta f (Pandoc m _) = case f m of
  [] -> Left "Couldn't find it"
  (xs) -> Right (stringify xs)

--Creates a post given a constructor for a post
--The long function in the type signature is just
--A constructor for a post (either `LaTeX` or `MD`)
createPost :: Show a =>
     (String -> String -> DateTime -> Pandoc -> Post)
     -> String -> Either a Pandoc -> Either String Post
createPost _ _ (Left e) = Left (show e)
createPost t fn (Right pd) = 
  t <$> pure fn <*> title <*> date <*> pure pd
  where
    date = (getMeta docDate pd) >>= parseAbsoluteDate
    title = getMeta docTitle pd

fileToPost :: String -> IO (Either String Post)
fileToPost fn = 
  case splitOn "." fn of
    [fn, "pdf"] -> 
      return . createPost LaTeX fn  . readLaTeX def =<< readFile ("posts/" ++ fn ++ ".tex")
    [fn, "md"] -> 
      return . createPost MD fn . readMarkdown def =<< readFile ("posts/" ++ fn ++ ".md")
    _ -> return (Left "Not a LaTeX or MD file")

injectIndex :: String -> Html -> Either String String 
injectIndex layout ul = injectAt [(TagOpen "ul" [("id","blog-posts")]), (TagClose "ul")] layout (show ul)

injectTemplate :: String -> Post -> Either String String 
injectTemplate layout (MD fn _ _ t) = injectAt tags layout inp 
  where 
    tags = [(TagOpen "div" [("id","blog-post")]), (TagClose "div")]
    inp = "<div id='blog-post'>" ++ (writeHtmlString def t) ++ "</div>"

injectAt :: [Text.HTML.TagSoup.Tag String] -> String -> String -> Either String String
injectAt p layout insert = case splitOn p (parseTags layout) of 
  [beg, end] -> Right (renderTags (beg ++ parseTags insert ++ end))
  _ -> Left "Broken layout file"

writeHTML :: String -> Post -> IO ()
writeHTML template p@(MD fn _ _ t) = do
  case injectTemplate template p of
    Right html -> writeFile ("posts/" ++ fn ++ ".html") html 
    _ -> return ()
writeHTML _ (LaTeX _ _ _ _) = return ()
