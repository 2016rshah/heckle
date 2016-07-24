{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Heckle where

--Stuff for BlazeHTML
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

--Pandoc
import Text.Pandoc.Options
import Text.Pandoc.Readers.Markdown
import Text.Pandoc.Definition
import Text.Pandoc.Writers.HTML
import Text.Pandoc.Shared

--Other stuff I'm using
import Data.List.Split
import Data.Either
import Control.Applicative
import Data.Map.Lazy as Map (lookup)
import Control.Monad

instance Show Html where
  show = renderHtml

--Post {fileName = "example-post", postTitle = "Example Post", postAuthor = "Rushi Shah", postDate = 1 January 2015, 0:0:0, syntaxTree = TeXSeq (TeXComm "documentclass" [FixArg (TeXRaw "article")]) (TeXSeq (TeXRaw "\n") (TeXSeq (TeXComm "author" [FixArg (TeXRaw "Rushi Shah")]) (TeXSeq (TeXRaw "\n") (TeXSeq (TeXComm "date" [FixArg (TeXRaw "1 January 2015")]) (TeXSeq (TeXRaw "\n") (TeXSeq (TeXComm "title" [FixArg (TeXRaw "Example Post")]) (TeXSeq (TeXRaw "\n") (TeXSeq (TeXEnv "document" [] (TeXSeq (TeXRaw "\n") (TeXSeq (TeXCommS "maketitle") (TeXRaw "\nThis is an example LaTeX/PDF post.\n")))) (TeXRaw "\n")))))))))}

showMonth ::  Int -> String
showMonth i = months !! (i-1)
              where months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]

displayDate :: DateTime -> String
displayDate (DateTime y m d h mins s) = show d ++ " " ++ showMonth m ++ " " ++ show y

postsToHtml :: [Post] -> Html
postsToHtml xs = do
  ul ! A.id "blog-posts" $
    forM_ xs postToHtml

--DRY
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
  , syntaxTree :: LaTeX
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

extractFromArgs :: String -> [[TeXArg]] -> Either String Text
extractFromArgs _ (((FixArg (TeXRaw s)):_):_) = Right s
extractFromArgs s [] = Left ("Command not found: " ++ s)
extractFromArgs s _ = Left ("Could not parse arguments passed to " ++ s ++ " command")

getCommandValue :: String -> LaTeX -> Either String String
getCommandValue s = (fmap unpack . extractFromArgs s . lookForCommand s) 

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

createMDPost :: Show a => String -> Either a Pandoc -> Either String Post
createMDPost _ (Left e) = Left (show e)
createMDPost fn (Right pd) = 
  MD <$> pure fn <*> title <*> date <*> pure pd
  where
    date = (getMeta docDate pd) >>= parseAbsoluteDate
    title = getMeta docTitle pd
--Make more DRY
createLaTeXPost :: String -> Either ParseError LaTeX -> Either String Post
createLaTeXPost _ (Left err) = Left (show err) -- Just `show` a ParseError to stick with Strings as error messages
createLaTeXPost s (Right t) = 
  LaTeX <$> pure s <*> title <*> date <*> pure t
  where 
    date = (getCommandValue "date" t) >>= parseAbsoluteDate -- Either monad
    title = getCommandValue "title" t

fileToPost :: String -> IO (Either String Post)
fileToPost fn = 
  case splitOn "." fn of
    [fn, "pdf"] -> do
      latexFile <- fmap (parseLaTeXWith (ParserConf ["verbatim", "minted"]) . pack) (readFile ("posts/"++fn++".tex"))
      return (createLaTeXPost fn latexFile)
    [fn, "md"] -> do
      native <- fmap (readMarkdown def) (readFile ("posts/" ++ fn++".md"))
      return (createMDPost fn native)
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
