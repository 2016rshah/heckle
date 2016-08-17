{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Heckle where

import Control.Applicative
import Control.Monad
import Data.Bifunctor
import Data.Either
import Data.Function (on)
import Data.List
import Data.List.Split (splitOn)
import Data.String (IsString)
import Data.Monoid
import System.FilePath

import Text.Blaze.Html5 as H hiding (main, map)
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty

import qualified Text.HTML.TagSoup as TagSoup

import Data.Dates hiding (month)

import Text.Pandoc.Definition hiding (Format)
import Text.Pandoc.Options          (def)
import Text.Pandoc.Readers.LaTeX    (readLaTeX)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Shared           (stringify)
import Text.Pandoc.Writers.HTML     (writeHtmlString)

instance Show Html where
  show = renderHtml

data Month
  = January
  | February
  | March
  | April
  | May
  | June
  | July
  | August
  | September
  | October
  | November
  | December
  deriving (Show, Eq, Bounded, Enum)

month :: Int -> Month
month n = toEnum (n-1)

displayDate :: DateTime -> String
displayDate (DateTime y m d h mins s) =
  intercalate " " [ show d, show (month m), show y ]

postsToHtml :: [Post] -> Html
postsToHtml xs = do
  ul ! A.id "blog-posts" $
    forM_ xs postToHtml

postToHtml :: Post -> Html
postToHtml Post{..} = li ! class_ "blog-post" $ do
  a ! class_ "post-link" ! href (stringValue ("posts/" ++ fileName ++ ext)) $ toHtml postTitle
  H.div ! class_ "post-date" $ toHtml (displayDate postDate)
  where
    ext = getOutputExtension format

--data InputFormat =
data Format = LaTeX | Markdown
  deriving (Show, Eq)

getOutputExtension :: Format -> String
getOutputExtension LaTeX    = ".pdf"
getOutputExtension Markdown = ".html"

-- data FileName = FileName { getFileName :: String, getExtension ::  }
newtype Title = Title { getTitle :: String } deriving (Show, Eq, IsString, ToMarkup)

data Post = Post
  { fileName    :: String  -- TODO make this more typed
  , postTitle   :: Title
  , postDate    :: DateTime
  , format      :: Format
  , pd          :: Pandoc
  }  deriving (Show, Eq)

instance Ord Post where
  compare = compare `on` postDate

-- | Relative dates aren't supported by BlaTeX (it makes no sense for a post to
-- always be written "yesterday", a specific date should be given) However
-- parsing the date requires the current datetime to be given to parse relative
-- dates.
--
-- Originally I went through the IO hurdles of getting current datetime, but
-- that introduced unnecessary side-effects so this is just a cleaner function
-- to parse absolute dates. (It will give nonsensical results for relative
-- dates: use carefully!)
--
-- I also wanted to stick with strings for error messages, so this just shows
-- the ParseErrors from parseDate
parseAbsoluteDate :: String -> Either String DateTime
parseAbsoluteDate = first show . parseDate mempty

getMeta :: (Meta -> [Inline]) -> Pandoc -> Either String String
getMeta f (Pandoc m _) = case f m of
  [] -> Left "Couldn't find it"
  xs -> Right (stringify xs)

-- | Creates a post given a constructor for a post
createPost
  :: Show a
  => Format
  -> String
  -> Either a Pandoc
  -> Either String Post
createPost _ _ (Left e) = Left (show e)
createPost format fileName (Right pd) = do
  postTitle <- Title <$> getMeta docTitle pd
  postDate  <- getMeta docDate pd >>= parseAbsoluteDate
  return Post{..}

fileToPost :: String -> IO (Either String Post)
fileToPost fileName =
  case splitExtension fileName of
    (name, ".pdf") ->
      return . createPost LaTeX name . readLaTeX def =<< readFile ("posts/" <> name <> ".tex")
    (name, ".md") ->
      return . createPost Markdown name . readMarkdown def =<< readFile ("posts/" <> fileName)
    _ -> pure (Left "Not a LaTeX or MD file")

injectIndex :: String -> Html -> Maybe String
injectIndex layout ul = injectAt [ TagSoup.TagOpen "ul" [("id","blog-posts")]
                                 , TagSoup.TagClose "ul"]
                                 layout (show ul)

injectTemplate :: String -> Post -> Maybe String
injectTemplate layout post
  | format post == Markdown = injectAt tags layout inp
  | otherwise = Nothing
  where
    tags = [TagSoup.TagOpen "div" [("id","blog-post")], TagSoup.TagClose "div"]
    inp  = "<div id='blog-post'>" <> writeHtmlString def (pd post) <> "</div>"

injectAt :: [TagSoup.Tag String] -> String -> String -> Maybe String
injectAt p layout insert = case splitOn p (TagSoup.parseTags layout) of
  [beg, end] -> Just $ TagSoup.renderTags (beg <> TagSoup.parseTags insert <> end)
  _          -> Nothing

writeHTML :: String -> Post -> Maybe (IO ())
writeHTML template p@Post{..} =
  writeFile ("posts/" <> fileName <> ".html") <$> injectTemplate template p
