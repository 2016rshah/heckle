{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeSynonymInstances       #-}

module Heckle where

import Control.Applicative
import Control.Monad
import Data.Either
import Data.Function (on)
import Data.List.Split (splitOn)
import Data.String (IsString)
import Data.Monoid
import System.FilePath

import Text.Blaze.Html5 as H hiding (main, map)
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Pretty

import qualified Text.HTML.TagSoup as TagSoup

import Data.Time

import Text.Pandoc.Definition       hiding (Format)
import Text.Pandoc.Options          (def)
import Text.Pandoc.Readers.LaTeX    (readLaTeX)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Shared           (stringify)
import Text.Pandoc.Writers.HTML     (writeHtmlString)

instance Show Html where
  show = renderHtml

displayDate :: UTCTime -> String
displayDate = formatTime defaultTimeLocale "%-d %B %Y"

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

data Format = LaTeX | Markdown
  deriving (Show, Eq)

getOutputExtension :: Format -> String
getOutputExtension LaTeX    = ".pdf"
getOutputExtension Markdown = ".html"

newtype Title = Title { getTitle :: String } 
  deriving (Show, Eq, IsString, ToMarkup)

data Post = Post
  { fileName    :: String  -- TODO make this more typed
  , postTitle   :: Title
  , postDate    :: UTCTime
  , format      :: Format
  , pd          :: Pandoc
  }  
  deriving (Show, Eq)

instance Ord Post where
  compare = compare `on` postDate

parseAbsoluteDate :: String -> Either String UTCTime
parseAbsoluteDate s = case parseAbsoluteDate' s of
  Just a -> Right a
  Nothing -> Left "Date does not match valid formats"


-- | Valid formats:
-- | 6 January 2012
-- | January 6, 2012
-- | 9:47AM 6 January 2012
-- | 9:47AM January 6, 2012
parseAbsoluteDate' :: String -> Maybe UTCTime 
parseAbsoluteDate' s = foldr (<|>) Nothing results  
  where
    results = map ($ s) options
    options = map (parseTimeM True defaultTimeLocale) formats
    formats = ["%-d %B %Y", "%B %-d, %Y", "%-l:%M%p %-d %B %Y", "%-l:%M%p %B %-d, %Y"]

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
