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

main = do
  inp <- fmap (catMaybes . map getPDF) (getDirectoryContents "posts")
  print inp
  let lis = (map makeLI inp)
  print lis
  let ul = compileUL lis
  print ul
  writeFile "index.html" ul
