import System.Directory
import Data.List.Split

isPDF :: FilePath -> Bool
isPDF xs = (splitOn "." xs) !! 1 == "pdf"

main = do
  inp <- fmap (filter isPDF) (getDirectoryContents "posts")
  print inp
