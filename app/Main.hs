import System.Environment (getArgs)
import Ulc (run)

main :: IO ()
main = do
  mode : file : [] <- getArgs
  source <- readFile file
  case run mode source of
    Left message -> putStr message
    Right result -> putStr result
