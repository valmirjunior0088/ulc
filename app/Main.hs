import System.Environment (getArgs)
import System.Exit (die)
import Ulc (run)

main :: IO ()
main = do
  mode : file : output : [] <- getArgs
  source <- readFile file
  case run mode source of
    Left message -> die message
    Right result -> writeFile output result
