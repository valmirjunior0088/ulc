import System.Environment (getArgs, getProgName)
import System.Exit (die)
import Ulc (run)

usage :: String -> String
usage name =
  "USAGE: " ++ name ++ " FILE OUTPUT"

main :: IO ()
main = do
  arguments <- getArgs

  (mode, file, output) <- case arguments of
    mode : file : output : [] -> return (mode, file, output)
    _ -> getProgName >>= die . usage

  source <- readFile file

  case run mode source of
    Left message -> die message
    Right result -> writeFile output result
