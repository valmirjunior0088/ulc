import System.Environment (getArgs, getProgName)
import System.Exit (die)
import Ulc (run)

usage :: String -> String
usage name =
  "USAGE: " ++ name ++ " {c|wasm} INPUT OUTPUT"

main :: IO ()
main = do
  arguments <- getArgs

  (mode, input, output) <- case arguments of
    mode : input : output : [] -> return (mode, input, output)
    _ -> getProgName >>= die . usage

  run mode input output
