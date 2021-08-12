import System.Environment (getArgs)
import Ulc (run)

base :: FilePath -> FilePath
base =
  takeWhile (/= '.')

main :: IO ()
main = do
  file : [] <- getArgs
  source <- readFile file
  case run source of
    Left message -> do
      putStrLn "error:"
      putStrLn message
    Right result -> do
      putStrLn result
      writeFile (base file ++ ".c") result
