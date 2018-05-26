import System.Environment (getArgs)

interactWith :: (String -> String)
  -> FilePath -> FilePath -> IO ()
interactWith function inputFile outputFile =
  do
    input <- readFile inputFile
    writeFile outputFile (function input)

main :: IO ()
main = mainWith myFunction
  where
    mainWith function = do
      args <- getArgs
      case args of
        [input, output] -> interactWith function input output
        _ -> putStrLn "error: exactly two arguments needed"

    -- replace id with the name of our function
    myFunction = firstWord


firstWord:: String -> String
firstWord cs = unlines firstWords
  where firstWords = map firstWordOfLine $ lines cs

firstWordOfLine:: String -> String
firstWordOfLine "" = ""
firstWordOfLine cs = head . words $ cs
