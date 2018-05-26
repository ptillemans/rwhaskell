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
    myFunction = transposeLinesInText

transposeLinesInText:: String -> String
transposeLinesInText = unlines . transposeLines . lines

transposeLines:: [String] -> [String]
transposeLines [] = []
transposeLines [s] = [s]
transposeLines (a:b:rest) = ts ++ transposeLines rest
  where ts = zipWith (\x y -> x:[y]) a b
