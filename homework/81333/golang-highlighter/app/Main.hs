module Main where

import qualified Data.Text.IO       as TI
import           System.Environment (getArgs)

import           Lib                (golangToHtml)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStrLn "Usage: ./golangToHtml <file_with_code>"
    _ -> do
      code <- TI.readFile $ args !! 0
      TI.writeFile "source.html" (golangToHtml code)
      putStrLn "Source written."
