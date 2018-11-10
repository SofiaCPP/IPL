module Main 
    ( main
    ) where

import qualified Data.ByteString as BS

import           System.Environment

import           Brainfsck.Eval  (eval)
import           Brainfsck.Parse (parse)

main :: IO ()
main = do
    xs <- getArgs
    if length xs /= 1
    then putStrLn usage
    else either fail eval . parse =<< BS.readFile (head xs)

usage :: String
usage = "To run the interpreter, start brainfsck with one argument - the path to a brainfuck script.\
         \> stack run brainfsck -- asdf.bf"
