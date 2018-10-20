module Main
    ( main
    ) where

import qualified Data.ByteString as BS

import           System.Environment (getArgs)

import           Lucid (renderToFile)

import           Clex.Lex (lexC)
import           Html (cToHtml)

main :: IO ()
main = do
    xs <- getArgs
    if length xs /= 3
    then putStrLn usage
    else do
        let [inFile, outFile, col] = xs
        file <- BS.readFile inFile
        renderToFile outFile $ cToHtml col $ either fail id $ lexC file

usage :: String
usage = " \nUsage: cToHtml inFile.c outFile.html <colour>\n\
        \    Converts inFile.c to a colourised version in which all\n\
        \    keywords, identifiers and literals are coloured in <colour>.\n\
        \    We rely on <colour> working in a style tag.\n\
        \    (we basically set \"color:<colour>\" as a style)\n"
