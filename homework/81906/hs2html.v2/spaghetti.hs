import Prelude
import Control.Monad
import Data.List.Split

data File = File {fileName::String, content::String} deriving (Eq, Show)
data Directory = Directory {dirName::String, subDirectories::[Directory], files::[File]}  deriving (Eq, Show)

addDirectory :: Maybe Directory -> String -> Directory
addDirectory (Just (Directory dirName dirs files)) name = 
    if (getSubDirectory name (Directory dirName dirs files)) == Nothing then
        Directory dirName (dirs ++ [(Directory name [] [])]) files
    else
        (Directory dirName dirs files)

hasFileHelper (File fileName content) targetName = 
    fileName == targetName

hasFile :: [File] -> String -> Maybe File
hasFile [] _ = Nothing
hasFile fileList fileName = 
    if hasFileHelper (head fileList) fileName then
        (Just (head fileList))
    else 
        hasFile (tail fileList) fileName 
    

addFile :: File -> Directory -> Directory
addFile file (Directory dirName dirs files) = 
    if (hasFile files name) == Nothing then
        Directory dirName dirs (files ++ [file])
    else
        Directory dirName dirs files
    where name = (fileName file)

splitPath :: String -> [String]
splitPath path 
    | path !! 0 == '/' = "/":(splitOn "/" (tail path))
    | path !! 0 == '.' && path !! 1 == '/' = (splitOn "/" (drop 2 path))
    | path !! 0 == '.' && path !! 1 == '.' && path !! 2 == '/' = "/":(take ((length (splitOn "/" (drop 3 path))) - 1) (splitOn "/" (drop 3 path)))
    | otherwise = splitOn "/" path


splitInput :: String -> [String]
splitInput input = splitOn " " input

getSubDirectoryHelper :: String -> Directory -> Bool
getSubDirectoryHelper name directory = name == directoryName
   where directoryName = dirName directory

getSubDirectory :: String -> Directory -> Maybe Directory
getSubDirectory name (Directory dirname dirs files) 
    | name == "" = Just (Directory dirname dirs files)
    | null dirs = Nothing
    | getSubDirectoryHelper name (head dirs) = Just (head dirs)
    | otherwise = getSubDirectory name (Directory dirname (tail dirs) files)


getDirectory :: [String] -> Maybe Directory -> Maybe Directory
getDirectory _ Nothing = Nothing
getDirectory [] (Just mainDirectory) = Just mainDirectory
getDirectory pathInList (Just mainDirectory) = 
    getDirectory (tail pathInList) (getSubDirectory (head pathInList) mainDirectory)

cd :: String -> Directory -> Directory -> Maybe Directory
cd path mainDirectory currDirectory =
    let k = (splitPath path) in
        if (head k) == "/" then 
            getDirectory (tail k) (Just mainDirectory)
        else
            getDirectory k (Just currDirectory) 


findFileHelper :: Maybe Directory -> String -> Maybe File
findFileHelper (Just (Directory _ _ files)) fileName = (hasFile files fileName)

findFile :: String -> Directory -> Directory -> Maybe File
findFile path mainDirectory currDirectory =
    let k = (splitPath path) in 
        if (head k) == "/" then 
            (findFileHelper (getDirectory (take ((length k) - 2) (tail k)) (Just mainDirectory)) (head (reverse k)))
        else
            (findFileHelper (getDirectory (take ((length k) - 1) (tail k )) (Just currDirectory)) (head (reverse k)))

concatContent :: String -> Maybe File -> String
concatContent result (Just file) = (result ++ (content file))

inputFileContent :: String -> IO String
inputFileContent result = do
    input <- getLine
    if input == "." then 
        return result
    else 
        inputFileContent (result ++ input ++ "\n")

catGenCurrDir :: String -> String -> Directory -> Directory
catGenCurrDir fileName fileContent currDirectory = if (hasFile (files currDirectory) fileName) /= Nothing then
        (addFile (File fileName fileContent) (rm fileName currDirectory))
    else 
        (addFile (File fileName fileContent) currDirectory)

catInputFileHelper :: [String] -> String -> Directory -> Directory -> String -> IO ()
catInputFileHelper paths path mainDirectory currDirectory result = do
    let fileName = (head paths)-- (head (reverse (splitPath (head (tail paths)))))
    fileContent <- (inputFileContent "")
    let currDir = catGenCurrDir fileName fileContent currDirectory
    putStrLn (show currDir)
    let (Just mainDir) = (changeDirectory (Just mainDirectory) (splitPath path) currDir)
    directoryRecursion (Just mainDir) path (cd path mainDir mainDir)

catFileAssignHelper :: [String] -> String -> Directory -> Directory -> String -> IO ()
catFileAssignHelper paths path mainDirectory currDirectory result = do
    let fileName = (head paths)-- (head (reverse (splitPath path)))
    let currDir = catGenCurrDir fileName result currDirectory-- (addFile (File fileName result) (rm fileName currDirectory))
    let (Just mainDir) = (changeDirectory (Just mainDirectory) (splitPath path) currDir)
    directoryRecursion (Just mainDir) path (cd path mainDir mainDir)

cat :: [String] -> String -> Directory -> Directory -> String -> IO ()
cat paths path mainDirectory currDirectory result
    | ((length paths) == 1) && ((head paths) /= ">") && (result == "") = 
        let (Just file) = (findFile (head paths) mainDirectory currDirectory) in putStrLn (content file)
    | (head paths) == ">" && result /= "" = 
        catFileAssignHelper (tail paths) path mainDirectory currDirectory result
    | (head paths) == ">" && result == "" = 
        catInputFileHelper (tail paths) path mainDirectory currDirectory result
    | otherwise = cat (tail paths) path mainDirectory currDirectory (concatContent result (findFile (head paths) mainDirectory currDirectory))

mkdir :: String -> Directory -> Directory
mkdir name currDirectory = addDirectory (Just currDirectory) name

conCatDirs :: [Directory] -> String -> String
conCatDirs [] result = result
conCatDirs dirs result = conCatDirs (tail dirs) (result ++ (dirName (head dirs)) ++ " ")

conCatFiles :: [File] -> String -> String
conCatFiles [] result = result
conCatFiles files result = conCatFiles (tail files) (result ++ (fileName (head files)) ++ " ")

conCatStringListHelper :: [String] -> String -> String -> String
conCatStringListHelper [] _ result = result
conCatStringListHelper stringList separator result = conCatStringListHelper (tail stringList) separator (result ++ separator ++ (head stringList))

conCatStringList :: [String] -> String -> String
conCatStringList stringList separator = conCatStringListHelper stringList separator ""


ls :: Directory -> IO ()
ls (Directory dirName dirs files) = 
    putStrLn ((conCatDirs dirs "") ++ (conCatFiles files "") )

contentString :: Directory -> String
contentString directory  = "/"

touch name currDirectory = addFile (File name "") currDirectory

rmDirHelper :: String -> [Directory] -> [Directory]
rmDirHelper dirname dirs 
    |null dirs = []
    |dirname == (dirName (head dirs)) = (tail dirs)
    |otherwise = rmDirHelper dirname (tail dirs)

rmdir :: String -> Directory -> Directory
rmdir dirName (Directory name dirs files) = 
    Directory name (rmDirHelper dirName dirs) files
  
rmFileHelper :: String -> [File] -> [File]
rmFileHelper filename files 
    |null files = []
    |filename == (fileName (head files)) = (tail files)
    |otherwise = rmFileHelper filename (tail files)

rm :: String -> Directory -> Directory
rm fileName (Directory name dirs files) = 
    Directory name dirs (rmFileHelper fileName files) 

addDir :: Directory -> Maybe Directory -> Maybe Directory
addDir _ Nothing = Nothing
addDir (Directory dirName dirs file) (Just newDirectory) = 
    Just (Directory dirName (dirs ++ [newDirectory]) file)

changeDirectory :: Maybe Directory -> [String] -> Directory -> Maybe Directory
changeDirectory Nothing _ _ = Nothing 
changeDirectory _ [] changedDirectory = (Just changedDirectory)
changeDirectory (Just mainDirectory) pathInList changedDirectory =
    let k = changeDirectory (cd (head pathInList) mainDirectory mainDirectory) (tail pathInList) changedDirectory in
        if (dirName changedDirectory) == "/" then 
            Just changedDirectory
        else
            addDir (rmdir (head pathInList) mainDirectory) k --  Just changedDirectory

rmMainHelper :: [String] -> Maybe Directory -> String -> Maybe Directory -> IO ()
rmMainHelper inputs (Just mainDirectory) path (Just currDirectory) = do
    let changedDirectory = (rm (head (tail inputs)) currDirectory)
    let pathList = (tail (splitPath path))
    let (Just newMainDirectory) = changeDirectory (Just mainDirectory) pathList changedDirectory
    let newCurrDirectory = (cd path newMainDirectory mainDirectory) 
    directoryRecursion (Just newMainDirectory) path newCurrDirectory
    
mkdirMainHelper :: [String] -> Maybe Directory -> String -> Maybe Directory -> IO ()
mkdirMainHelper inputs (Just mainDirectory) path (Just currDirectory) = do
    let changedDirectory = (mkdir (head (tail inputs)) currDirectory)
    let pathList = (tail (splitPath path))
    let (Just newMainDirectory) = changeDirectory (Just mainDirectory) pathList changedDirectory
    let newCurrDirectory = (cd path newMainDirectory mainDirectory) 
    directoryRecursion (Just newMainDirectory) path newCurrDirectory

touchMainHelper :: [String] -> Maybe Directory -> String -> Maybe Directory -> IO ()
touchMainHelper inputs (Just mainDirectory) path (Just currDirectory) = do
    let changedDirectory = (touch (head (tail inputs)) currDirectory)
    let pathList = (tail (splitPath path))
    let (Just newMainDirectory) = changeDirectory (Just mainDirectory) pathList changedDirectory
    let newCurrDirectory = (cd path newMainDirectory mainDirectory) 
    directoryRecursion (Just mainDirectory) path newCurrDirectory

buildPath :: String -> String -> String
buildPath path next = if path == "/" then
        (path ++ next)
    else
        (path ++ "/" ++ next)

directoryRecursion :: Maybe Directory -> String -> Maybe Directory -> IO ()
directoryRecursion (Just mainDirectory) path (Just currDirectory) = do
    input <- getLine 
    let inputs = splitInput input
    case head inputs of
        "cat" -> cat (tail inputs) path mainDirectory currDirectory "" 
        "pwd" -> putStrLn path
        "rm" -> rmMainHelper inputs (Just mainDirectory) path (Just currDirectory)
        "mkdir" -> mkdirMainHelper inputs (Just mainDirectory) path (Just currDirectory)
        "touch" -> touchMainHelper inputs (Just mainDirectory) path (Just currDirectory)
        "ls" -> ls currDirectory-- same 
        "cd" -> if (cd (head (tail inputs)) mainDirectory currDirectory) == Nothing then
                    (putStrLn "Couldn't find this directory")
                else 
                    let newPath = (buildPath path (head (tail inputs))) in 
                     (directoryRecursion (Just mainDirectory) newPath (cd (head (tail inputs)) mainDirectory currDirectory))
        _ -> putStrLn "Invalid operation!"
    directoryRecursion (Just mainDirectory) path (Just currDirectory)

main :: IO ()
main = do 
    let mainDirectory = Directory "/" [] []
    directoryRecursion (Just mainDirectory) "/" (Just mainDirectory)
    putStrLn (show mainDirectory)
    putStrLn "ended"
    return ()
    
