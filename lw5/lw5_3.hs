import System.Environment
import System.Directory
import System.IO
import Data.List
import Data.Char

changePunctuationChars :: Char -> String -> String
changePunctuationChars ch st = map (\c -> if (isPunctuation c) then ch else c) st

copy :: [String] -> IO ()
copy [inputFileName, outputFileName] = do
    contents <- readFile inputFileName
    writeFile outputFileName (filter (\x -> (not (isUpper x))) $ filter (\x -> (not (isPunctuation x))) contents)

remove :: [String] -> IO ()
remove [fileName, numberString] = do
    handle <- openFile fileName ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp"
    contents <- hGetContents handle
    let number = read numberString 
        todoTasks = lines contents  
        newTodoItems = delete (todoTasks !! number) todoTasks
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile fileName
    renameFile tempName fileName

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
    contents <- readFile fileName
    let todoTasks = lines contents
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
    putStr $ unlines numberedTasks

dispatch :: [(String, [String] -> IO ())]
dispatch =  [("add", add),
             ("view", view),
             ("remove", remove)
            ]
main = do
    (command:args) <  getArgs
    let (Just action) = lookup command dispatch
    action args