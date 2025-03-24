module HelperFunctions where
    import System.IO
    import Data.Text (toLower, pack, unpack)
    import Data.List
    import FileSystem
    import Data.List (intercalate, words)
    import System.Directory
    import System.FilePath.Posix
    
    --converts given path to the needed list of strings        
    getWordsList :: String -> [String] ->[String]
    getWordsList "" result = helper result
        where
            helper :: [String] -> [String]
            helper lst = do
                let new = reverse lst
                let h = head new
                if ((head h) == '/')
                    then ["/"] ++ [tail (head new)] ++ tail new
                else new        
    getWordsList str [] = getWordsList (tail str) [[head str]]
    getWordsList str result@(x:xs) = do
        let h = head str
        let t = tail str
        if (h == '/')
            then getWordsList t ([""] ++ result)
        else getWordsList t ([(x ++ [h])] ++ xs)

    --used to separate args in cat
    separate :: [[String]] -> [[String]] -> ([[String]], [[String]])
    separate [] forcat = (forcat, [])
    separate (x:xs) forcat = do
        if (x == [">"]) 
            then (forcat, xs)
        else separate xs (forcat ++ [x])

    --used in enterCommand gives us arguments of the current command in needed format         
    getAllArgs :: [String] -> [[String]]
    getAllArgs = map (\x->getWordsList x [])

    --used in enterCommand
    lowerCase :: String -> String
    lowerCase = unpack . toLower . pack

    --used in getFullPath
    isFullPath :: [String] -> Bool
    isFullPath lst = head lst == "/"

    getFullPath :: [String] -> [String] -> [String]
    getFullPath new old = do
        if (isFullPath new)
            then convert new []
        else convert (old ++ new) []
        where 
            convert :: [String] -> [String] -> [String]
            convert [] result = result
            convert (".": xs) result = convert xs result
            convert (".." : xs) result = convert xs (rmlast result)
            convert (x:xs) result = convert xs (result ++ [x])

    --returns the full path of every given path
    getFullPathFiles :: [[String]] -> [String] -> [[String]]
    getFullPathFiles files dir = map (\x -> getFullPath x dir) files
    
    --used in enterCommand removes white spaces in the begginig of given string
    rmvWhiteSpaceAtBeg :: String -> String
    rmvWhiteSpaceAtBeg "" = ""
    rmvWhiteSpaceAtBeg str = do
        if ((head str) /= ' ')
            then str
        else rmvWhiteSpaceAtBeg $ tail str

    --used in ls to print what directory has
    getListOfEl :: [String] -> FileSystem -> IO()
    getListOfEl path fs = do
        if (isPathInFS path fs)
            then do
                putStrLn $ getAllInDir fs path
        else putStrLn "Given wrong path!"

    --removes last element of list
    rmlast :: [a] -> [a]
    rmlast lst = take ((length lst) - 1) lst

    --for working with files:

    --converts given path to OS path  
    convertToActualPaths::[[String]] -> [String]
    convertToActualPaths files = do
        let newFiles = map (\x -> if x /= [] then tail x else x) files 
        map (\x -> intercalate [pathSeparator] x) newFiles

    --write information in file:
    getInfoFromFilesInFile :: [String] -> Handle -> IO()
    getInfoFromFilesInFile [] _ = putStrLn ""
    getInfoFromFilesInFile (x:xs) file = do
        str <- readFile x
        hPutStr file str 
        getInfoFromFilesInFile xs file 
    
    --deletes real files in OS
    removeActualFiles :: [String] -> IO()
    removeActualFiles [] = putStrLn "All removed!"
    removeActualFiles (x:xs) = do
        removeFile x
        removeActualFiles xs

   