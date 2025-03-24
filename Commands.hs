module Commands where
    import FileSystem
    import HelperFunctions
    import System.IO
    import System.Directory
    import Data.List (intercalate, words)

    --changes current directory if needed 
    --it takes: command; fs; posible directory; old directory and returns the changed dir
    changeList :: String -> FileSystem -> [[String]] -> [String] -> [String]
    changeList "cd" fs new old = snd (cd new old fs)
    changeList _ _ _ old = old

    --changes the file system if needed 
    --it takes: comand; fs; args of command; current directory
    changeFS :: String -> FileSystem -> [[String]] -> [String] -> FileSystem
    changeFS "cat" fs args dir = do
        let concat = separate args []
            fullPaths = getFullPathFiles ((fst concat) ++ (snd concat)) dir
            path = rmlast $ last fullPaths
        if ((snd concat) /= [])
            then 
                if ((areAllFilesInFS (rmlast fullPaths) fs) && (isPathInFS path fs) && (not (isFileInFs (last fullPaths) fs)))
                    then do
                        let f = File (last (last fullPaths))
                        insertSubTree fs f path 
                else fs
        else fs
    changeFS "rm" fs args dir = do
        let fullPaths = getFullPathFiles args dir
        if (areAllFilesInFS fullPaths fs)
            then removeListFiles fs fullPaths
        else fs
    changeFS "mkdir" fs args dir = do
        if (length args > 1 || length args == 0)
            then fs
        else do
            let path = getFullPath (head args) dir
                name = last path
            if(isPathInFS (rmlast path) fs)
                then insertSubTree fs (Directory name [Empty]) (rmlast path)
            else fs 
    changeFS _ old _ _ = old

    --print working directory
    pwd :: [[String]] -> [String] -> IO()
    pwd [] dir = do
        if (length dir > 1)
            then putStrLn $ tail (intercalate "/" dir)
        else putStrLn $ intercalate "/" dir
    pwd _ _ = putStrLn "pwd takes 0 arguments!"

    -- takes: arguments of the command; current diectory; fs
    -- retuns tuple of Bool (if the directory was changed) and current directory
    cd :: [[String]] -> [String] -> FileSystem -> (Bool, [String])
    cd [[".."]] ["/"] _ = (False, ["/"])
    cd [[".."]] dir _ = (True, take ((length dir) - 1) dir)
    cd [["."]] dir _ = (True, dir)
    cd [] dir _ = (False, dir)
    cd [[x]] dir fs = do 
        if (isPathInFS (dir ++ [x]) fs)
            then (True, dir ++ [x])
        else (False, dir)
    cd [path] dir fs = do 
        let newPath = getFullPath path dir
        if (isPathInFS newPath fs)
            then (True, newPath)
        else (False, dir) 
    cd (_:_) dir _ = (False, dir)

    --print elements of the directory
    ls :: [[String]] -> [String] -> FileSystem -> IO()
    ls [] dir fs = getListOfEl dir fs
    ls [path] dir fs = do  
        if (getFullPath path dir == [])
            then putStrLn "Wrong path!"
        else getListOfEl (getFullPath path dir) fs
    ls (x:xs) _ _ = putStrLn "ls takes 0 or 1 arguments!"

    --concatenates files
    cat :: [[String]] -> [String] -> FileSystem -> IO()
    cat [] _ _  = putStrLn "cat takes 1 or more arguments!"
    cat files dir fs = do
        let concat = separate files []
            first = fst concat
            second = snd concat
        if (first == [])
            then do
                let filePath = getFullPath (head second) dir
                if (filePath == [])
                    then putStrLn "Wrong path!" 
                else
                    if(length second /= 1 && not (isPathInFS (rmlast filePath) fs))
                        then putStrLn "Wrong number of arguments or given wrong path!"
                    else do
                        let filePath = convertToActualPaths (getFullPathFiles second dir)
                        file <- openFile (head filePath) AppendMode
                        getCharFromIO file
        else if ((snd concat) == [])
                then do
                    let full = getFullPathFiles first dir
                    if (areAllFilesInFS full fs)
                        then getInfoFromFilesInFile (convertToActualPaths full) stdout
                    else putStrLn "Given wrong path!"
            else 
                if (length second /= 1)
                    then putStrLn "Wrong number of arguments!"
                else do
                        let fullF = getFullPathFiles first dir
                            fullS =  head (getFullPathFiles second dir)
                        if (fullS == [])
                            then putStrLn "Wrong paths!"
                        else 
                            if ((areAllFilesInFS fullF fs) && (isPathInFS (rmlast fullS) fs))
                                then do
                                    let actualPaths = convertToActualPaths (fullF ++ [fullS])
                                    handle <- openFile (last actualPaths) AppendMode
                                    getInfoFromFilesInFile (rmlast actualPaths) handle
                                    hClose handle
                            else putStrLn "Invalid paths!"
        where 
            getCharFromIO :: Handle -> IO()
            getCharFromIO file = do
                letter <- getChar
                if (letter == '.')
                    then hClose file
                else do
                    hPutChar file letter
                    getCharFromIO file 

    --remove files
    rm :: [[String]] -> [String] -> FileSystem -> IO()
    rm args dir fs = do 
        let fullPaths = getFullPathFiles args dir
        if (areAllFilesInFS fullPaths fs) 
            then removeActualFiles (convertToActualPaths fullPaths)
        else putStrLn "Incorrect paths!"

    --create directory if the rest of the path is in fs
    mkdir :: [[String]] -> [String] ->FileSystem -> IO()
    mkdir ls dir fs = do
        if (length ls /= 1)
            then putStrLn "mkdir takes 1 argument!"
        else do
            let fullPath = getFullPath (head ls) dir
            if (fullPath == [])
                then putStrLn "Wrong path!"
            else do
                let path = rmlast fullPath
                    name = last fullPath
                if (isPathInFS path fs)
                    then createDirectory $ head $ convertToActualPaths [fullPath]
                else putStrLn "Can't create directoy with given path!" 

    --execute command on fs with arguments of the command and current directory 
    executeCommand :: String -> FileSystem -> [[String]] -> [String] -> IO()
    executeCommand "pwd" _ lstcmd dir = pwd lstcmd dir
    executeCommand "cd" fs lstcmd dir = do
        if (fst (cd lstcmd dir fs))
            then putChar '\0'
        else putStrLn "Can't change current directory! Wrong path or number of arguments!"
    executeCommand "ls" fs lstcmd dir = ls lstcmd dir fs
    executeCommand "cat" fs lstcmd dir = cat lstcmd dir fs
    executeCommand "rm" fs lstcmd dir = rm lstcmd dir fs
    executeCommand "mkdir" fs lstcmd dir = mkdir lstcmd dir fs
    executeCommand _ _ _ _ = putStrLn "Wrong command!"

    --simulating user interface
    enterComand :: FileSystem -> [String] -> IO()
    enterComand fs dir = do
        cmd <- getLine
        let command = rmvWhiteSpaceAtBeg cmd
        if (command == "")
            then enterComand fs dir
        else do
            let lstcmd = words command
                argscmd = getAllArgs (tail lstcmd)
                lowerCommand = lowerCase $ head lstcmd
            if (lowerCommand == "exit")
                then do 
                    save fs
                    putStrLn "Exit the program!"
            else 
                if (elem lowerCommand commandList)
                    then do
                        executeCommand lowerCommand fs argscmd dir
                        enterComand (changeFS lowerCommand fs argscmd dir) (changeList lowerCommand fs argscmd dir)
                else do 
                    putStrLn "Invalid command!"
                    enterComand fs dir
            where
                commandList = ["cd", "mkdir", "pwd", "ls" , "cat", "rm"]

    --load needed information from file  
    load :: String -> IO()
    load path = do
        file <- openFile path ReadMode
        info <- hGetLine file
        let fs = read info::FileSystem
        hClose file
        enterComand fs ["/"]

            
   