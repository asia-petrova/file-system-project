module FileSystem where
    import System.IO

    data FileSystem = Empty | File String | Directory String [FileSystem]
        deriving(Show, Read, Eq)

    getRootValue:: FileSystem ->  String
    getRootValue Empty = "\0"
    getRootValue (File name) = name
    getRootValue (Directory name _) = name
    
    --gets the content of every directory as list of directories/files
    getChildrenList :: FileSystem -> [FileSystem]
    getChildrenList (Directory _ ls) = ls
    getChildrenList _ = []

    --returns string of every element in directory
    getAllInDir :: FileSystem -> [String] -> String
    getAllInDir fs path = do
        let subtree = extractSubtree fs (getChildrenList fs) path
        unwords (removeEmpty (map (\x -> getRootValue x) (getChildrenList subtree)) [])
        where 
            removeEmpty :: [String] -> [String] -> [String] 
            removeEmpty [] result = result
            removeEmpty ("\0":xs) result = removeEmpty xs result
            removeEmpty (x:xs) result = removeEmpty xs (result ++ [x])

    --gets: tree; list (chilren of the root); list of the path 
    --returns: subtree with root last element of path 
    --if the path is incorrect returns Empty
    extractSubtree :: FileSystem -> [FileSystem] -> [String] -> FileSystem
    extractSubtree tree _ [x] = 
        if ((getRootValue tree) == x) 
            then tree
        else Empty
    extractSubtree _ [] _ = Empty
    extractSubtree Empty _ _ = Empty
    extractSubtree tree (t:ts) (x:xs) = 
        if ((getRootValue tree) ==  x)
            then do
                let h = head xs
                if (getRootValue t == h)
                    then  extractSubtree t (getChildrenList t) xs
                else 
                    extractSubtree tree ts (x:xs)
        else Empty

    --insert tree/node in tree with given path where 
    --if the path is incorrect returns the same tree
    insertSubTree :: FileSystem -> FileSystem -> [String] -> FileSystem
    insertSubTree tree _ [] = tree
    insertSubTree Empty _ _ = Empty
    insertSubTree file@(File _) _ _ = file
    insertSubTree tree@(Directory root children) n [x] = 
        if (root == x)
            then Directory root (children ++ [n])
        else tree
    insertSubTree tree@(Directory root children) n (x:xs) = 
        if (root == x) 
            then Directory x (map (\z->insertSubTree z n xs) children)
        else tree

    --deletes file (given as path) from file system
    removeNodes :: FileSystem -> [String] -> FileSystem
    removeNodes f@(File x) [z] = do
        if (x == z)
            then Empty
        else f
    removeNodes f@(File _) (_:_) = f
    removeNodes Empty _ = Empty
    removeNodes fs [] = fs
    removeNodes tree@(Directory root children) (x:xs) = do
        if (root == x)
            then Directory x (map (\x -> removeNodes x xs) children)
        else tree

    --deletes multiple files (given as paths)
    removeListFiles :: FileSystem -> [[String]] -> FileSystem
    removeListFiles fs [] = fs
    removeListFiles fs (x:xs) = removeListFiles (removeNodes fs x) xs 

    --saves the fs in file after we exit the program
    save :: FileSystem -> IO()
    save tree = writeFile "fileSystem.txt" $ show tree

    isFile :: FileSystem -> Bool
    isFile (File _) = True
    isFile _ = False

    isDirectory :: FileSystem -> Bool
    isDirectory (Directory _ _) = True
    isDirectory _ = False

    --chek if path is from file system
    isPathInFS :: [String] -> FileSystem -> Bool
    isPathInFS [] _ = False
    isPathInFS path fs = isDirectory tree
        where 
            tree = extractSubtree fs (getChildrenList fs) path

    --chek if file(given as path) is from fs
    isFileInFs :: [String] -> FileSystem -> Bool
    isFileInFs [] _ = False
    isFileInFs path fs = isFile tree
        where tree = extractSubtree fs (getChildrenList fs) path

    --chek if given paths are files in fs
    areAllFilesInFS :: [[String]] -> FileSystem -> Bool
    areAllFilesInFS [] _ = False
    areAllFilesInFS files fs = all (\x -> isFileInFs x fs) files

    --chek if given paths are directories in fs
    areAllDirInFs :: [[String]] -> FileSystem -> Bool 
    areAllDirInFs [] _ = False
    areAllDirInFs files fs = all (\x -> isPathInFS x fs) files


        

