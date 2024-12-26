import Data.Char (isDigit)
import Utils (readInt)

----------------------------------------------
-- The file system, modelled as a Rose Tree --
----------------------------------------------

type Filename   = String
type Filesize   = Int
type Foldername = String
data Node      = Empty
                | File Filename Filesize
                | Dir Foldername [Node]
                deriving Show

---------------------------------------------
-- And some straightforward helpers for it --
---------------------------------------------

-- Checks whether a given node is a directory.
isFolder          :: Node -> Bool
isFolder (Dir _ _) = True
isFolder _         = False

-- Given the root node, return its subfolders. We only
-- need this because "/" contains some files which we
-- want to ignore.
getRootFolders                   :: Node -> [Node]
getRootFolders (Dir "/" children) = filter isFolder children
getRootFolders _                  = error "Please provide root node."

-- Given a node, calculate its size recursively.
nodeSize                        :: Node -> Int
nodeSize Empty                   = 0
nodeSize (File _ size)           = size
nodeSize dir@(Dir name children) = sum $ map nodeSize children

-- Prefix the name of a node; needed for path normalisation.
prefix                         :: String -> Node -> Node
prefix pre (File name size)     = File (pre ++ name) size
prefix pre (Dir  name children) = Dir  (pre ++ name) children

----------------------------------------------------
-- And some slightly more involved helpers for it --
----------------------------------------------------

-- Insert a given node into a folder (identified by name) of an existing tree
insertNodeAt                        :: Foldername -> Node -> Node -> Node
insertNodeAt _ node Empty            = node
insertNodeAt _ node (File name size) = File name size
insertNodeAt target node (Dir folderName children)
    | folderName == target = Dir folderName (node : children)
    | otherwise            = Dir folderName (map (insertNodeAt target node) children)


-- Insert a list of nodes into a folder (identified by name) of an existing tree
insertNodesAt                         :: [Node] -> Foldername -> Node -> Node
insertNodesAt [] _ tr                  = tr
insertNodesAt (node:nodes) target tree = insertNodesAt nodes target tree'
  where
    tree'                          = insertNodeAt target node tree


-------------------
-- Parsing stuff --
-------------------

-- Chop a terminal session into a list of folder names and their listings
chopSession :: [String] -> ([String], [[String]])
chopSession xs = go xs "/" ([], [])
  where
    go :: [String] -> String -> ([String], [[String]]) -> ([String], [[String]])
    go [] _ accum = accum
    go (l:ls) curPath accum
        | l == "$ cd .."      = go ls  parentDir accum
        | take 5 l == "$ cd " = go ls  nextDir   accum
        | l == "$ ls"         = go ls' curPath   accum'
      where
        parentDir = reverse . dropWhile (/='/') . drop 1 . reverse $ curPath
        nextDir   = curPath ++ (drop 5 l) ++ "/"
        ls'       = (drop (length listing) ls)
        accum'    = (fst accum ++ [curPath], snd accum ++ [listing])
        listing   = takeWhile (\s -> s!!0 /= '$') ls

-- Parse the output of a single "ls" statement to a list of nodes.
-- This is applies to the second elements of the tuples the previous
-- function outputs. 
parseLs       :: [String] -> [Node]
parseLs []     = []
parseLs (l:ls) = current : parseLs ls
  where
    current    = case (take 3 l) of
        "dir" -> Dir  folderName []
        _     -> File fileName   fileSize
    folderName = drop 4 l ++ "/"
    fileSize   = readInt . takeWhile isDigit $ l
    fileName   = drop 1 $ snd $ break (==' ') l


main = do
    -- Read file lines, skipping initial "cd /" as per task
    fileContent  <- readFile "test.txt"
    let fileLines = drop 1 . lines $ fileContent
    
    -- Parse the session into folder names and associated folder contents.
    let choppedSession = chopSession fileLines
    let folders        =               fst $ choppedSession
    let contents       = map parseLs . snd $ choppedSession
    let input          = zip folders contents
    
    -- Prefix entry names and build the tree from that
    let normalisedInput = map (\(par, node) -> (par, map (prefix par) node))
                        $ input
    let tree            = foldl (\tree (par, nodes) -> insertNodesAt nodes par tree) (Dir "/" [])
                        $ normalisedInput
    
    -- Part 1: Sum sizes of all directories which are smaller than 1e5
    print $ sum
          . filter (<= 100000)
          . map nodeSize
          $ getRootFolders tree

    -- Part 2: Find the smallest directory we need to delete to have enough space
    let required = nodeSize tree - 40000000
    print $ minimum
          . filter (>= required)
          . map nodeSize
          $ getRootFolders tree

    print $ "Done."
