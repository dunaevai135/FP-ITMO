import System.Cmd
import System.Environment
import System.IO
import Data.List.Split
import Data.List
import Data.Maybe

data QElement = QElement {
    name :: String,
    cmd :: String
} deriving (Show, Eq)

data DeltaElement = DeltaElement {
    from :: String,
    to :: String,
    letter :: String
} deriving (Show, Eq)

data Mchn = Mchn {
    q :: [QElement],
    delta :: [DeltaElement]
} deriving (Show, Eq)

main = do  
    args <- getArgs
    let fileName = head args
    handle <- openFile (fileName) ReadMode
    contents <- hGetContents handle
    let lines = splitOn "\n" contents
    -- print $ lines
    -- compile lines fileName
    m <- compute lines (Mchn [] []) 1 "" 
    hClose handle
    runMachine (name $ q m !! 0) m

runState state m
    | elem state (map name $ q m) = system $ fromJust $ lookup state (zip (map name $ q m) (map cmd $ q m))
    | otherwise = error $ "I do not know this state"

runMachine state m = do
    print $ state
    runState state m
    -- TODO after
    if not $ isNothing $ find (\x -> letter x == "E" && from x == state) (delta m)then do
        runMachine (nsate "E") m
    else do
        line <- getLine
        if line /= "" then do
            runMachine (nsate line) m
        else
            putStr "End"
        where
            nsate line = case find (\x -> letter x == line && from x == state) (delta m) of 
                        Just a       -> to a
                        Nothing -> error $ "I do not know this way to state"

outPut result = "digraph sm" ++ " {\n" ++ result ++ "}"

compute [] machine pos out
    | pos == 1 = error $ "Compilation error: file is empty."
    | otherwise = do
                    args <- getArgs
                    writeFile (name args) $ outPut out
                    return machine
                where
                    name args = case length args of 
                                2 -> last args
                                1 -> (head args) ++ ".dot"
                                otherwise -> error $ "Output error see help"

compute (line:lines) machine pos out
    | line == "" =  compute lines machine (pos + 1) out
    | command == "Q" && length operands >= 2 = compute lines
        (Mchn (q machine ++ [QElement (operands !! 0) (foldl ((++).(++" ")) "" (tail operands))]) (delta machine) ) (pos + 1) 
        (out ++ "\t" ++ (operands !! 0) ++ ";\n")
    | elem command (map name (q machine)) && length operands == 2 = compute lines
        (Mchn (q machine) (delta machine ++ [DeltaElement (command) (operands !! 0) (operands !! 1)]) ) (pos + 1)
        (out ++ graphOut line)
    | command == "debug" = error $ show machine
    -- Coment
    | command == "#" = compute lines machine (pos + 1) out
    | otherwise = error $ "Compilation error on line " ++ (show pos)
        where 
            command = head $ splitOn " " line
            operands = tail $ splitOn " " line


graphOut line = "\t" ++ show f ++ " -> " ++ show to ++ " [label=\"" ++ c ++ "\"];\n"
    where
        f = sl !! 0
        to = sl !! 1
        c = sl !! 2
        sl = splitOn " " line
