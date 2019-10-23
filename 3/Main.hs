import System.Environment
import Data.List.Split
import Data.List
import Data.Maybe
import Text.Read
import Text.Printf

helpStr = "-f <frequency> set frequency > 0 default = 0.1\n-t <type> approximation type \n\tline - Line Approximation\n\tspline - Spline interpolation"
formatStr = "Enter the points row by row in '<xi>;<yi>' format"
notMonoStr = "Each point should be grater than the previous one."
epsilon = 0.0000000001

printHelp = putStrLn $ helpStr ++ "\n" ++ formatStr

main = do 
    args <- getArgs
    runApproximation args
    where
        runApproximation args
            | (elem "-h" args) || (not $ elem "-t" args) || (frequency args) == -1 = printHelp
            | tIndex args /= -1 = case (maybeIndex ((tIndex args)+1) args) of
                                    Just "line"     -> lineApproximation [] $ frequency args
                                    Just "spline"   -> splineInterpolation [] $ frequency args
                                    _               -> printHelp
            | otherwise = printHelp
        frequency args
            | (elem "-f" args) = case (maybeIndex ((fromJust $ elemIndex "-f" args)+1) args) of
                                Just x  -> case readMaybe x :: Maybe Double of
                                            Just x | x > 0  -> x
                                            _               -> -1
                                Nothing -> -1
            | otherwise = 0.1
        tIndex args = case elemIndex "-t" args of
                        Just x -> x
                        Nothing -> -1

interpolate points = (\x -> let
    lamb xi = product $ map (\xj -> (x-xj)/(xi-xj)) (delete xi (map fst points))
    in sum $ zipWith (*) (map snd points) (map lamb (map fst points))
    )


splineInterpolation points frequency
    | points == [] = do
        line <- getLine
        if line /= "" then do
            let point = lineToPoint line
            splineInterpolation [point] frequency
        else putStr ""
    | otherwise = do
        line <- getLine
        if line /= "" then do
            let point = lineToPoint line
            let p = interpolate $ points ++ (point:[])
            let left = (fst . head) points
            let right = (fst . last) $ points
            putStrLn ("x\tp(x)\n" ++ (out frequency p left right))
            splineInterpolation (points ++ point:[]) frequency
        else putStr ""

lineApproximation [] frequency = do
    line <- getLine
    if line /= "" then do
        let point = lineToPoint line
        lineApproximation [point] frequency
    else
        putStr ""

lineApproximation [(x1, y1)] frequency = do
    line <- getLine
    if line /= "" then do
        let newPoint = lineToPoint line
        let x2 = fst newPoint
        let y2 = snd newPoint
        if x1 < x2 then do
            let p = approximate (x1, y1) newPoint
            putStr $ out frequency p x1 x2
            lineApproximation [newPoint] frequency
        else do 
            putStr notMonoStr
            lineApproximation [(x1, y1)] frequency
    else putStr ""

approximate (x1, y1) (x2, y2) = (\x -> k * x + b)
    where 
        k = (y2 - y1) / (x2 - x1)
        b = y1 - x1 * k

out frequency p left right
    | left  - right > epsilon = ""
    | otherwise = (printf "%.2f" left) ++ "\t" ++ show (p left) ++ "\n"
        ++ out frequency p (left + frequency) right
        
lineToPoint line = (x, y) 
    where 
        x = read $ splitedLine !! 0 :: Double
        y = read $ splitedLine !! 1 :: Double
        splitedLine = splitOn ";" line

-- findSubstring :: Eq a => [a] -> [a] -> Maybe Int
-- findSubstring pat str = findIndex (isPrefixOf pat) (tails str) 

maybeIndex n x  | n < length x = Just (x !! n)
                | otherwise = Nothing
