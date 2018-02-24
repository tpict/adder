import qualified System.Console.ANSI as ANSI
import           Control.Concurrent
import           System.IO

data Point = Point Int Int deriving (Show)

(Point a b) |+| (Point c d) = Point (a + c) (b + d)
infixl 6 |+|

getY (Point a b) = a
getX (Point a b) = b


move :: Maybe Point -> [Point] -> [Point]
move Nothing xs = xs
move (Just dir) (x:xs) = init $ (dir |+| x):x:xs

modStr :: Point -> [[Char]] -> [[Char]]
modStr point str = let (beforeLines, line:afterLines) = splitAt (getY point) str
                       (beforeChars, _:afterChars) = splitAt (getX point) line
                       newLine = beforeChars ++ ['x'] ++ afterChars
  in beforeLines ++ [newLine] ++ afterLines

toStr :: [Point] -> [[Char]]
toStr xs = foldl (\acc x -> modStr x acc) (replicate 10 (replicate 20 '.')) xs

charToDir :: Char -> Maybe Point
charToDir c
  | c == 'w' = Just (Point (-1) 0)
  | c == 'a' = Just (Point 0 (-1))
  | c == 's' = Just (Point 1 0)
  | c == 'd' = Just (Point 0 1)
  | otherwise = Nothing

movePlease :: Char -> [Point] -> [Point]
movePlease c xs = move (charToDir c) xs

snake = ([Point 0 x | x <- reverse [0..2]], Nothing)

main = do
    c <- newEmptyMVar
    putMVar c snake
    hSetBuffering stdin NoBuffering
    hSetEcho stdin False
    putStrLn $ unlines $ toStr $ fst snake
    forkIO $ myinput c
    wait c
    where wait c = do
            -- auto move goes here
            threadDelay 1000000 >> wait c
          myinput c = do
            a <- getChar
            state <- takeMVar c
            ANSI.cursorUp $ (length $ toStr $ fst state) + 1
            putStrLn $ unlines $ toStr $ movePlease a $ fst state
            putMVar c (movePlease a (fst state), Just a)
            myinput c
