import qualified System.Console.ANSI as ANSI
import           Control.Concurrent
import           System.IO

move :: Maybe (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
move Nothing xs = xs
move (Just (dirY, dirX)) (x:xs) = init $ (dirY + fst x, dirX + snd x):x:xs

modStr :: (Int, Int) -> [[Char]] -> [[Char]]
modStr point str = let (beforeLines, line:afterLines) = splitAt (fst point) str
                       (beforeChars, _:afterChars) = splitAt (snd point) line
                       newLine = beforeChars ++ ['x'] ++ afterChars
  in beforeLines ++ [newLine] ++ afterLines

toStr :: [(Int, Int)] -> [[Char]]
toStr xs = foldl (\acc x -> modStr x acc) (replicate 10 (replicate 20 '.')) xs

charToDir :: Char -> Maybe (Int, Int)
charToDir c
  | c == 'w' = Just (-1, 0)
  | c == 'a' = Just (0, -1)
  | c == 's' = Just (1, 0)
  | c == 'd' = Just (0, 1)
  | otherwise = Nothing

movePlease :: Char -> [(Int, Int)] -> [(Int, Int)]
movePlease c xs = move (charToDir c) xs

snake = ([(0, 2), (0, 1), (0, 0)], Nothing)

main = do
    c <- newEmptyMVar
    putMVar c snake
    -- thingy <- newEmptyMVar

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
            putMVar c (movePlease a $ fst state, Just a)
            myinput c
