import qualified System.Console.ANSI as ANSI
import           Control.Concurrent
import           System.IO

yMax = 10
xMax = 20
emptyMap =  replicate yMax $ replicate xMax '.'

data Point = Point Int Int deriving (Show, Eq)

(Point a b) |+| (Point c d) = Point (a + c) (b + d)
infixl 6 |+|

getY (Point a b) = a
getX (Point a b) = b

data GameState = GameState {snake :: Maybe [Point], dir :: Maybe Point} deriving (Show)

move :: Maybe Point -> Maybe [Point] -> Maybe [Point]
move Nothing xs = xs
move _ Nothing = Nothing
move (Just dir) (Just (x:xs))
  | getY newHead < 0 = Nothing
  | getY newHead >= yMax = Nothing
  | getX newHead < 0 = Nothing
  | getX newHead >= xMax = Nothing
  | otherwise = Just $ init $ newHead:x:xs
  where newHead = (dir |+| x)

modStr :: Point -> [String] -> [String]
modStr point str = let (beforeLines, line:afterLines) = splitAt (getY point) str
                       (beforeChars, _:afterChars) = splitAt (getX point) line
                       newLine = beforeChars ++ ['x'] ++ afterChars
  in beforeLines ++ [newLine] ++ afterLines

toStr :: GameState -> [String]
toStr (GameState Nothing _) = ["Game over!"]
toStr state@(GameState (Just snake) _) = foldl (\acc x -> modStr x acc) emptyMap $ snake

charToDir :: Char -> Maybe Point
charToDir c
  | c == 'w' = Just (Point (-1) 0)
  | c == 'a' = Just (Point 0 (-1))
  | c == 's' = Just (Point 1 0)
  | c == 'd' = Just (Point 0 1)
  | otherwise = Nothing

movePlease :: Char -> GameState -> GameState
movePlease c state = let newDir = charToDir c
  in GameState {
    snake = move newDir $ snake state,
    dir = newDir
  }

snakey = GameState {
  snake = Just [Point 0 x | x <- reverse [0..2]],
  dir = Nothing
}

main = do
  c <- newEmptyMVar
  putMVar c snakey
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  putStrLn $ unlines $ toStr snakey
  forkIO $ myinput c
  wait c
  where wait c = do
          -- auto move goes here
          threadDelay 1000000 >> wait c
        myinput c = do
          a <- getChar
          state <- takeMVar c
          ANSI.cursorUp $ yMax + 1
          putStrLn $ unlines $ replicate yMax $ replicate xMax ' '
          ANSI.cursorUp $ yMax + 1
          putStrLn $ unlines $ toStr $ movePlease a state
          if snake state == Nothing then
            return ()
          else putMVar c (movePlease a state) >> myinput c
