import qualified System.Console.ANSI as ANSI
import           Control.Concurrent
import           System.IO
import System.Exit

yMax = 10
xMax = 20
initialLength = 3
emptyMap =  replicate yMax $ replicate xMax '.'

data Point = Point { getY :: Int, getX :: Int } deriving (Show, Eq)

(Point a b) |+| (Point c d) = Point (a + c) (b + d)
infixl 6 |+|

inRange :: Point -> Int -> Int -> Bool
inRange point my mx = and [
  getY point < my,
  getY point >= 0,
  getX point < mx,
  getX point >= 0
  ]

data GameState = GameState {
  snake :: Maybe [Point],
  food :: Point,
  dir :: Maybe Point
} deriving (Show)

move :: GameState -> GameState
move state@(GameState _ _ Nothing) = state
move state@(GameState Nothing _ _) = state
move (GameState (Just (x:xs)) food (Just dir)) =
  let newSnake | not $ inRange newHead yMax xMax = Nothing
               | elem newHead (x:xs) == True = Nothing
               | otherwise = Just $ init $ newHead:x:xs
  in GameState newSnake food (Just dir)
  where newHead = (dir |+| x)

modStr :: Point -> Char -> [String] -> [String]
modStr point c str = let (beforeLines, line:afterLines) = splitAt (getY point) str
                         (beforeChars, _:afterChars) = splitAt (getX point) line
                         newLine = beforeChars ++ [c] ++ afterChars
  in beforeLines ++ [newLine] ++ afterLines

addSnake :: [Point] -> [String] -> [String]
addSnake snake map = foldl (\acc x -> modStr x 's' acc) map $ snake

addFood :: Point -> [String] -> [String]
addFood food map = modStr food 'O' map

toStr :: GameState -> String
toStr (GameState Nothing _ _) = "Game over!"
toStr state@(GameState (Just snake) food _) = unlines $ addSnake snake $ addFood food emptyMap

toDir :: Char -> Maybe Point
toDir c
  | c == 'w' = Just (Point (-1) 0)
  | c == 'a' = Just (Point 0 (-1))
  | c == 's' = Just (Point 1 0)
  | c == 'd' = Just (Point 0 1)
  | otherwise = Nothing

changeDir :: Maybe Point -> GameState -> GameState
changeDir Nothing state = state
changeDir newDir state
  | ((|+|) <$> newDir <*> (dir state)) == Just (Point 0 0) = state
  | otherwise = GameState {
    snake = snake state,
    food = food state,
    dir = newDir
  }

snakey = GameState {
  snake = Just [Point 0 x | x <- reverse [0..initialLength - 1]],
  food = Point 5 10,
  dir = Nothing
}

clearScreen = do
  ANSI.cursorUp $ yMax + 1
  putStrLn $ unlines $ replicate yMax $ replicate xMax ' '
  ANSI.cursorUp $ yMax + 1

main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  c <- newEmptyMVar
  putMVar c snakey
  putStrLn $ toStr snakey
  forkIO $ wait c
  myinput c
  where wait c = do
          -- auto move goes here
          threadDelay 1000000 >> wait c
        myinput c = do
          a <- getChar
          state <- takeMVar c
          dir <- return (toDir a)
          newState <- return (changeDir dir state)
          anotherNewState <- return (move newState)
          clearScreen
          putStrLn $ toStr anotherNewState
          if snake anotherNewState == Nothing then
            exitSuccess
          else putMVar c anotherNewState >> myinput c
