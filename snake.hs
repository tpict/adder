import           Control.Concurrent
import           Data.Maybe
import qualified System.Console.ANSI as ANSI
import           System.Exit
import           System.IO
import           System.Random

tickDur = 200000
yMax = 10
xMax = 20
initLen = 3
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
  snake   :: Maybe [Point],
  food    :: Point,
  dir     :: Maybe Point,
  lastDir :: Maybe Point,
  randGen :: StdGen
} deriving (Show)

placeFood :: GameState -> GameState
placeFood state
    | inSnake = placeFood state { randGen = lastGen }
    | otherwise = state { food = Point y x, randGen = lastGen }
    where gen = randGen state
          (y, newGen) = randomR (0, yMax - 1) gen
          (x, lastGen) = randomR (0, xMax - 1) newGen
          p = Point y x
          inSnake = fromMaybe False $ (elem p) <$> snake state

move :: GameState -> GameState
move state@(GameState { dir = Nothing }) = state
move state@(GameState { snake = Nothing }) = state
move state@(GameState (Just (x:xs)) food (Just dir) _ _)
  -- if the head is out of range
  | not $ inRange newHead yMax xMax = state { snake = Nothing }

  -- if the head has collided with the tail
  | elem newHead (x:xs) == True = state { snake = Nothing }

  -- if the head is over food
  | newHead == food = placeFood state {
    snake = Just (newHead:x:xs),
    lastDir = Just dir
  }

  -- regular movement
  | otherwise = state {
    snake = Just $ init $ newHead:x:xs,
    lastDir = Just dir
  }
  where newHead = dir |+| x

modStr :: Point -> Char -> [String] -> [String]
modStr point c str = let (beforeLines, line:afterLines) = splitAt (getY point) str
                         (beforeChars, _:afterChars) = splitAt (getX point) line
                         newLine = beforeChars ++ [c] ++ afterChars
  in beforeLines ++ [newLine] ++ afterLines

addSnake :: [Point] -> [String] -> [String]
addSnake (x:xs) map = modStr x 'S' $ foldl (\acc y -> modStr y 's' acc) map xs

addFood :: Point -> [String] -> [String]
addFood food map = modStr food 'O' map

toStr :: GameState -> String
toStr (GameState { snake = Nothing }) = "Game over!"
toStr (GameState (Just snake) food _ _ _) = unlines $ addSnake snake $ addFood food emptyMap

toDir :: Char -> Maybe Point
toDir c
  | c == 'w' = Just $ Point (-1) 0
  | c == 'a' = Just $ Point 0 (-1)
  | c == 's' = Just $ Point 1 0
  | c == 'd' = Just $ Point 0 1
  | otherwise = Nothing

changeDir :: Maybe Point -> GameState -> GameState
changeDir Nothing state = state
changeDir newDir state
  | ((|+|) <$> newDir <*> (lastDir state)) == Just (Point 0 0) = state
  | otherwise = state { dir = newDir }

getInitialState = do
    gen <- getStdGen
    state <- return GameState {
      snake = Just [Point initY x | x <- reverse [initX - initLen + 1..initX]],
      food = Point 0 0,
      dir = Nothing,
      lastDir = Nothing,
      randGen = gen
    }
    return $ placeFood state
  where initY = yMax `div` 2
        initX = xMax `div` 2

clearScreen = do
  ANSI.cursorUp $ yMax + 1
  putStrLn $ unlines $ replicate yMax $ replicate xMax ' '
  ANSI.cursorUp $ yMax + 1

handleInput c = do
  dir <- toDir <$> getChar
  changeDir dir <$> takeMVar c >>= putMVar c
  handleInput c

gameTick c = do
  newState <- move <$> takeMVar c
  clearScreen
  putStrLn $ toStr newState
  if snake newState == Nothing then
    exitSuccess
  else putMVar c newState >> threadDelay tickDur >> gameTick c

main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  c <- newEmptyMVar
  state <- getInitialState
  putMVar c state
  putStrLn $ toStr state
  forkIO $ handleInput c
  gameTick c
