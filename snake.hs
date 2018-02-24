import           Control.Concurrent
import           Data.Maybe
import qualified System.Console.ANSI as ANSI
import           System.Exit
import           System.IO
import           System.Random

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
  snake   :: Maybe [Point],
  food    :: Point,
  dir     :: Maybe Point,
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
move state@(GameState (Just (x:xs)) food (Just dir) _)
  | not $ inRange newHead yMax xMax = state {snake = Nothing}
  | elem newHead (x:xs) == True = state {snake = Nothing}
  | newHead == food = placeFood state {snake = Just (newHead:x:xs)}
  | otherwise = state {snake = Just $ init $ newHead:x:xs}
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
toStr state@(GameState (Just snake) food _ _) = unlines $ addSnake snake $ addFood food emptyMap

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
  | otherwise = state { dir = newDir }

getSnakey = do
  gen <- getStdGen
  state <- return GameState {
    snake = Just [Point 0 x | x <- reverse [0..initialLength - 1]],
    food = Point 0 0,
    dir = Nothing,
    randGen = gen
  }
  return $ placeFood state

clearScreen = do
  ANSI.cursorUp $ yMax + 1
  putStrLn $ unlines $ replicate yMax $ replicate xMax ' '
  ANSI.cursorUp $ yMax + 1

main = do
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False
  c <- newEmptyMVar
  snakey <- getSnakey
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
