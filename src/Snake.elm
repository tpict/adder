module Snake exposing (..)

import Debug exposing (log)
import Html exposing (Attribute, Html, beginnerProgram, div, input, text)
import Html.Attributes exposing (attribute, property)
import Html.Events exposing (on, keyCode, onInput)
import Json.Encode exposing (string)
import Json.Decode as Decode
import Json.Encode as Encode
import List exposing (all, drop, foldl, map, member, range, repeat)
import List.Extra exposing (init, splitAt)
import Maybe exposing (Maybe, map2)
import String exposing (fromList, join, toList)
import NativeModule exposing (randomInt)


-- import           Control.Concurrent
-- import           Data.Maybe
-- import qualified System.Console.ANSI as ANSI
-- import           System.Exit
-- import           System.IO
-- import           System.Random

-- tickDur = 200000
yMax : Int
yMax = 10

xMax : Int
xMax = 20

initLen : Int
initLen = 3

emptyMap : List String
emptyMap =  repeat yMax (fromList (repeat xMax '.'))

type Point = Point Int Int

add : Point -> Point -> Point
add (Point y1 x1) (Point y2 x2) = Point (y1 + y2) (x1 + x2)
-- add (Point {a, c}) (Point {b, d})
--   = Point (a + c) (b + d)

-- (Point a b) |+| (Point c d) = Point (a + c) (b + d)
-- infixl 6 |+|

inRange : Point -> Int -> Int -> Bool
inRange (Point y x) my mx =
  y < my
  && y >= 0
  && x < mx
  && x >= 0

type GameState = GameState (List Point) Point (Maybe Point) (Maybe Point)

placeFood : GameState -> GameState
placeFood ((GameState snake food dir lastDir) as state) =
  let y = randomInt 0 (yMax - 1)
      x = randomInt 0 (xMax - 1)
      p = Point y x
      inSnake = member p snake
  in
      if inSnake then placeFood state
      else GameState snake p dir lastDir

move : GameState -> GameState
move state = case state of
  GameState _ _ Nothing _ -> state
  GameState [] _ _ _ -> state
  GameState (x::xs) food (Just dir) oldDir ->
    let newHead = add dir x
        newSnake = init (newHead::x::xs)
    in
        if not (inRange newHead yMax xMax) then
          GameState [] food (Just dir) oldDir
        else if member newHead (x::xs) then
          GameState [] food (Just dir) oldDir
        else if newHead == food then
          placeFood <| GameState (newHead::x::xs) food (Just dir) (Just dir)
        else
          case newSnake of
            Nothing ->
              state
            Just s -> GameState s food (Just dir) (Just dir)


modStr : Point -> Char -> List String -> List String
modStr (Point y x) c str =
 let (beforeLines, otherLines) = splitAt y str
     (line, afterLines) = case otherLines of
       [] -> ("", [])
       x::xs -> (x, xs)

     (beforeChars, otherChars) = splitAt x (toList line)
     (_, afterChars) = case otherChars of
       [] -> ('E', []) -- error state
       x::xs -> (x, xs)


     newLine = fromList (beforeChars ++ [c] ++ afterChars)
 in beforeLines ++ [newLine] ++ afterLines

addSnake : List Point -> List String -> List String
addSnake snake map = case snake of
  [] -> map
  x::xs -> modStr x 'S' (foldl (\y acc -> modStr y 's' acc) map xs)

addFood : Point -> List String -> List String
addFood food map = modStr food 'O' map

toStr : GameState -> String
toStr (GameState snake food _ _) = case snake of
  [] -> "Game over!"
  _ -> join "<br>" (addSnake snake (addFood food emptyMap))


textHtml: String -> Html msg
textHtml t =
    div
        [ string t
            |> property "innerHTML"
        ]
        []

type alias Model = { state : GameState  }

type Msg
  = NoOp
  | KeyDown Int

update: Msg -> Model -> Model
update msg model =
  case msg of

    NoOp ->
      model

    KeyDown key ->
      let newState = changeDir (toDir key) model.state
      in { model | state = move newState }

tabindex : Attribute msg
tabindex =
  attribute "tabindex" "0"

view : Model -> Html Msg
view model =
  div [ tabindex, onKeyDown KeyDown ] [ textHtml <| toStr model.state ]

initState : GameState
initState = let initY = yMax // 2
                initX = xMax // 2
                initHead = initX - initLen + 1
                snake = map (\x -> Point initY x) <| range initHead initX
      in GameState snake (Point 0 0) Nothing (Just (Point 0 -1))

main =
  beginnerProgram
  { model = { state = initState }
  , view = view
  , update = update
  }

onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
  on "keydown" (Decode.map tagger keyCode)

toDir : Int -> Maybe Point
toDir c = case c of
  87 -> Just (Point (-1) 0)
  65 -> Just (Point 0 (-1))
  83 -> Just (Point 1 0)
  68 -> Just (Point 0 1)
  _ -> Nothing

changeDir : Maybe Point -> GameState -> GameState
changeDir newDir ((GameState snake food dir lastDir) as state) = case newDir of
  Nothing -> state
  _ ->
    if map2 add newDir lastDir == Just (Point 0 0) then
      state
    else
      GameState snake food newDir lastDir
