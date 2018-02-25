module Snake exposing (..)

import Html exposing (Attribute, Html, program, div, input, text)
import Html.Attributes exposing (attribute, property, style)
import Html.Events exposing (on, keyCode, onInput)
import Json.Encode exposing (string)
import Json.Decode as Decode
import Json.Encode as Encode
import List exposing (all, drop, foldl, length, map, maximum, member, range, repeat)
import List.Extra exposing (init, splitAt)
import Maybe exposing (Maybe, map2, withDefault)
import String exposing (fromList, join, toList)
import NativeModule exposing (randomInt)
import Task
import Process
import Time exposing (second)

tickDur : Float
tickDur = second * 0.25

yMax : Int
yMax = 8

xMax : Int
xMax = 12

initLen : Int
initLen = 3

emptyMap : List String
emptyMap =  repeat yMax (fromList (repeat xMax '.'))

type Point = Point Int Int

add : Point -> Point -> Point
add (Point y1 x1) (Point y2 x2) = Point (y1 + y2) (x1 + x2)

up : Point
up = Point -1 0

down : Point
down = Point 1 0

left : Point
left = Point 0 -1

right : Point
right = Point 0 1

inRange : Point -> Int -> Int -> Bool
inRange (Point y x) my mx =
  y < my
  && y >= 0
  && x < mx
  && x >= 0

type GameState = GameState (List Point) Point (Maybe Point) (Maybe Point)

initState : GameState
initState =
  let initY = yMax // 2
      initX = xMax // 2
      initHead = initX - initLen + 1
      snake = map (\x -> Point initY x) <| range initHead initX
  in placeFood <| GameState snake (Point 0 0) Nothing (Just (Point 0 -1))

reset : GameState -> GameState
reset (GameState snake _ _ _ as state) = case snake of
  [] -> initState
  _ -> state

changeDir : Maybe Point -> GameState -> GameState
changeDir newDir (GameState snake food dir lastDir as state) = case newDir of
  Nothing -> state
  _ ->
    if map2 add newDir lastDir == Just (Point 0 0) then
      state
    else
      GameState snake food newDir lastDir

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

centreText : List String -> String
centreText lines =
  let yTopPadAmount = (yMax - length lines) // 2
      yBottomPadAmount = yMax - yTopPadAmount
      yTopPad = join "" <| repeat yTopPadAmount "<br>"
      yBottomPad = join "" <| repeat yBottomPadAmount "<br>"
  in yTopPad ++ join "<br>" lines ++ yBottomPad

toStr : GameState -> String
toStr (GameState snake food _ _) = case snake of
  [] -> centreText ["Game over!", "r to restart"]
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
  | Move

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of

    NoOp ->
      (model, Cmd.none)

    KeyDown key ->
      ({ model | state = handleInput key model.state }, Cmd.none)

    Move ->
      ({ model | state = move model.state }, loop)

tabindex : Attribute msg
tabindex =
  attribute "tabindex" "0"

view : Model -> Html Msg
view model =
  div [ monoStyle, tabindex, onKeyDown KeyDown ] [ textHtml <| toStr model.state ]

loop : Cmd Msg
loop =
    Process.sleep tickDur |> Task.perform (\_ -> Move)

main : Program Never Model Msg
main =
  program
  { init = ({ state = initState }, loop)
  , view = view
  , update = update
  , subscriptions = (\_ -> Sub.none)
  }

monoStyle : Attribute Msg
monoStyle = style [
  ("font-family", "monospace"),
  ("font-size", "48px")
  ]

onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
  on "keydown" (Decode.map tagger keyCode)

handleInput : Int -> GameState -> GameState
handleInput c = case c of
  38 -> changeDir <| Just up -- up arrow
  87 -> changeDir <| Just up -- w key
  37 -> changeDir <| Just left -- left arrow
  65 -> changeDir <| Just left -- a key
  40 -> changeDir <| Just down -- down arrow
  83 -> changeDir <| Just down -- s key
  39 -> changeDir <| Just right -- right arrow
  68 -> changeDir <| Just right -- d key
  82 -> reset -- r key
  _ -> identity
