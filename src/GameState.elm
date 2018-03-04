module GameState exposing (..)

import List exposing (all, drop, foldl, length, map, maximum, member, range, repeat)
import List.Extra exposing (init, splitAt)
import Maybe exposing (Maybe, withDefault)
import String exposing (fromList, join, toList)
import Point exposing (..)
import GlueRandom exposing (randomInt)


yMax : Int
yMax =
    8


xMax : Int
xMax =
    12


initLen : Int
initLen =
    3


emptyMap : List String
emptyMap =
    repeat yMax (fromList (repeat xMax '.'))


type alias GameState =
    { snake : List Point
    , food : Point
    , dir : Point
    , lastDir : Point
    }


initState : GameState
initState =
    let
        initY =
            yMax // 2

        initX =
            xMax // 2

        initHead =
            initX - initLen + 1

        snake =
            map (\x -> Point initY x) <| range initHead initX
    in
        placeFood <|
            { snake = snake
            , food = Point 0 0
            , dir = Point 0 0
            , lastDir = left
            }


reset : GameState -> GameState
reset state =
    case state.snake of
        [] ->
            initState

        _ ->
            state


changeDir : Point -> GameState -> GameState
changeDir newDir state =
    if add newDir state.lastDir == Point 0 0 then
      state
    else
      { state | dir = newDir }


placeFood : GameState -> GameState
placeFood state =
    let
        y =
            randomInt 0 (yMax - 1)

        x =
            randomInt 0 (xMax - 1)

        p =
            Point y x

        inSnake =
            member p state.snake
    in
        if inSnake then
            placeFood state
        else
            { state | food = p }


move : GameState -> GameState
move ({ snake, food, dir, lastDir } as state) =
    case ( snake, food, dir, lastDir ) of
        ( _, _, Point 0 0, _ ) ->
            state

        ( [], _, _, _ ) ->
            state

        ( x :: xs, food, dir, lastDir ) ->
            let
                newHead =
                    add dir x

                newSnake =
                    init (newHead :: x :: xs) |> withDefault []
            in
                if not (inRange newHead yMax xMax) then
                    -- collided with wall
                    { state | snake = [] }
                else if member newHead (x :: xs) then
                    -- collided with self
                    { state | snake = [] }
                else if newHead == food then
                    -- eating food
                    placeFood <|
                        { state
                            | snake = newHead :: x :: xs
                            , lastDir = dir
                        }
                else
                    -- regular movement
                    { state | snake = newSnake, lastDir = dir }


modStr : Point -> Char -> List String -> List String
modStr (Point y x) c str =
    let
        ( beforeLines, otherLines ) =
            splitAt y str

        ( line, afterLines ) =
            case otherLines of
                [] ->
                    ( "", [] )

                x :: xs ->
                    ( x, xs )

        ( beforeChars, otherChars ) =
            splitAt x (toList line)

        ( _, afterChars ) =
            case otherChars of
                -- error state
                [] ->
                    ( 'E', [] )

                x :: xs ->
                    ( x, xs )

        newLine =
            beforeChars ++ [ c ] ++ afterChars |> fromList
    in
        beforeLines ++ [ newLine ] ++ afterLines


addSnake : List Point -> List String -> List String
addSnake snake map =
    case snake of
        [] ->
            map

        x :: xs ->
            foldl (\y acc -> modStr y 's' acc) map xs |> modStr x 'S'


addFood : Point -> List String -> List String
addFood food map =
    modStr food 'O' map


centreText : List String -> String
centreText lines =
    let
        yTopPadAmount =
            (yMax - length lines) // 2

        yBottomPadAmount =
            yMax - yTopPadAmount - 1

        yTopPad =
            join "" <| repeat yTopPadAmount "<br>"

        yBottomPad =
            join "" <| repeat yBottomPadAmount "<br>"
    in
        yTopPad ++ join "<br>" lines ++ yBottomPad


toStr : GameState -> String
toStr { snake, food } =
    case snake of
        [] ->
            centreText [ "Game over!", "r to restart" ]

        _ ->
            emptyMap |> addFood food |> addSnake snake |> join "<br>"
