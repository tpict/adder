module GameState exposing (..)

import List exposing (all, drop, foldl, length, map, maximum, member, range, repeat)
import List.Extra exposing (init, splitAt)
import Maybe exposing (Maybe, map2, withDefault)
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


type GameState
    = GameState (List Point) Point (Maybe Point) (Maybe Point)


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
        placeFood <| GameState snake (Point 0 0) Nothing (Just left)


reset : GameState -> GameState
reset ((GameState snake _ _ _) as state) =
    case snake of
        [] ->
            initState

        _ ->
            state


changeDir : Maybe Point -> GameState -> GameState
changeDir newDir ((GameState snake food dir lastDir) as state) =
    case newDir of
        Nothing ->
            state

        _ ->
            if map2 add newDir lastDir == Just (Point 0 0) then
                state
            else
                GameState snake food newDir lastDir


placeFood : GameState -> GameState
placeFood ((GameState snake food dir lastDir) as state) =
    let
        y =
            randomInt 0 (yMax - 1)

        x =
            randomInt 0 (xMax - 1)

        p =
            Point y x

        inSnake =
            member p snake
    in
        if inSnake then
            placeFood state
        else
            GameState snake p dir lastDir


move : GameState -> GameState
move state =
    case state of
        GameState _ _ Nothing _ ->
            state

        GameState [] _ _ _ ->
            state

        GameState (x :: xs) food (Just dir) oldDir ->
            let
                newHead =
                    add dir x

                newSnake =
                    init (newHead :: x :: xs)
            in
                if not (inRange newHead yMax xMax) then
                    -- collided with wall
                    GameState [] food (Just dir) oldDir
                else if member newHead (x :: xs) then
                    -- collided with self
                    GameState [] food (Just dir) oldDir
                else if newHead == food then
                    -- eating food
                    placeFood <| GameState (newHead :: x :: xs) food (Just dir) (Just dir)
                else
                    -- regular movement
                    case newSnake of
                        Nothing ->
                            state

                        Just s ->
                            GameState s food (Just dir) (Just dir)


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
            yMax - yTopPadAmount

        yTopPad =
            join "" <| repeat yTopPadAmount "<br>"

        yBottomPad =
            join "" <| repeat yBottomPadAmount "<br>"
    in
        yTopPad ++ join "<br>" lines ++ yBottomPad


toStr : GameState -> String
toStr (GameState snake food _ _) =
    case snake of
        [] ->
            centreText [ "Game over!", "r to restart" ]

        _ ->
            emptyMap |> addFood food |> addSnake snake |> join "<br>"
