module Snake exposing (..)

import Html exposing (Attribute, Html, program, div, input, text)
import Html.Attributes exposing (attribute, property, style)
import Html.Events exposing (on, keyCode, onInput)
import Json.Encode exposing (string)
import Json.Decode as Decode
import Json.Encode as Encode
import List exposing (all, concat, drop, foldl, length, map, maximum, member, range, repeat)
import List.Extra exposing (init, splitAt)
import Maybe exposing (Maybe, withDefault)
import Task
import Process
import Time exposing (second)
import Point exposing (..)


-- import Game.Resources as Resources exposing (Resources)

import Game.TwoD as Game
import Game.TwoD.Render as Render exposing (Renderable)
import Game.TwoD.Camera as Camera exposing (Camera)
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


type alias Model =
    { snake : List Point
    , food : Point
    , dir : Point
    , lastDir : Point
    , screen : ( Int, Int )
    , camera : Camera
    }


initState : Model
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
            , screen = ( 800, 600 )
            , camera = Camera.fixedArea (yMax * xMax |> toFloat) ( 6.0, 4.0 )
            }


reset : Model -> Model
reset state =
    case state.snake of
        [] ->
            initState

        _ ->
            state


changeDir : Point -> Model -> Model
changeDir newDir state =
    if add newDir state.lastDir == Point 0 0 then
        state
    else
        { state | dir = newDir }


placeFood : Model -> Model
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


move : Model -> Model
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


tickDur : Float
tickDur =
    second * 0.25


textHtml : String -> Html msg
textHtml t =
    div
        [ string t
            |> property "innerHTML"
        ]
        []


type Msg
    = NoOp
    | KeyDown Int
    | Move


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        KeyDown key ->
            ( handleInput key model, Cmd.none )

        Move ->
            ( move model, loop )


tabindex : Attribute msg
tabindex =
    attribute "tabindex" "0"


renderFood : Point -> Renderable
renderFood food =
    Render.sprite
        { position = ( getX food |> toFloat, getY food |> toFloat )
        , size = ( 1.0, 1.0 )
        , texture = Nothing
        }


renderSnake : List Point -> List Renderable
renderSnake snake =
    map
        (\point ->
            Render.sprite
                { position = ( getX point |> toFloat, getY point |> toFloat )
                , size = ( 1.0, 1.0 )
                , texture = Nothing
                }
        )
        snake


render : Model -> List Renderable
render { snake, food } =
    concat [ renderSnake snake, [ renderFood food ] ]


view : Model -> Html Msg
view ({ screen, camera } as model) =
    div [ tabindex, onKeyDown KeyDown ]
        [ Game.render
            { time = 0
            , camera = camera
            , size = screen
            }
            (render model)
        ]


loop : Cmd Msg
loop =
    Process.sleep tickDur |> Task.perform (\_ -> Move)


main : Program Never Model Msg
main =
    program
        { init = ( initState, loop )
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Decode.map tagger keyCode)


handleInput : Int -> Model -> Model
handleInput c =
    case c of
        -- up arrow
        38 ->
            changeDir <| up

        -- w key
        87 ->
            changeDir <| up

        -- left arrow
        37 ->
            changeDir <| left

        -- a key
        65 ->
            changeDir <| left

        -- down arrow
        40 ->
            changeDir <| down

        -- s key
        83 ->
            changeDir <| down

        -- right arrow
        39 ->
            changeDir <| right

        -- d key
        68 ->
            changeDir <| right

        -- r key
        82 ->
            reset

        _ ->
            identity
