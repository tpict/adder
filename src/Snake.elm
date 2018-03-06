module Snake exposing (..)

import Html exposing (Attribute, Html, program, div, input, text)
import Html.Attributes exposing (attribute, property, style)
import Html.Events exposing (on, keyCode, onInput)
import Json.Encode exposing (string)
import Json.Decode as Decode
import Json.Encode as Encode
import List exposing (all, concat, drop, foldl, length, map, maximum, member, range, repeat)
import List.Extra as LExtra exposing (init, lift2)
import Maybe exposing (Maybe, withDefault)
import Task
import Process
import Time exposing (second)
import Point exposing (..)
import Game.TwoD as Game
import Game.TwoD.Render as Render exposing (Renderable)
import Game.TwoD.Camera as Camera exposing (Camera)
import Game.Resources as Resources exposing (Resources)
import GlueRandom exposing (randomInt)
import WebGL.Texture as Texture exposing (Texture)


yMax : Int
yMax =
    16


xMax : Int
xMax =
    16


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
    , resources : Resources
    }


initState : Model
initState =
    let
        floatY =
            toFloat yMax

        floatX =
            toFloat xMax

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
            , screen = ( xMax * 32, yMax * 32 )
            , camera = Camera.fixedArea (floatY * floatX) ( floatX / 2.0, floatY / 2.0 )
            , resources = Resources.init
            }


init : ( Model, Cmd Msg )
init =
    initState
        ! [ Cmd.map Resources (Resources.loadTextures [ "images/bg1.png", "images/bg2.png", "images/apple.png", "images/body.png", "images/head.png", "images/bend.png" ])
          , loop
          ]


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
                    LExtra.init (newHead :: x :: xs) |> withDefault []
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
    Time.second * 0.25


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
    | Resources Resources.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        KeyDown key ->
            ( handleInput key model, Cmd.none )

        Move ->
            ( move model, loop )

        Resources msg ->
            { model | resources = Resources.update msg model.resources } ! []


tabindex : Attribute msg
tabindex =
    attribute "tabindex" "0"


renderFood : Point -> Resources -> Renderable
renderFood food resources =
    Render.sprite
        { position = ( getX food |> toFloat, getY food |> toFloat )
        , size = ( 1.0, 1.0 )
        , texture = Resources.getTexture "images/apple.png" resources
        }


renderSnake : List Point -> Resources -> List Renderable
renderSnake snake resources =
    map
        (\point ->
            Render.sprite
                { position = ( getX point |> toFloat, getY point |> toFloat )
                , size = ( 1.0, 1.0 )
                , texture = Resources.getTexture "images/body.png" resources
                }
        )
        snake


getTileForPoint : Point -> Resources -> Maybe Texture
getTileForPoint (Point y x) resources =
    let
        remainder =
            rem (y * (xMax + 1) + x) 2

        tileName =
            if remainder == 0 then
                "images/bg1.png"
            else
                "images/bg2.png"
    in
        Resources.getTexture tileName resources


renderBackground : Resources -> List Renderable
renderBackground resources =
    let
        coords =
            lift2 (\c1 c2 -> Point c1 c2) (range 0 yMax) (range 0 xMax)
    in
        map
            (\point ->
                Render.sprite
                    { position = ( getX point |> toFloat, getY point |> toFloat )
                    , size = ( 1.0, 1.0 )
                    , texture = getTileForPoint point resources
                    }
            )
            coords


render : Model -> List Renderable
render { snake, food, resources } =
    concat [ renderBackground resources, renderSnake snake resources, [ renderFood food resources ] ]


view : Model -> Html Msg
view ({ screen, camera } as model) =
    div [ tabindex, onKeyDown KeyDown, style [ ( "width", "100%" ), ( "height", "100%" ) ] ]
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
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
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


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch []
