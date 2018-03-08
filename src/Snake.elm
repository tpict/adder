module Snake exposing (..)

import Html exposing (Attribute, Html, program, div, input, text)
import Html.Attributes exposing (attribute, property, style)
import Html.Events exposing (on, keyCode, onInput)
import Json.Encode exposing (string)
import Json.Decode as Decode
import Json.Encode as Encode
import List exposing (all, concat, drop, foldl, foldr, head, length, map, maximum, member, range, repeat, tail)
import List.Extra as LExtra exposing (groupWhile, init, lift2, last)
import Maybe exposing (Maybe, withDefault, map)
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
    { snake : List DirectedPoint
    , food : Point
    , dir : Point
    , screen : ( Int, Int )
    , camera : Camera
    , resources : Resources
    }


trim : List DirectedPoint -> List DirectedPoint
trim xs =
    let
        nohead =
            tail xs |> withDefault []
    in
        LExtra.init nohead |> withDefault []


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
            List.map (\x -> DirectedPoint (Point initY x) left) <| range initHead initX
    in
        placeFood <|
            { snake = snake
            , food = Point 0 0
            , dir = Point 0 0
            , screen = ( xMax * 32, yMax * 32 )
            , camera = Camera.fixedArea (floatY * floatX) ( floatX / 2.0, floatY / 2.0 )
            , resources = Resources.init
            }


init : ( Model, Cmd Msg )
init =
    initState
        ! [ Cmd.map Resources (Resources.loadTextures [ "images/bg1.png", "images/bg2.png", "images/apple.png", "images/body.png", "images/head.png", "images/bend.png", "images/tail.png" ])
          , loop
          ]


reset : Model -> Model
reset state =
    case state.snake of
        [] ->
            initState

        _ ->
            state


lastDir : Model -> Point
lastDir { snake } =
    head snake |> Maybe.map getDirection |> withDefault origin


changeDir : Point -> Model -> Model
changeDir newDir state =
    if (lastDir state |> add newDir) == Point 0 0 then
        state
    else
        { state | dir = newDir }


getSnakePos : Model -> List Point
getSnakePos { snake } =
    List.map (\dpoint -> getPosition dpoint) snake


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
            member p <| getSnakePos state
    in
        if inSnake then
            placeFood state
        else
            { state | food = p }


move : Model -> Model
move ({ snake, food, dir } as state) =
    case ( snake, food, dir ) of
        ( _, _, Point 0 0 ) ->
            state

        ( [], _, _ ) ->
            state

        ( x :: xs, food, dir ) ->
            let
                lastHeadPos =
                    getPosition x

                newHeadPos =
                    add dir lastHeadPos

                newHead =
                    DirectedPoint newHeadPos dir

                newSnake =
                    LExtra.init (newHead :: x :: xs) |> withDefault []

                lastBodyPos =
                    List.map getPosition (x :: xs)
            in
                if not (inRange newHeadPos yMax xMax) then
                    -- collided with wall
                    { state | snake = [] }
                else if member newHeadPos lastBodyPos then
                    -- collided with self
                    { state | snake = [] }
                else if newHeadPos == food then
                    -- eating food
                    placeFood <|
                        { state
                            | snake = newHead :: x :: xs
                        }
                else
                    -- regular movement
                    { state | snake = newSnake }


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


renderFood : Point -> Resources -> List Renderable
renderFood food resources =
    Render.sprite
        { position = ( getX food |> toFloat, getY food |> toFloat )
        , size = ( 1.0, 1.0 )
        , texture = Resources.getTexture "images/apple.png" resources
        }
        :: []


renderSnakeBends : List DirectedPoint -> Resources -> List Renderable
renderSnakeBends snake resources =
    List.map
        (\point ->
            let
                pos =
                    getPosition point
            in
                Render.spriteWithOptions
                    { position = ( (getX pos |> toFloat) + 0.5, (getY pos |> toFloat) + 0.5, 0.0 )
                    , size = ( 1.0, 1.0 )
                    , pivot = ( 0.5, 0.5 )
                    , tiling = ( 1, 1 )
                    , rotation = getAngle (getDirection point)
                    , texture = Resources.getTexture "images/bend.png" resources
                    }
        )
    <|
        withDefault [] <| tail snake

renderSnakeStraights : List DirectedPoint -> Resources -> List Renderable
renderSnakeStraights snake resources =
    List.map
        (\point ->
            let
                pos =
                    getPosition point
            in
                Render.spriteWithOptions
                    { position = ( (getX pos |> toFloat) + 0.5, (getY pos |> toFloat) + 0.5, 0.0 )
                    , size = ( 1.0, 1.0 )
                    , pivot = ( 0.5, 0.5 )
                    , tiling = ( 1, 1 )
                    , rotation = getAngle (getDirection point)
                    , texture = Resources.getTexture "images/body.png" resources
                    }
        )
    <|
        withDefault [] <| LExtra.init snake


renderSnake : List DirectedPoint -> Resources -> List Renderable
renderSnake snake resources =
    let
        grouped =
            groupWhile (\x y -> (getDirection x) == (getDirection y)) snake

        trimmedGroups = foldr (\dp acc -> (withDefault [] <| tail dp) :: acc) [] grouped

        straights = concat trimmedGroups

        tgrouped =
            groupWhile (\x y -> (getDirection x) == (getDirection y)) snake

        bends = foldr (\dp acc -> (withDefault (DirectedPoint origin origin) <| head dp) :: acc) [] tgrouped

        --     List.map (\dp -> tail dp |> withDefault []) grouped |> concat
    in
        concat [renderSnakeStraights straights resources, renderSnakeBends bends resources]


renderTail : List DirectedPoint -> Resources -> List Renderable
renderTail snake resources =
    let
        snakeTail =
            withDefault (DirectedPoint origin origin) <| last snake

        pos =
            getPosition snakeTail
    in
        Render.spriteWithOptions
            { position = ( (getX pos |> toFloat) + 0.5, (getY pos |> toFloat) + 0.5, 0.0 )
            , size = ( 1.0, 1.0 )
            , pivot = ( 0.5, 0.5 )
            , tiling = ( 1, 1 )
            , rotation = getAngle (getDirection snakeTail)
            , texture = Resources.getTexture "images/tail.png" resources
            }
            :: []


renderHead : List DirectedPoint -> Resources -> List Renderable
renderHead snake resources =
    let
        snakeHead =
            withDefault (DirectedPoint origin origin) <| head snake

        pos =
            getPosition snakeHead
    in
        Render.spriteWithOptions
            { position = ( (getX pos |> toFloat) + 0.5, (getY pos |> toFloat) + 0.5, 0.0 )
            , size = ( 1.0, 1.0 )
            , pivot = ( 0.5, 0.5 )
            , tiling = ( 1, 1 )
            , rotation = getAngle (getDirection snakeHead)
            , texture = Resources.getTexture "images/head.png" resources
            }
            :: []


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
        List.map
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
    List.map (\r -> r resources)
        [ renderBackground
        , renderSnake snake
        , renderTail snake
        , renderHead snake
        , renderFood food
        ]
        |> concat


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
