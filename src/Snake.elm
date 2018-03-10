module Snake exposing (..)

import Html exposing (Html, program)
import List exposing (..)
import Random exposing (..)
import List.Extra as LExtra exposing (init, last, lift2)
import Maybe exposing (Maybe, andThen, withDefault)
import Task
import Process
import Time exposing (second)
import Point exposing (..)
import Game.TwoD as Game
import Game.TwoD.Render as Render exposing (Renderable)
import Game.TwoD.Camera as Camera exposing (Camera)
import Game.Resources as Resources exposing (Resources)
import WebGL.Texture as Texture exposing (Texture)
import Debug exposing (..)
import Keyboard.Extra as Keeb exposing (..)


yMax : Int
yMax =
    16


xMax : Int
xMax =
    16


initLen : Int
initLen =
    3


type alias SnakePart =
    { pos : Point, dir : Point, pdir : Point }


type alias Snake =
    ( SnakePart, List SnakePart, SnakePart )


asList : Snake -> List SnakePart
asList ( h, b, t ) =
    h :: b ++ [ t ]


headInRange : Snake -> Bool
headInRange ( h, _, _ ) =
    inRange h.pos yMax xMax


positions : Snake -> List Point
positions snake =
    List.map (\x -> x.pos) <| asList snake


movePart : SnakePart -> Point -> SnakePart
movePart x d =
    { x | pos = add x.pos d, dir = d, pdir = d }


collidedWith : Snake -> Point -> Bool
collidedWith ( h, _, _ ) p =
    p == h.pos


collidedWithSelf : Snake -> Bool
collidedWithSelf ( h, b, t ) =
    let
        bp =
            List.map (\sp -> sp.pos) b
    in
        member h.pos bp || h.pos == t.pos


grow : Snake -> Point -> Snake
grow ( h, b, t ) d =
    ( movePart h d, { h | dir = d } :: b, t )


trim : Snake -> Snake
trim ( h, b, t ) =
    case reverse b of
        [] ->
            ( h, [], t )

        nt :: nb ->
            ( h, reverse nb, nt )


type alias Model =
    { keys : List Key
    , gameStarted : Bool
    , gameOver : Bool
    , dir : Point
    , snake : Snake
    , food : Point
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
            ( { pos = Point initY initHead, dir = right, pdir = right }
            , [ { pos = Point initY (initHead - 1), dir = right, pdir = right } ]
            , { pos = Point initY (initHead - 2), dir = right, pdir = right }
            )
    in
        { keys = []
        , gameStarted = False
        , gameOver = False
        , snake = snake
        , dir = left
        , food = Point initY (initHead + 3)
        , screen = ( xMax * 32, yMax * 32 )
        , camera = Camera.fixedArea (floatY * floatX) ( floatX / 2.0, floatY / 2.0 )
        , resources = Resources.init
        }


init : ( Model, Cmd Msg )
init =
    initState
        ! [ Cmd.map Resources
                (Resources.loadTextures
                    [ "images/bg1.png"
                    , "images/bg2.png"
                    , "images/apple.png"
                    , "images/body.png"
                    , "images/head.png"
                    , "images/bodybend.png"
                    , "images/tail.png"
                    , "images/tailbend.png"
                    ]
                )
          , loop
          ]


reset : Model -> Model
reset state =
    if state.gameOver then
        initState
    else
        state


changeDir : Model -> ( Model, Cmd Msg )
changeDir ({ snake, keys } as state) =
    let
        ( h, _, _ ) =
            snake

        lastDir =
            h.pdir

        mdir =
            pointFromKeys keys
    in
        case mdir of
            Nothing ->
                state ! []

            Just newDir ->
                if add newDir lastDir == Point 0 0 then
                    state ! []
                else
                    { state | dir = newDir, gameStarted = True } ! []


placeFood : Model -> Point -> ( Model, Cmd Msg )
placeFood state p =
    let
        inSnake =
            member p <| positions state.snake
    in
        if inSnake then
            update PlaceFood state
        else
            ( { state | food = p }, Cmd.none )


move : Model -> ( Model, Cmd Msg )
move ({ snake, food, dir } as state) =
    let
        ns =
            grow snake dir
    in
        if collidedWith ns food then
            update PlaceFood { state | snake = ns }
        else if collidedWithSelf ns then
            ( { state | gameOver = True }, Cmd.none )
        else if not (headInRange ns) then
            ( { state | gameOver = True }, Cmd.none )
        else
            ( { state | snake = trim ns }, Cmd.none )


tickDur : Float
tickDur =
    Time.second * 0.175


type Msg
    = NoOp
    | Move
    | PlaceFood
    | UpdateFood ( Int, Int )
    | KeyMsg Keeb.Msg
    | Resources Resources.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Move ->
            if model.gameOver || not model.gameStarted then
                ( model, loop )
            else
                let
                    ( m, cb ) =
                        move model
                in
                    m ! [ cb, loop ]

        PlaceFood ->
            ( model, Random.generate UpdateFood (Random.pair (Random.int 0 (xMax - 1)) (Random.int 0 (yMax - 1))) )

        UpdateFood ( x, y ) ->
            placeFood model <| Point y x

        Resources msg ->
            { model | resources = Resources.update msg model.resources } ! []

        KeyMsg keyMsg ->
            let
                keys =
                    Keeb.update keyMsg model.keys

                _ =
                    log "keys" keys
            in
                changeDir { model | keys = keys }


renderFood : Point -> Resources -> List Renderable
renderFood food resources =
    Render.sprite
        { position = ( getX food |> toFloat, getY food |> toFloat )
        , size = ( 1.0, 1.0 )
        , texture = Resources.getTexture "images/apple.png" resources
        }
        :: []


renderHead : Snake -> Resources -> List Renderable
renderHead ( h, _, _ ) r =
    Render.spriteWithOptions
        { position = ( (getX h.pos |> toFloat) + 0.5, (getY h.pos |> toFloat) + 0.5, 0.0 )
        , size = ( 1.0, 1.0 )
        , tiling = ( 1.0, 1.0 )
        , rotation = getAngle up h.dir
        , pivot = ( 0.5, 0.5 )
        , texture = Resources.getTexture "images/head.png" r
        }
        :: []


renderPart : String -> SnakePart -> Resources -> Renderable
renderPart tname part resources =
    let
        straight =
            part.dir == part.pdir

        ( t, r ) =
            if straight then
                ( "images/" ++ tname ++ ".png", getAngle up part.dir )
            else
                ( "images/" ++ tname ++ "bend.png", getAngle right part.dir )

        flipY =
            if (getAngle part.dir part.pdir) < 0 then
                -1.0
            else
                1.0
    in
        Render.spriteWithOptions
            { position = ( (getX part.pos |> toFloat) + 0.5, (getY part.pos |> toFloat) + 0.5, 0.0 )
            , size = ( 1.0, 1.0 )
            , tiling = ( 1.0, flipY )
            , rotation = r
            , pivot = ( 0.5, 0.5 )
            , texture = Resources.getTexture t resources
            }


renderTail : Snake -> Resources -> List Renderable
renderTail ( _, _, t ) r =
    [ renderPart "tail" t r ]


renderSnake : Snake -> Resources -> List Renderable
renderSnake ( _, b, _ ) resources =
    List.map (\part -> renderPart "body" part resources) b


getTileForCoords : Int -> Int -> Resources -> Maybe Texture
getTileForCoords x y resources =
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
            lift2 (\x y -> ( x, y )) (range 0 xMax) (range 0 yMax)
    in
        List.map
            (\( x, y ) ->
                Render.sprite
                    { position = ( toFloat x, toFloat y )
                    , size = ( 1.0, 1.0 )
                    , texture = getTileForCoords x y resources
                    }
            )
            coords


render : Model -> List Renderable
render { snake, food, resources } =
    List.map (\f -> f resources)
        [ renderBackground
        , renderSnake snake
        , renderHead snake
        , renderTail snake
        , renderFood food
        ]
        |> concat


view : Model -> Html Msg
view ({ screen, camera } as model) =
    Game.render
        { time = 0
        , camera = camera
        , size = screen
        }
        (render model)


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


pointFromKeys : List Key -> Maybe Point
pointFromKeys l =
    case (head l) of
        Just Keeb.CharW ->
            Just up

        Just Keeb.ArrowUp ->
            Just up

        Just Keeb.CharA ->
            Just left

        Just Keeb.ArrowLeft ->
            Just left

        Just Keeb.CharS ->
            Just down

        Just Keeb.ArrowDown ->
            Just down

        Just Keeb.CharD ->
            Just right

        Just Keeb.ArrowRight ->
            Just right

        _ ->
            Nothing


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Sub.map KeyMsg Keeb.subscriptions ]
