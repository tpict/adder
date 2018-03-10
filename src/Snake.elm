module Snake exposing (..)

import Debug exposing (..)
import Game.Resources as Resources exposing (Resources)
import Game.TwoD as Game
import Game.TwoD.Camera as Camera exposing (Camera)
import Game.TwoD.Render as Render exposing (Renderable)
import Html exposing (Html, program)
import Keyboard.Extra as Keeb exposing (..)
import List exposing (..)
import List.Extra as LExtra exposing (lift2, zip)
import Maybe exposing (Maybe)
import Point exposing (..)
import Process
import Random exposing (..)
import Task
import Time exposing (second)
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
    , background : Resources -> List Renderable
    }


initModel : Model
initModel =
    let
        initY =
            yMax // 2

        initX =
            xMax // 2

        snake =
            ( { pos = Point initY initX, dir = right, pdir = right }
            , [ { pos = Point initY (initX - 1), dir = right, pdir = right } ]
            , { pos = Point initY (initX - 2), dir = right, pdir = right }
            )
    in
        { keys = []
        , gameStarted = False
        , gameOver = False
        , snake = snake
        , dir = left
        , food = Point initY (initX + 3)
        , screen = ( xMax * 32, yMax * 32 )
        , camera =
            Camera.fixedArea (toFloat (yMax * xMax))
                ( (toFloat (xMax // 2))
                , (toFloat (yMax // 2))
                )
        , resources = Resources.init
        , background = \_ -> []
        }


loadResources : Cmd Msg
loadResources =
    Cmd.map Resources
        (Resources.loadTextures
            [ "images/bg-l-0.png"
            , "images/bg-l-1.png"
            , "images/bg-l-2.png"
            , "images/bg-l-3.png"
            , "images/bg-d-0.png"
            , "images/bg-d-1.png"
            , "images/bg-d-2.png"
            , "images/bg-d-3.png"
            , "images/apple.png"
            , "images/body.png"
            , "images/bodybend.png"
            , "images/head.png"
            , "images/tail.png"
            , "images/tailbend.png"
            , "images/bodydead.png"
            , "images/bodydeadbend.png"
            , "images/headdead.png"
            , "images/taildead.png"
            , "images/taildeadbend.png"
            , "images/title.png"
            , "images/gameover.png"
            ]
        )


init : ( Model, Cmd Msg )
init =
    let
        model =
            initModel
    in
        model ! [ loadResources, send CreateBackground, loop ]


send : msg -> Cmd msg
send msg =
    Task.succeed msg
        |> Task.perform identity


reset : Model -> ( Model, Cmd Msg )
reset ({ gameOver, resources, background } as model) =
    if gameOver then
        { initModel | resources = resources, background = background } ! []
    else
        model ! []


changeDir : Model -> Point -> ( Model, Cmd Msg )
changeDir ({ snake } as state) newDir =
    let
        ( h, _, _ ) =
            snake

        lastDir =
            h.pdir
    in
        if add newDir lastDir == Point 0 0 then
            state ! []
        else
            { state | dir = newDir, gameStarted = True } ! []


getTileForCoords : Int -> Int -> Int -> Resources -> Maybe Texture
getTileForCoords x y t resources =
    let
        remainder =
            rem (y * (xMax + 1) + x) 2

        prefix =
            if remainder == 0 then
                "images/bg-l-"
            else
                "images/bg-d-"

        tileName =
            prefix ++ toString t ++ ".png"
    in
        Resources.getTexture tileName resources


createBackground : Model -> List ( Int, Int ) -> ( Model, Cmd Msg )
createBackground model l =
    let
        coords =
            lift2 (\x y -> ( x, y )) (range 0 xMax) (range 0 yMax)

        background =
            \resources ->
                List.map
                    (\( ( x, y ), ( r, t ) ) ->
                        Render.spriteWithOptions
                            { position = ( toFloat x + 0.5, toFloat y + 0.5, 0.0 )
                            , pivot = ( 0.5, 0.5 )
                            , tiling = ( 1.0, 1.0 )
                            , rotation = pi / 2 * toFloat r
                            , size = ( 1.0, 1.0 )
                            , texture = getTileForCoords x y t resources
                            }
                    )
                <|
                    LExtra.zip coords l
    in
        { model | background = background } ! []


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
            ( { state | snake = ns, gameOver = True }, Cmd.none )
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
    | CreateBackground
    | ActuallyCreateBackground (List ( Int, Int ))
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
            ( model
            , Random.generate UpdateFood
                (Random.pair
                    (Random.int 0 (xMax - 1))
                    (Random.int 0 (yMax - 1))
                )
            )

        UpdateFood ( x, y ) ->
            placeFood model <| Point y x

        CreateBackground ->
            ( model, Random.generate ActuallyCreateBackground (Random.list ((yMax + 1) * (xMax + 1)) (pair (Random.int 0 3) (Random.int 0 3))) )

        ActuallyCreateBackground l ->
            createBackground model l

        Resources msg ->
            { model | resources = Resources.update msg model.resources } ! []

        KeyMsg keyMsg ->
            handleKeys { model | keys = Keeb.update keyMsg model.keys }


renderFood : Model -> List Renderable
renderFood { food, resources } =
    Render.sprite
        { position = ( getX food |> toFloat, getY food |> toFloat )
        , size = ( 1.0, 1.0 )
        , texture = Resources.getTexture "images/apple.png" resources
        }
        :: []


renderHead : Model -> List Renderable
renderHead { snake, resources, gameOver } =
    let
        ( h, _, _ ) =
            snake

        tname =
            if gameOver then
                "images/headdead.png"
            else
                "images/head.png"
    in
        Render.spriteWithOptions
            { position = ( (getX h.pos |> toFloat) + 0.5, (getY h.pos |> toFloat) + 0.5, 0.0 )
            , size = ( 1.0, 1.0 )
            , tiling = ( 1.0, 1.0 )
            , rotation = getAngle up h.dir
            , pivot = ( 0.5, 0.5 )
            , texture = Resources.getTexture tname resources
            }
            :: []


renderPart : String -> SnakePart -> Model -> Renderable
renderPart tname_ part { resources, gameOver } =
    let
        straight =
            part.dir == part.pdir

        tname =
            if gameOver then
                tname_ ++ "dead"
            else
                tname_

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


renderTail : Model -> List Renderable
renderTail ({ snake } as m) =
    let
        ( _, _, t ) =
            snake
    in
        [ renderPart "tail" t m ]


renderSnake : Model -> List Renderable
renderSnake ({ snake } as m) =
    let
        ( _, b, _ ) =
            snake
    in
        List.map (\part -> renderPart "body" part m) b


renderTitle : Model -> List Renderable
renderTitle { resources } =
    Render.sprite
        { position = ( 0.0, 0.0 )
        , size = ( toFloat yMax, toFloat xMax )
        , texture = Resources.getTexture "images/title.png" resources
        }
        :: []


renderGameOver : Model -> List Renderable
renderGameOver { resources } =
    Render.sprite
        { position = ( 0.0, 0.0 )
        , size = ( toFloat yMax, toFloat xMax )
        , texture = Resources.getTexture "images/gameover.png" resources
        }
        :: []


render : Model -> List Renderable
render ({ gameStarted, gameOver, background, resources } as m) =
    let
        g =
            if gameOver then
                renderGameOver
            else
                \_ -> []

        r =
            if gameStarted then
                [ renderSnake
                , renderTail
                , renderHead
                , renderFood
                , g
                ]
            else
                [ renderTitle ]
    in
        background resources
            :: (List.map (\f -> f m) r)
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


handleKeys : Model -> ( Model, Cmd Msg )
handleKeys ({ keys } as m) =
    case (head keys) of
        Just Keeb.CharW ->
            changeDir m up

        Just Keeb.ArrowUp ->
            changeDir m up

        Just Keeb.CharA ->
            changeDir m left

        Just Keeb.ArrowLeft ->
            changeDir m left

        Just Keeb.CharS ->
            changeDir m down

        Just Keeb.ArrowDown ->
            changeDir m down

        Just Keeb.CharD ->
            changeDir m right

        Just Keeb.ArrowRight ->
            changeDir m right

        Just Keeb.CharR ->
            reset m

        _ ->
            m ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Sub.map KeyMsg Keeb.subscriptions ]
