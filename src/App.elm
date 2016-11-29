module App exposing (..)

import Html exposing (Html, div)
import Color exposing (rgb)
import Collage exposing (collage, oval, filled, move)
import Element exposing (container, middle, toHtml)
import Math.Vector2 exposing (Vec2, vec2, getX, getY)
import Random exposing (..)
import AnimationFrame
import Window
import Task


type alias Ball =
    { velocity : Vec2
    , position : Vec2
    }


type alias Model =
    { balls : List Ball
    , windowSize : Window.Size
    }


defaultBall : Float -> Float -> Ball
defaultBall x y =
    { velocity = vec2 0 0
    , position = vec2 x y
    }


defaultWindow : Window.Size
defaultWindow =
    { width = 0
    , height = 0
    }


defaultModel : Model
defaultModel =
    { balls = []
    , windowSize = defaultWindow
    }


initialSizeCmd : List (Cmd Msg)
initialSizeCmd =
    [ Task.perform (\{ width, height } -> Resize width height) Window.size
    , Random.generate GetRandomVectors vectorGenerator
    ]


vectorGenerator : Generator (List Vec2)
vectorGenerator =
    Random.list 3 <|
        Random.map (\( x, y ) -> vec2 x y) (Random.pair (Random.float -500 0) (Random.float -500 0))


init : ( Model, Cmd Msg )
init =
    ( defaultModel, initialSizeCmd |> Cmd.batch )


type Msg
    = Resize Int Int
    | Tick Float
    | GetRandomVectors (List Vec2)


update : Msg -> Model -> Model
update msg model =
    case msg of
        Resize newWidth newHeight ->
            let
                newWindowSize =
                    { width = newWidth, height = newHeight }
            in
                { model | windowSize = newWindowSize }

        Tick dt ->
            step dt model

        GetRandomVectors vectors ->
            { model | balls = createBalls vectors }


createBalls : List Vec2 -> List Ball
createBalls vectors =
    List.map (\vector -> defaultBall (getX vector) (getY vector)) vectors



-- Debug.log (toString vectors)
--     [ defaultBall -500 -500 ]


step : Float -> Model -> Model
step dt model =
    { model | balls = List.map (\ball -> ballStepHelper ball dt) model.balls }


ballStepHelper : Ball -> Float -> Ball
ballStepHelper ball dt =
    let
        currentX =
            getX ball.position

        currentY =
            getY ball.position

        newX =
            currentX + 1

        newY =
            currentY + 1
    in
        { ball | position = vec2 newX newY }


view : Model -> Html Msg
view model =
    let
        { balls, windowSize } =
            model

        { width, height } =
            windowSize
    in
        toHtml <|
            collage width
                height
                (List.map
                    (\ball ->
                        (oval 50 50
                            |> filled (rgb 60 100 60)
                            |> move ( getX ball.position, getY ball.position )
                        )
                    )
                    balls
                )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes (\{ width, height } -> Resize width height)
        , AnimationFrame.diffs Tick
        ]
