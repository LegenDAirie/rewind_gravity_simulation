module App exposing (..)

import Html exposing (Html, div)
import Color exposing (rgb)
import Collage exposing (collage, oval, filled)
import Element exposing (container, middle, toHtml)
import AnimationFrame
import Window
import Task


type alias Ball =
    { velocity : { vx : Float, vy : Float }
    , position : { x : Float, y : Float }
    }


type alias Model =
    { ball : Ball
    , windowSize : Window.Size
    }


defaultBall : Ball
defaultBall =
    { velocity = { vx = 0, vy = 0 }
    , position = { x = 0, y = 0 }
    }


defaultWindow : Window.Size
defaultWindow =
    { width = 0
    , height = 0
    }


defaultModel : Model
defaultModel =
    { ball = defaultBall
    , windowSize = defaultWindow
    }


initialSizeCmd : Cmd Msg
initialSizeCmd =
    Task.perform (\{ width, height } -> Resize width height) Window.size


init : ( Model, Cmd Msg )
init =
    ( defaultModel, initialSizeCmd )


type Msg
    = Resize Int Int
    | Tick Float


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


step : Float -> Model -> Model
step dt model =
    model


view : Model -> Html Msg
view model =
    let
        { ball, windowSize } =
            model

        { width, height } =
            windowSize
    in
        toHtml <|
            collage width
                height
                [ oval ball.position.x ball.position.y
                    |> filled (rgb 60 100 60)
                ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes (\{ width, height } -> Resize width height)
        , AnimationFrame.diffs Tick
        ]
