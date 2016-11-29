module App exposing (..)

import Html exposing (Html, div)
import Color exposing (rgb)
import Collage exposing (collage, oval, filled)
import Element exposing (container, middle, toHtml)
import Window
import Task


type alias Ball =
    { velocity : List Float
    , position : List Float
    }


type alias Model =
    { ball : Ball
    , windowSize : Window.Size
    }


defaultBall : Ball
defaultBall =
    { velocity = [ 0, 0 ]
    , position = [ 20, 20 ]
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


update : Msg -> Model -> Model
update msg model =
    case msg of
        Resize newWidth newHeight ->
            let
                newWindowSize =
                    { width = newWidth, height = newHeight }
            in
                { model | windowSize = newWindowSize }


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
                [ oval 100 100
                    |> filled (rgb 60 100 60)
                ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes (\{ width, height } -> Resize width height)
