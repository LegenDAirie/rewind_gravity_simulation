module App exposing (..)

import Html exposing (Html, div)
import Color exposing (rgb)
import Collage exposing (collage, oval, filled, move)
import Element exposing (container, middle, toHtml)
import AnimationFrame
import Window
import Task
import Math.Vector2 exposing (Vec2, vec2, getX, getY)


type alias Ball =
    { velocity : Vec2
    , position : Vec2
    }


type alias Model =
    { ball : Ball
    , windowSize : Window.Size
    }


defaultBall : Ball
defaultBall =
    { velocity = vec2 0 0
    , position = vec2 0 0
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
    { model | ball = ballStepHelper model.ball dt }


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
        { ball, windowSize } =
            model

        { width, height } =
            windowSize
    in
        toHtml <|
            collage width
                height
                [ oval 50 50
                    |> filled (rgb 60 100 60)
                    |> move ( getX ball.position, getY ball.position )
                ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Window.resizes (\{ width, height } -> Resize width height)
        , AnimationFrame.diffs Tick
        ]
