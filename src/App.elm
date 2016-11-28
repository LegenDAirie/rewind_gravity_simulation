module App exposing (..)

import Html exposing (Html, div)


-- import Html.Attributes
-- import Collage exposing (collage, oval, filled)
-- import Element exposing (container, middle, toHtml)

import Window
import Task


type alias Ball =
    { velocity : List Float
    , position : List Float
    }


type alias WindowSize =
    { width : Int
    , height : Int
    }


type alias Model =
    { ball : Ball
    , windowSize : WindowSize
    }


defaultBall : Ball
defaultBall =
    { velocity = [ 0, 0 ]
    , position = [ 20, 20 ]
    }


defaultWindow : WindowSize
defaultWindow =
    { width = 0
    , height = 0
    }


defaultModel : Model
defaultModel =
    { ball = defaultBall
    , windowSize = defaultWindow
    }


init : ( Model, Cmd Msg )
init =
    ( defaultModel, Cmd.none )


type Msg
    = Resize Int Int
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Resize newHeight newWidth ->
            let
                newWindowSize =
                    { width = newWidth, height = newHeight }
            in
                ( { model | windowSize = newWindowSize }
                , Cmd.none
                )

        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ Html.text (toString model.windowSize.width)
        , Html.text " "
        , Html.text (toString model.windowSize.height)
        ]



-- let
--     { ball, windowSize } =
--         model
--
--     { width, height } =
--         windowSize
-- in
--     toHtml <|
--         container width height middle <|
--             collage 400
--                 400
--                 [ oval 100 100
--                     |> filled (rgb 60 100 60)
--                 ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Window.resizes (\{ height, width } -> Resize height width)
