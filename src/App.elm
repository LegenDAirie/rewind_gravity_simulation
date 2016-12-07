module App exposing (..)

import Html exposing (Html, div)
import Color exposing (rgb)
import Collage exposing (collage, oval, filled, move)
import Element exposing (container, middle, toHtml)
import Math.Vector2 exposing (Vec2, vec2, getX, getY, add, toTuple)
import Random exposing (Generator)
import AnimationFrame
import Window
import Task


type alias Ball =
    { velocity : Vec2
    , location : Vec2
    }


type alias Model =
    { balls : List Ball
    , windowSize : Window.Size
    }


defaultBall : Vec2 -> Ball
defaultBall vec =
    { velocity = vec2 0 0
    , location = vec
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
    , Random.generate CreateBalls vectorGenerator
    ]


createBalls : List Vec2 -> List Ball
createBalls vectors =
    List.map (\vector -> defaultBall vector) vectors


vectorGenerator : Generator (List Vec2)
vectorGenerator =
    Random.list 3 <|
        Random.map (\( x, y ) -> vec2 x y) (Random.pair (Random.float -500 500) (Random.float 500 0))


init : ( Model, Cmd Msg )
init =
    ( defaultModel, Cmd.batch initialSizeCmd )


type Msg
    = Resize Int Int
    | Tick Float
    | CreateBalls (List Vec2)


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
            { model | balls = step dt model.balls }

        CreateBalls vectors ->
            { model | balls = createBalls vectors }


step : Float -> List Ball -> List Ball
step dt balls =
    List.map (\ball -> applyForces dt ball) balls


applyForces : Float -> Ball -> Ball
applyForces dt ball =
    ball
        |> applyGravity dt
        |> applyFloor


applyGravity : Float -> Ball -> Ball
applyGravity dt ball =
    let
        { location, velocity } =
            ball

        gravitationalAcceleration =
            vec2 0 -0.01

        newVelocity =
            add velocity gravitationalAcceleration
    in
        { ball
            | location = add location newVelocity
            , velocity = newVelocity
        }


applyFloor : Ball -> Ball
applyFloor ball =
    let
        currentX =
            getX ball.location

        currentY =
            getY ball.location

        newY =
            if currentY < -200 then
                -200
            else
                currentY

        newVelocity =
            if currentY < -200 then
                vec2 (getX ball.velocity) -(getY ball.velocity)
            else
                ball.velocity
    in
        { ball
            | location = vec2 currentX newY
            , velocity = newVelocity
        }


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
                            |> move (toTuple ball.location)
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
