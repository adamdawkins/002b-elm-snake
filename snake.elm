import Set exposing (..)
import Array exposing (..)
import Random exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Debug exposing (log)
import Time exposing (Time, second)
import Keyboard exposing (downs, KeyCode)
import Json.Decode as Json

main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

-- config
pixel = 20
boardSize = 20

-- geometry
distance : Point -> Point -> Float
distance (x1, y1) (x2, y2) =
  sqrt((toFloat x2 - toFloat x1)^2 + (toFloat y2 - toFloat y1)^2)

addPoints : Point -> Point -> Point
addPoints (x, y) (xV, yV) =
  (x + xV, y + yV)

willCollide : Point -> Point -> Bool
willCollide p1 p2 =
  distance p1 p2 == 0


-- helpers
dropLast : List a -> List a
dropLast list =
  case list of 
    [] ->
      []
    [x] ->
      []
    xs -> 
      List.reverse xs |> (List.drop 1) |> List.reverse


-- the world
type alias Point = (Int, Int)

type alias Model =
  { snake :
    { head : Point
    , tail : List Point
    }
  , direction : Point
  , apple: Point
  }


init : (Model, Cmd Msg)
init =
  let
    origin = (boardSize//2, boardSize//2)
  in
    (
      { snake =
        { head = origin
        , tail =
            [ addPoints origin (-1, 0)
            , addPoints origin (-2, 0)
            ]
        }
      , direction = (1, 0)
      , apple = (5, 5)
      }
    , Cmd.none
    )

-- UPDATE

-- handles the new position of the snake head, accounting for flipping to the other side of board
newSnakeHead : Model -> Point
newSnakeHead { snake, direction } =
  let 
    (goalX, goalY) = addPoints snake.head direction
  in 
    if goalX == boardSize then
      (0, goalY)
    else if goalY == boardSize then
      (goalX, 0)
    else if goalX == -1 then
      (boardSize - 1, goalY)
    else if goalY == -1 then
      (goalX, boardSize - 1)
    else
      (goalX, goalY)

newDirection : Int -> Model -> Model 
newDirection keyCode model =
  case keyCode of
    37 ->
      -- log "left"
      { model | direction = (-1, 0) }
    38 ->
      -- log "up"
      { model | direction = (0, -1) }
    39 ->
      -- log "right"
      { model | direction = (1, 0) }
    40 ->
      -- log "down"
      { model | direction = (0, 1) }
    _ ->
      -- ignore all other keys
      model


randomPoint: Random.Generator Point
randomPoint =
  Random.pair (Random.int 0 (boardSize - 1)) (Random.int 0 (boardSize - 1)) 

  
type Msg
  = Tick Time
  | KeyDown KeyCode
  | MoveApple Point


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      let
          newHead = newSnakeHead model
          cmd = 
            if willCollide model.apple newHead then
              Random.generate MoveApple randomPoint
            else
              Cmd.none
          newTail = 
            if willCollide model.apple newHead then
              model.snake.head :: model.snake.tail
            else
              model.snake.head :: dropLast model.snake.tail
      in
        ({ model | snake = { head = newHead, tail = newTail } }, cmd)
    KeyDown code ->
      (newDirection code model ! [])
    MoveApple point ->
      ({ model | apple = point }, Cmd.none)


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Time.every (150 * Time.millisecond) Tick
        , downs KeyDown
        ]

-- VIEW 
view model =
  let
      width = boardSize * pixel
      height = width
  in
      div [ style
            [ ("position", "relative")
            , ("height", toString height ++ "px")
            , ("width",  toString width ++ "px")
            , ("background", "#ccc")
            ]
        ]
        ( dot "blue" model.snake.head
          :: dot "red" model.apple
          :: List.map (dot "green") model.snake.tail
        )

dot : String -> Point -> Html msg
dot color (left, top) =
  div [ style
    [ ( "position", "absolute" )
    , ( "top", (toString <|  top * pixel) ++ "px")
    , ( "left", (toString <| left * pixel) ++ "px")
    , ( "width", toString pixel ++ "px")
    , ( "height", toString pixel ++ "px")
    , ( "background", color)
    ]
  ] []
