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

-- helpers
dropLast : List a -> Maybe (List a)
dropLast list =
  case list of 
    [] ->
      Nothing
    [x] ->
      Just [x]
    xs -> 
      Just (List.reverse xs |> (List.drop 1) |> List.reverse)


-- the world
type alias Point = (Int, Int)
type alias Snake = List Point

type alias Model =
  { snake : Snake
  , direction : Point
  , apple: Point
  }


allPoints : List Point
allPoints = List.map2 (,) (List.range 0 boardSize) (List.range 0 boardSize)

-- initial model
init : (Model, Cmd Msg)
init =
  let
    origin = (boardSize//2, boardSize//2)
    direction = (1, 0)
    snake =
      [ origin
      , moveInDirection origin (-1, 0)
      , moveInDirection origin (-2, 0)
      ]
  in
    (
      { snake = snake
      , direction = direction
      , apple = (5, 5)
      }
    , Cmd.none
    )

-- everywhere the snake isn't
allFreeSpace : Model -> List Point
allFreeSpace { snake } =
  let
      notTheSnake : Point -> Bool
      notTheSnake x = not <| List.member x snake
  in
    List.filter notTheSnake allPoints  

-- UPDATE
moveInDirection : Point -> Point -> Point
moveInDirection (x, y) (xV, yV) =
  (x + xV, y + yV)

moveSnake : Model -> Model
moveSnake model =
  let
    head = Maybe.withDefault (0,0) (List.head model.snake)
    tail = Maybe.withDefault [] (dropLast model.snake)
    (goalX, goalY) = moveInDirection head model.direction
    newHead =
      if goalX == boardSize then
        (0, goalY)
      else if goalY == boardSize then
        (goalX, 0)
      else if goalX == 0 then
        (boardSize - 1, goalY)
      else if goalY == 0 then
        (goalX, boardSize - 1)
      else
        (goalX, goalY)

  in
    { model | snake = newHead :: tail }

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

type Msg
  = Tick Time
  | KeyDown KeyCode

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      (moveSnake model, Cmd.none)
    KeyDown code ->
      (newDirection code model ! [])


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
      [ viewSnake model
      , viewApple model
      ]

viewSnake : Model -> Html msg
viewSnake { snake } = 
  div [] (List.indexedMap snakePart snake)

snakePart : Int -> Point -> Html msg
snakePart index (left, top) =
  let
      color = if index == 0 then "blue" else "green"
  in
    div [ style
        [ ( "position", "absolute" )
        , ( "top", (toString <|  top * pixel) ++ "px")
        , ( "left", (toString <| left * pixel) ++ "px")
        , ( "width", toString pixel ++ "px")
        , ( "height", toString pixel ++ "px")
        , ( "background", color)
        ]
      ] []

viewApple : Model -> Html msg
viewApple { apple } =
  let 
      (left, top) = apple
  in
    div [ style
        [ ( "position", "absolute" )
        , ( "top", (toString <|  top * pixel) ++ "px")
        , ( "left", (toString <| left * pixel) ++ "px")
        , ( "width", toString pixel ++ "px")
        , ( "height", toString pixel ++ "px")
        , ( "background", "red")
        ]
  ] []
