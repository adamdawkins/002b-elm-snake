import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Debug exposing (..)
import Time exposing (Time, second)

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

-- initial model

init : (Model, Cmd Msg)
init =
  let
    origin = boardSize//2
    body = origin - 1
  in
    (
      { snake =
          [ (origin, origin)
          , (origin, body)
          , (origin, body - 1)
          ]
      }
    , Cmd.none
    )

type alias Snake = List (Int, Int)

type alias Model =
  { snake : Snake
  }

-- UPDATE
type Msg
  = Tick Time

moveSnake : Model -> Model
moveSnake model =
  let
    newHead = case List.head model.snake of
      Nothing ->
        (0,0)
      Just (x, y) ->
        if x == boardSize - 1 then
          (0, y)
        else
          (x + 1, y)
    tail = Maybe.withDefault [] (dropLast model.snake)
  in
    { model | snake = newHead :: tail }

  
          

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick newTime ->
      (moveSnake model, Cmd.none)


-- SUBSCRIPTIONS
subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every second Tick


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
      [ viewSnake model ]

viewSnake : Model -> Html msg
viewSnake { snake } = 
  div [] (List.indexedMap snakePart snake)

snakePart : Int -> (Int, Int) -> Html msg
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
