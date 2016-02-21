module Counter (Model, init, Action, update, view) where

import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

-- Model
type alias Model = Int

init : Int -> Model
init count = count


-- Update
type Action = Increment | Decrement | Double

update : Action -> Model -> Model
update action model =
  case action of
    Increment -> model + 1
    Decrement -> model - 1
    Double    -> model * 2


-- View
view : Signal.Address Action -> Model -> Html
view address model =
  div [ containerStyle ]
    [ button [ onClick address Decrement ] [ text "-" ]
    , div [ countStyle ] [ text (toString model) ]
    , button [ onClick address Increment ] [ text "+" ]
    , button [ onClick address Double ] [ text "*2" ]
    ]

countStyle : Attribute
countStyle =
  style
    [ ("font-size", "20px")
    , ("font-family", "monospace")
    , ("display", "inline-block")
    , ("width", "50px")
    , ("text-align", "center")
    ]

containerStyle : Attribute
containerStyle =
  style
    [ ("width", "100px")
    , ("margin", "20px 0")
    , ("text-align", "center")
    ]
