module CounterList where

import Counter
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- Model
type alias Model =
  { counters : List ( ID, Counter.Model )
  , nextID : ID
  }

type alias ID = Int

init : Model
init =
  { counters = []
  , nextID = 0
  }


-- Update
type Action
  = Insert
  | Remove
  | Modify ID Counter.Action

update : Action -> Model -> Model
update action model =
  case action of
    Insert ->
      let
        newCounter = ( model.nextID, Counter.init 0 )
        newCounters = model.counters ++ [ newCounter ]
      in
        { model |
            counters = newCounters,
            nextID = model.nextID + 1
        }

    Remove ->
      { model | counters = List.drop 1 model.counters }

    Modify id counterAction ->
      let
        updateCounter (counterID, counterModel) =
          if counterID == id then
            (counterID, Counter.update counterAction counterModel)
          else
            (counterID, counterModel)
      in
        { model | counters = List.map updateCounter model.counters }


-- View
view : Signal.Address Action -> Model -> Html
view address model =
  let
    insert = button [ onClick address Insert ] [ text "Add" ]
    remove = button [ onClick address Remove ] [ text "Remove" ]
    counters = List.map (viewCounter address) model.counters
  in
    div [ containerStyle ] ([insert, remove] ++ counters)

viewCounter : Signal.Address Action -> (ID, Counter.Model) -> Html
viewCounter address (id, model) =
  Counter.view (Signal.forwardTo address (Modify id)) model

containerStyle : Attribute
containerStyle =
  style
    [ ("padding", "10px")
    ]
