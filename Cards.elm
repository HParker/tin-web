module Cards where

import Html exposing (Html, Attribute, text, toElement, div, input, span)
import Signal exposing (Address)
import Json.Decode as Json exposing ((:=))
import Http
import Task
import Effects exposing (Effects)
import Card


type alias ID = Int


type alias Model =
  { cards : List Card.Model
  , nextID : ID
  }


type Action
  = NoOp
  | Get String
  | Add (Maybe (Card.ID -> Card.Model))
  | Card Card.Action


init : Model
init = Model [] 0


view : Address Action -> Model -> Html
view address model =
  div
    []
    (List.map (Card.view (Signal.forwardTo address Card)) model.cards)


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Get command ->
      (model, getCard command)
    Add card ->
      let
        newCards = case card of
                  Just c -> (c model.nextID) :: model.cards
                  Nothing -> model.cards
      in
        ({model |
            cards = newCards,
            nextID = model.nextID + 1
         },
           Effects.none
        )
    Card act -> -- TODO: forward events to correct card
      let
        (cards, fxs) = List.unzip (List.map (Card.update act) model.cards)
      in
      ({model | cards = cards}, (Effects.map Card (Effects.batch fxs)))
    NoOp ->
      (model, Effects.none)


getCard : String -> Effects Action
getCard command =
  Http.get decodeCard ("?q=" ++ command)
    |> Task.toMaybe
    |> Task.map Add
    |> Effects.task


decodeCard : Json.Decoder (Card.ID -> Card.Model)
decodeCard =
  Json.object2 (\title body -> Card.build title body)
    ("title" := Json.string)
    ("body" := Json.string)
