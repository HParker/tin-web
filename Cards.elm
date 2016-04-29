module Cards where

import Html exposing (Html, Attribute, text, toElement, div, input, span)
import Signal exposing (Address)
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
  | Move (List Card.Model)


init : Model
init = Model [] 0

add : Card.Model -> Model -> Model
add card model =
  { model |
      cards = card::model.cards,
      nextID = model.nextID + 1
  }

view : Address Action -> Model -> Html
view address model =
  div
    []
    (List.map (Card.view (Signal.forwardTo address Card)) model.cards)


addCards : List Card.Model -> Model -> Model
addCards newCards model =
  { model | cards = model.cards ++ newCards }

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Get command ->
      (model, getCard command)
    Add card ->
      let
        newCards =
          case card of
            Just c -> (c model.nextID) :: model.cards
            Nothing -> model.cards
      in
        ( {model |
            cards = newCards,
            nextID = model.nextID + 1
          }
        , Effects.none
        )
    Card act ->
      case act of
        Card.Delete cardID ->
          let
            newCards =
              List.filter (\c -> c.id /= cardID) model.cards
          in
            ({model | cards = newCards}, Effects.none)
        Card.Move cardID ->
          let
            newCards =
              List.filter (\c -> c.id /= cardID) model.cards
            movingCards =
              List.filter (\c -> c.id == cardID) model.cards
            fx =
              Effects.task <| Task.succeed <| Move movingCards
          in
            ({model | cards = newCards}, fx)
        _ ->
          let
            (newCards, fxs) = List.unzip (List.map (Card.update act) model.cards)
          in
            ({model | cards = newCards}, (Effects.map Card (Effects.batch fxs)))
    Move _ ->
      (model, Effects.none)
    NoOp ->
      (model, Effects.none)


getCard : String -> Effects Action
getCard command =
  Http.get Card.decode ("?q=" ++ command)
    |> Task.toMaybe
    |> Task.map Add
    |> Effects.task
