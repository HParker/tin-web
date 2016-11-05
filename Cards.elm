module Cards exposing (..)

import Html exposing (Html, Attribute, text, div, input, span)
import Http
import Task
import Card
import Html.App

type alias ID = Int


type alias Model =
  { cards : List Card.Model
  , nextID : ID
  }


type Msg
  = NoOp
  | Get String
  | Add (Card.ID -> Card.Model)
  | AddFailed Http.Error
  | Card Card.Msg


init : Model
init = Model [] 0

add : Card.Model -> Model -> Model
add card model =
  { model |
      cards = card::model.cards,
      nextID = model.nextID + 1
  }

view : Model -> Html Msg
view model =
  div
    []
    (List.map viewCard model.cards)

viewCard : Card.Model -> Html Msg
viewCard card =
  Html.App.map Card (Card.view card)


addCards : List Card.Model -> Model -> Model
addCards newCards model =
  { model | cards = model.cards ++ newCards }

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    Get command ->
      (model, getCard command)
    Add card ->
      let
        newCards =
          (card model.nextID) :: model.cards
      in
        ( {model |
            cards = newCards,
            nextID = model.nextID + 1
          }
        , Cmd.none
        )
    Card act ->
      case act of
        Card.Delete cardID ->
          let
            newCards =
              List.filter (\c -> c.id /= cardID) model.cards
          in
            ({model | cards = newCards}, Cmd.none)
        _ ->
          let
            (newCards, fxs) = List.unzip (List.map (Card.update act) model.cards)
          in
            ({model | cards = newCards}, (Cmd.map Card (Cmd.batch fxs)))
    AddFailed _ ->
      (model, Cmd.none)
    NoOp ->
      (model, Cmd.none)

getCard : String -> Cmd Msg
getCard command =
  Http.get Card.decode ("api/?q=" ++ command)
    |> Task.perform AddFailed Add
