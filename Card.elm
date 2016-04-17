module Card where

import Html exposing (Html, Attribute, text, toElement, div, input, span)
import Html.Attributes exposing (id, class)
import Signal exposing (Address)
import Json.Decode as Json exposing ((:=))
import Http
import Task
import Effects exposing (Effects)
import Markdown
import Html.Events


type alias Card =
  { title : String
  , body : String
  , collapsed : Bool
  }

type alias ID = Int

type alias Model =
  { cards : List (ID, Card)
  , nextID : ID
  }



type Action
  = NoOp
  | Get String
  | Add (Maybe Card)
  | Collapse ID


initCard : String -> String -> Card
initCard title body =
  Card title body False


init : Model
init = Model [] 0

show : Signal.Address Action -> (ID, Card) -> Html
show address cardTuple =
  let
    cardID = fst cardTuple
    card = snd cardTuple
    cardBody = if card.collapsed then
                 ""
               else
                 card.body
  in
  div [class "card"]
    [ span [class "title"] [text card.title]
    , span
        [ class "icon octicon octicon-pin"

        ] []
    , span [ class "icon octicon octicon-chevron-down"
           , Html.Events.onClick address (Collapse cardID)
           ] []
    , Markdown.toHtml cardBody
    ]

view : Address Action -> Model -> Html
view address model =
  div
    []
    (List.map (show address) model.cards)


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Get command ->
      (model, getCard command)
    Add card ->
      let
        newCards = case card of
                  Just c -> (model.nextID, c) :: model.cards
                  Nothing -> model.cards
      in
        ({model |
            cards = newCards,
            nextID = model.nextID + 1
         },
           Effects.none
        )
    Collapse id ->
      let
        updateCard (cardID, card) =
          if id == cardID then
            (cardID, { card | collapsed = True })
          else
            (cardID, card)
      in
        ({ model | cards = List.map updateCard model.cards }, Effects.none)
    NoOp ->
      (model, Effects.none)

getCard : String -> Effects Action
getCard command =
  Http.get decodeCard ("?q=" ++ command)
    |> Task.toMaybe
    |> Task.map Add
    |> Effects.task

decodeCard : Json.Decoder Card
decodeCard =
  Json.object2 (\title body -> initCard title body)
    ("title" := Json.string)
    ("body" := Json.string)
