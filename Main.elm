import Html exposing (Html, Attribute)
import Html.App
import Html exposing (Html, Attribute, text, div, input, span)
import Html.Attributes exposing (id, class)
import Html.App as Html
import Keyboard

import Cards
import Card
import Input

type alias Model =
  { input : Input.Model
  , cards : Cards.Model
  , pins : Cards.Model
  , histories : Cards.Model
  }

type Msg
  = Input Input.Msg
  | Cards Cards.Msg
  | Pins Cards.Msg
  | History Cards.Msg
  | HandleKeypress Int

init : (Model, Cmd Msg)
init =
  let
    (input, fx) = Input.init
  in
    (Model input Cards.init Cards.init Cards.init, Cmd.map Input fx)


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    HandleKeypress keyCode ->
      if keyCode == 13 then
        makeRequest model
      else
        (model, Cmd.none)
    Input msg ->
      let
        (input, fx) = Input.update msg model.input
      in
        ({ model | input = input}, Cmd.map Input fx)
    Cards act ->
      case act of
        Cards.Move newCards ->
          ({ model | pins = Cards.addCards newCards model.pins }, Cmd.none)
        _ ->
          let
            (cards, fx) = Cards.update act model.cards
          in
            ({ model | cards = cards}, Cmd.map Cards fx)
    Pins act ->
      case act of
        Cards.Move newCards ->
          ({ model | cards = Cards.addCards newCards model.cards }, Cmd.none)
        _ ->
          let
            (cards, fx) = Cards.update act model.pins
          in
            ({ model | pins = cards}, Cmd.map Pins fx)
    History act ->
      let
        (cards, fx) = Cards.update act model.pins
      in
        ({ model | histories = cards}, Cmd.map History fx)




view : Model -> Html Msg
view model =
  div
    [class "center"]
    [ div
        [class "left"]
        [ span
            [ class "header-icon icon mega-octicon octicon-book" ]
            []
        , div
            [class "contents histories"]
            [Html.App.map History (Cards.view model.histories)]
        ]
    , div
        [class "right"]
        [ span
            [ class "header-icon icon mega-octicon octicon-pin" ]
            []
        , div
            [class "contents pins"]
            [Html.App.map Pins (Cards.view model.pins)]
        ]
    , div
        [id "app"]
        [ Html.App.map Input (Input.view model.input)
        , Html.App.map Cards (Cards.view model.cards)
        ]
    ]

makeRequest : Model -> (Model, Cmd Msg)
makeRequest model =
  let
    newInput = Input.storeCommand "" model.input
    newHistories = Cards.add (Card.build "" model.input.command 0) model.histories
    (cards, fx) = Cards.update (Cards.Get model.input.command) model.cards
  in
    ({ model |
         input = newInput,
         cards = cards,
         histories = newHistories
     }
    , Cmd.map Cards fx)

main : Program Never
main =
  Html.program
    { init = init, update = update, view = view, subscriptions = \_ -> Keyboard.presses HandleKeypress }
