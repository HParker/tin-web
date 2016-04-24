import Html exposing (Html, Attribute)
import StartApp
import Effects exposing (Effects, Never)
import Task
import Html exposing (Html, Attribute, text, toElement, div, input)
import Html.Attributes exposing (id, class)
import Keyboard
import AutoComplete

import Cards
import Input


type alias Model =
  { input : Input.Model
  , cards : Cards.Model
  , pins : Cards.Model
  }

type Action
  = Input Input.Action
  | Cards Cards.Action
  | Pins Cards.Action

init : (Model, Effects Action)
init =
  let
    (input, fx) = Input.init
  in
    (Model input Cards.init Cards.init, Effects.map Input fx)


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Input msg ->
      case msg of
        Input.Request command ->
          let
            newInput = Input.storeCommand "" model.input
            (cards, fx) = Cards.update (Cards.Get command) model.cards
          in
            ({ model | input = newInput, cards = cards}, Effects.map Cards fx)
        _ ->
          let
            (input, fx) = Input.update msg model.input
          in
            ({ model | input = input}, Effects.map Input fx)
    Cards act ->
      case act of
        Cards.Move newCards ->
          ({ model | pins = Cards.addCards newCards model.pins }, Effects.none)
        _ ->
          let
            (cards, fx) = Cards.update act model.cards
          in
            ({ model | cards = cards}, Effects.map Cards fx)
    Pins act ->
      case act of
        Cards.Move newCards ->
          ({ model | cards = Cards.addCards newCards model.cards }, Effects.none)
        _ ->
          let
            (cards, fx) = Cards.update act model.pins
          in
            ({ model | pins = cards}, Effects.map Pins fx)



view : Signal.Address Action -> Model -> Html
view address model =
  div
    [class "center"]
    [ Input.view (Signal.forwardTo address Input) model.input
    , div
        [class "pins"]
        [Cards.view (Signal.forwardTo address Pins) model.pins]
    , Cards.view (Signal.forwardTo address Cards) model.cards
    ]

arrowPressAutoCompleteInput : { x : Int, y : Int } -> Action
arrowPressAutoCompleteInput =
  Input << Input.AutoComplete << AutoComplete.ArrowPress


keyboardInputs : Signal Action
keyboardInputs =
  Signal.map arrowPressAutoCompleteInput Keyboard.arrows


app : StartApp.App Model
app =
  StartApp.start { init = init, view = view, update = update, inputs = [keyboardInputs] }


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks

main : Signal Html
main =
  app.html
