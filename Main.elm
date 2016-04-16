import Html exposing (Html, Attribute)
import StartApp
import Effects exposing (Effects, Never)
import Task
import Card
import Input
import Html exposing (Html, Attribute, text, toElement, div, input)
import Html.Attributes exposing (id, class)
import Keyboard
import AutoComplete

type alias Model =
  { input : Input.Model
  , cards : Card.Model
  }

type Action
  = Input Input.Action
  | Card Card.Action


init : (Model, Effects Action)
init =
  let
    (input, fx) = Input.init
    cards = []
  in
    (Model input cards, Effects.map Input fx)


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Input msg ->
      case msg of
        Input.Request command ->
          let
            input = model.input
            newInput = { input | command = "" }
            (cards, fx) = Card.update (Card.Get command) model.cards
          in
            ({ model | input = newInput, cards = cards}, Effects.map Card fx)
        _ ->
          let
            (input, fx) = Input.update msg model.input
          in
            ({ model | input = input}, Effects.map Input fx)
    Card msg ->
      case msg of
        _ ->
          let
            (cards, fx) = Card.update msg model.cards
          in
            ({ model | cards = cards}, Effects.map Card fx)


view : Signal.Address Action -> Model -> Html
view address model =
  div [class "center"]
      [ Input.view (Signal.forwardTo address Input) model.input
      , Card.view (Signal.forwardTo address Card) model.cards
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
