module Input where

import Effects exposing (Effects, Never)
import Task
import Signal exposing (Address)
import Html exposing (Html, input, ul, li, text, div)
import Html.Attributes exposing (placeholder, value, autofocus, id, class)
import Html.Events exposing (on, targetValue, keyCode, onKeyPress, onFocus)
import AutoComplete

type alias Model =
  { command : String
  , completions : AutoComplete.Model
  }


type Action
  = NoOp
  | Request String
  | Completions String
  | AutoComplete AutoComplete.Action
  | StoreVal String


init : (Model, Effects Action)
init =
  (Model "" AutoComplete.init, Effects.map AutoComplete (AutoComplete.fetch "default"))


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    StoreVal s -> ({model | command = s}, Effects.none)
    Completions s -> (model, Effects.map AutoComplete (AutoComplete.fetch "default"))
    AutoComplete act ->
      case act of
        AutoComplete.Complete completion ->
          ({ model | command = completion }, Effects.none)
        _ ->
          let
            (completionsModel, fx) = AutoComplete.update act model.completions
          in
            ({ model | completions = completionsModel}, Effects.map AutoComplete fx)
    Request _ -> (model, Effects.none) -- Handled in Main
    NoOp -> (model, Effects.none)


sendCard : String -> Effects Action
sendCard command =
  Effects.task <| Task.succeed <| Request command


view : Address Action -> Model -> Html
view address model =
  div [id "app"]
    [ input
        [ placeholder "Hello"
        , value model.command
        , id "interface"
        , onFocus address (AutoComplete (AutoComplete.ShowCompletion True))
        , onKeyPress  address (handleKeyPress model)
        , on "input" targetValue (\val -> Signal.message address (StoreVal val))
        ]
        []
    , AutoComplete.view (Signal.forwardTo address AutoComplete) model.command model.completions
    ]


handleKeyPress : Model -> Int -> Action
handleKeyPress model code =
  if code == 13 then Request model.command else NoOp
