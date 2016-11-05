module Input exposing (..)

import Html exposing (Html, input, ul, li, text, div, button)
import Html.App
import Html.Attributes exposing (placeholder, value, autofocus, id, class)
import Html.Events exposing (on, targetValue, keyCode, onFocus, onBlur, onInput, onClick, onSubmit)
import AutoComplete

type alias Model =
  { command : String
  , completions : AutoComplete.Model
  }


type Msg
  = NoOp
  | Completions String
  | AutoComplete AutoComplete.Msg
  | StoreVal String

init : (Model, Cmd Msg)
init =
  (Model "" AutoComplete.init, Cmd.map AutoComplete (AutoComplete.fetch "default"))


storeCommand : String -> Model -> Model
storeCommand command model =
  let
    completions = model.completions
    newCompletions =
      { completions | command = command}
  in
    { model |
        command = command,
        completions = newCompletions
    }

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    StoreVal s ->
      (storeCommand s model, Cmd.none)
    Completions s ->
      (model, Cmd.map AutoComplete (AutoComplete.fetch "default"))
    AutoComplete act ->
      case act of
        AutoComplete.Complete completion ->
          ({ model | command = completion }, Cmd.none)
        _ ->
          let
            (completionsModel, fx) = AutoComplete.update act model.completions
          in
            ({ model | completions = completionsModel}, Cmd.map AutoComplete fx)
    NoOp -> (model, Cmd.none)


view : Model -> Html Msg
view model =
  div [ id "main-interface" ]
    [ input
        [ value model.command
        , id "interface"
        , onFocus (AutoComplete (AutoComplete.ShowCompletion True))
        , onBlur (AutoComplete (AutoComplete.ShowCompletion True))
        , onInput StoreVal
        ]
        []
    , Html.App.map AutoComplete (AutoComplete.view model.completions)
    ]
