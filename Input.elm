module Input where

import Effects exposing (Effects, Never)
import Task
import Http
import Json.Decode as Json exposing ((:=))
import Signal exposing (Address)
import Html exposing (Html, input, ul, li, text, div)
import Html.Attributes exposing (placeholder, value, autofocus, id, class)
import Html.Events exposing (on, targetValue, keyCode, onKeyPress)
import AutoComplete

type alias Model =
  { command : String
  , completions : AutoComplete.Model
  }

type alias Card =
  { title : String
  , body : String
  }

type Action
  = NoOp
  | Add (Maybe Card)
  | Request
  | Completions String
  | AutoComplete AutoComplete.Action
  | StoreVal String


init : (Model, Effects Action)
init =
  (Model "" AutoComplete.init, Effects.map AutoComplete (AutoComplete.fetch "default"))


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Request -> ({model | command = ""}, getCard model.command)
    StoreVal s -> ({model | command = s}, Effects.none)
    Completions s -> (model, Effects.map AutoComplete (AutoComplete.fetch "default"))
    AutoComplete act ->
      case act of
        AutoComplete.Complete completion ->
          let
            newModel = { model | command = completion }
          in
            (newModel, Effects.none)
        _ ->
          let
            (completionsModel, fx) = AutoComplete.update act model.completions
          in
            ({ model | completions = completionsModel}, Effects.map AutoComplete fx)
    Add _ -> (model, Effects.none)
    NoOp -> (model, Effects.none)


-- TODO: move to card
decodeCard : Json.Decoder Card
decodeCard =
  Json.object2 (\title body -> Card title body)
    ("title" := Json.string)
    ("body" := Json.string)


getCard : String -> Effects Action
getCard command =
  Http.get decodeCard ("?q=" ++ command)
    |> Task.toMaybe
    |> Task.map Add
    |> Effects.task


view : Address Action -> Model -> Html
view address model =
  div [id "app"]
    [ input
        [ placeholder "Hey there!"
        , value model.command
        , autofocus True
        , id "interface"
        , onKeyPress  address handleKeyPress
        , on "input" targetValue (\val -> Signal.message address (StoreVal val))
        ]
        []
    , AutoComplete.view (Signal.forwardTo address AutoComplete) model.command model.completions
    ]


handleKeyPress : Int -> Action
handleKeyPress code =
  if code == 13 then Request else NoOp
