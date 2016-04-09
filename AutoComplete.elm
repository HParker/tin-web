module AutoComplete where

import Html exposing (Html, ul, li, text)
import Html.Attributes exposing (id, class)
import Html.Events exposing (onClick)
import String
import Effects exposing (Effects, Never)
import Json.Decode as Json exposing ((:=))
import Http
import Task

type Action
  = StoreCompletions (Maybe (List String))
  | ArrowPress { x : Int, y : Int }
  | Complete String

type alias Completion =
  { command : String }

type alias Model =
  { selection : Int
  , completions : List String
  }

init : Model
init =
  Model 0 []

show : Signal.Address Action -> String -> Html
show address completion =
  li
    [ Html.Events.onClick address (Complete completion)
    , class "completion"
    ]
    [text completion]


view : Signal.Address Action -> String -> Model -> Html
view address command model =
  let
    visible = List.filter (String.startsWith command) model.completions
  in
    ul [class "completions"] (List.map (show address) visible)


backupCompletions : List String
backupCompletions = ["one", "two", "three"]


moveSelection : { x : Int, y : Int } -> Model -> Model
moveSelection key model =
  { model | selection = model.selection + key.y }

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    StoreCompletions newCompletions ->
      let
        newModel = Model 0 (Maybe.withDefault backupCompletions newCompletions)
      in
        (newModel, Effects.none)
    ArrowPress key ->
      let
        newModel = moveSelection key model
      in
        (newModel, Effects.none)
    Complete s -> (model, Effects.none)


decodeCompletion : Json.Decoder (List String)
decodeCompletion =
  Json.list Json.string


fetch : String -> Effects Action
fetch package =
  Http.get decodeCompletion ("/comp/?pack=" ++ package)
    |> Task.toMaybe
    |> Task.map StoreCompletions
    |> Effects.task
