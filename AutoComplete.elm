module AutoComplete where

import Html exposing (Html, ul, li, text, i, b)
import Html.Attributes exposing (id, class)
import Html.Events exposing (onClick)
import String
import Effects exposing (Effects, Never)
import Json.Decode as Json exposing ((:=))
import Http
import Task

type Action
  = StoreCompletions (Maybe (List Completion))
  | ArrowPress { x : Int, y : Int }
  | Complete String

type alias Completion =
  { command : String
  , info : String
  }

type alias Model =
  { selection : Int
  , completions : List Completion
  }

init : Model
init =
  Model 0 []

show : Signal.Address Action -> Completion -> Html
show address completion =
  li
    [ Html.Events.onClick address (Complete completion.command)
    , class "completion"
    ]
    [ b [class "command"] [text completion.command]
    , i [class "info"] [text completion.info]
    ]


completionMatch : String -> Completion -> Bool
completionMatch command completion =
  (String.startsWith (String.toLower command)) completion.command

view : Signal.Address Action -> String -> Model -> Html
view address command model =
  let
    visible = List.filter (completionMatch command) model.completions
  in
    ul [class "completions"] (List.map (show address) visible)


backupCompletions : List Completion
backupCompletions = []


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


decodeCompletion : Json.Decoder (List Completion)
decodeCompletion =
  Json.list (Json.object2 (\action info -> Completion action info)
    ("command" := Json.string)
    ("info" := Json.string))


fetch : String -> Effects Action
fetch package =
  Http.get decodeCompletion ("/comp/?pack=" ++ package)
    |> Task.toMaybe
    |> Task.map StoreCompletions
    |> Effects.task
