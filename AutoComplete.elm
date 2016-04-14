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
  | ShowCompletion Bool


type alias Completion =
  { command : String
  , info : String
  }

type alias Model =
  { selection : Int
  , visible : Bool
  , completions : List Completion
  }

init : Model
init =
  Model 0 False []

show : Signal.Address Action -> Int -> Int -> Completion -> Html
show address selected current completion =
  let
    completionClass = if selected == current then
                "selected-completion completion"
              else
                "completion"
  in
  li
    [ Html.Events.onClick address (Complete completion.command)
    , class completionClass
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
    if model.visible then
      ul [class "completions"]
           (List.indexedMap (show address model.selection) visible)
    else
      ul [] []


backupCompletions : List Completion
backupCompletions = []


getAt : List a -> Int -> Maybe a
getAt xs idx = List.head <| List.drop idx xs


moveSelection : { x : Int, y : Int } -> Model -> (Model, Effects Action)
moveSelection key model =
  let
    potentialSelection =
      model.selection - key.y
    newSelection =
      if potentialSelection >= 0 && potentialSelection < (List.length model.completions) then
        potentialSelection
      else
        model.selection
    fx =
      if key.x > 0 then
        case getAt model.completions model.selection of
          Just completion -> Effects.task <| Task.succeed <| Complete completion.command
          Nothing -> Effects.none
      else
        Effects.none
  in
    ({ model | selection = newSelection }, fx)

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    StoreCompletions newCompletions ->
      let
        newModel = { model |
                       completions = (Maybe.withDefault backupCompletions newCompletions)
                   }
      in
        (newModel, Effects.none)
    ArrowPress key ->
      moveSelection key model
    Complete s -> (model, Effects.none)
    ShowCompletion state ->
      ({ model | visible = state}, Effects.none)


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
