module AutoComplete exposing (..)

import Html exposing (Html, ul, li, text, i, b)
import Html.Attributes exposing (id, class)
import Html.Events exposing (onClick)
import String
import Json.Decode as Json exposing ((:=))
import Task
import Http


type Msg
  = StoreCompletions (List Completion)
  | FetchFailed Http.Error
  | ArrowPress { x : Int, y : Int }
  | Complete String
  | ShowCompletion Bool
  | NoOp

type alias Completion =
  { command : String
  , info : String
  }

type alias Model =
  { selection : Int
  , visible : Bool
  , completions : List Completion
  , command : String
  }

init : Model
init =
  Model 0 False [] ""

show : Int -> Int -> Completion -> Html Msg
show selected current completion =
  let
    completionClass = if selected == current then
                "selected-completion completion"
              else
                "completion"
  in
  li
    [ Html.Events.onClick (Complete completion.command)
    , class completionClass
    ]
    [ b [class "command"] [text completion.command]
    , i [class "info"] [text completion.info]
    ]


completionMatch : String -> Completion -> Bool
completionMatch command completion =
  let
    keyword =
      case (List.head (String.split " " command)) of
        Just key ->
          String.toLower <| key
        Nothing ->
          ""
  in
    (String.startsWith keyword) completion.command

view : Model -> Html Msg
view model =
  let
    visible = List.filter (completionMatch model.command) model.completions
  in
    if model.visible then
      ul
        [class "completions"]
        (List.indexedMap (show model.selection) visible)
    else
      ul [] []


backupCompletions : List Completion
backupCompletions = []


getAt : List a -> Int -> Maybe a
getAt xs idx = List.head <| List.drop idx xs


moveSelection : { x : Int, y : Int } -> Model -> (Model, Cmd Msg)
moveSelection key model =
  let
    visibleCompletions =
      List.filter (completionMatch model.command) model.completions
    potentialSelection =
      model.selection - key.y
    newSelection =
      if potentialSelection >= 0 && potentialSelection < (List.length visibleCompletions) then
        potentialSelection
      else
        model.selection
    fx =
      case getAt visibleCompletions model.selection of
        Just completion ->
          Cmd.none
        Nothing -> Cmd.none
  in
    ({ model | selection = newSelection }, fx)

update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    StoreCompletions newCompletions ->
      let
        newModel = { model |
                       completions = newCompletions
                   }
      in
        (newModel, Cmd.none)
    ArrowPress key ->
      moveSelection key model
    Complete s ->
      (model, Cmd.none)
    ShowCompletion state ->
      ({ model | visible = state}, Cmd.none)
    FetchFailed _ ->
      (model, Cmd.none)
    NoOp ->
      (model, Cmd.none)


decodeCompletion : Json.Decoder (List Completion)
decodeCompletion =
  Json.list (Json.object2 (\action info -> Completion action info)
    ("command" := Json.string)
    ("info" := Json.string))


fetch : String -> Cmd Msg
fetch package =
  Task.perform FetchFailed StoreCompletions (Http.get decodeCompletion ("/completion/?pack=" ++ package))
    --  |> Task.toMaybe
    -- |> Task.map StoreCompletions
