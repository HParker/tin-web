module Card where

import Html exposing (Html, Attribute, text, toElement, div, input, span)
import Html.Attributes exposing (id, class)
import Signal exposing (Address)
import Json.Decode as Json exposing ((:=))
import Http
import Task
import Effects exposing (Effects)
import Markdown

type alias Card =
  { title : String
  , body : String
  }


type alias Model =
  List Card


type Action
  = NoOp
  | Get String
  | Add (Maybe Card)

show : Signal.Address Action -> Card -> Html
show address card =
  div [class "card"]
    [ span [class "title"] [text card.title]
    , span [class "icon octicon octicon-pin"] []
    , span [class "icon octicon octicon-chevron-up"] []
    , Markdown.toHtml card.body
    ]

view : Address Action -> Model -> Html
view address model =
  div
    []
    (List.map (show address) model)


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Get command ->
      (model, getCard command)
    Add card ->
      let
        newModel = case card of
                  Just c -> c :: model
                  Nothing -> model
      in
        (newModel, Effects.none)
    NoOp ->
      (model, Effects.none)

getCard : String -> Effects Action
getCard command =
  Http.get decodeCard ("?q=" ++ command)
    |> Task.toMaybe
    |> Task.map Add
    |> Effects.task

decodeCard : Json.Decoder Card
decodeCard =
  Json.object2 (\title body -> Card title body)
    ("title" := Json.string)
    ("body" := Json.string)
