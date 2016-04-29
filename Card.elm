module Card where

import Html exposing (Html, Attribute, text, toElement, div, input, span)
import Html.Attributes exposing (id, class)
import Signal exposing (Address)
import Markdown
import Html.Events
import Effects exposing (Effects)
import Json.Decode as Json exposing ((:=))
import Http
import Time exposing (Time)
import Task

type alias Model =
  { title : String
  , body : String
  , refreshRate : Int
  , timeLeft : Int
  , refreshUrl : String
  , id : ID
  , collapsed : Bool
  }


type alias ID = Int


type Action
  = NoOp
  | Collapse ID
  | Move ID
  | Delete ID
  | Tick Time
  | Refresh (Maybe Model)

build : String -> String -> Int -> String -> ID -> Model
build title body refreshRate refreshUrl id =
  Model title body refreshRate refreshRate refreshUrl id False


edit : Model -> String -> String -> Int -> String -> Model
edit model title body refreshRate refreshUrl =
  { model |
      title = title,
      body = body,
      refreshRate = refreshRate,
      refreshUrl = refreshUrl,
      timeLeft = refreshRate
  }


collapse : Model -> Model
collapse model =
  { model | collapsed = True}


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Collapse id ->
      let
        toggle =
          if model.collapsed then
            False
          else
            True
      in
        if model.id == id then
          ({ model | collapsed = toggle }, Effects.none)
        else
          (model, Effects.none)
    Tick time ->
      let
        newTimeLeft =
          model.timeLeft - 1
        newModel =
          if newTimeLeft > 0 then
            ({model | timeLeft = newTimeLeft}, Effects.none)
          else
            ({model | timeLeft = model.refreshRate}, Effects.none)
      in
        if newTimeLeft < 0 && model.refreshRate > 0 then
          ({model | timeLeft = model.refreshRate}, refresh model)
        else
          ({model | timeLeft = newTimeLeft}, Effects.none)
    Refresh maybeModel ->
      let
        newModel =
          case maybeModel of
            Just c -> c
            Nothing -> model

      in
        if newModel.id == model.id then
          (newModel, Effects.none)
        else
          (model, Effects.none)
    Move id ->
      (model, Effects.none)
    Delete _ ->
      (model, Effects.none)
    NoOp ->
      (model, Effects.none)


redecode : Model -> Json.Decoder Model
redecode model =
  Json.object4 (edit model)
    ("title" := Json.string)
    ("body" := Json.string)
    ("refresh_rate" := Json.int)
    ("refresh_url" := Json.string)

decode : Json.Decoder (ID -> Model)
decode =
  Json.object4 build
    ("title" := Json.string)
    ("body" := Json.string)
    ("refresh_rate" := Json.int)
    ("refresh_url" := Json.string)


refresh : Model -> Effects Action
refresh model =
  Http.get (redecode model) model.refreshUrl
    |> Task.toMaybe
    |> Task.map Refresh
    |> Effects.task


view : Signal.Address Action -> Model -> Html
view address card =
  let
    (cardBody, collapseIcon) =
      if card.collapsed then
        ("", "card-icon icon octicon octicon-chevron-up")
      else
        (card.body, "card-icon icon octicon octicon-chevron-down")
  in
    div
      [class "card"]
        [ span
            [class "title"]
            [text card.title]
        , span
            [ class "card-icon icon octicon octicon-x"
            , Html.Events.onClick address (Delete card.id)
            ]
            []
        , span
           [ class "card-icon icon octicon octicon-pin"
           , Html.Events.onClick address (Move card.id)
           ]
           []
        , span
           [ class collapseIcon
           , Html.Events.onClick address (Collapse card.id)
           ]
           []
        , Markdown.toHtml cardBody
        ]
