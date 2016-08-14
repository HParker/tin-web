module Card exposing(..)

import Html exposing (Html, Attribute, text, div, input, span)
import Html.Attributes exposing (id, class)
import Markdown
import Html.Events
import Json.Decode as Json exposing ((:=))

type alias Model =
  { title : String
  , body : String
  , id : ID
  , collapsed : Bool
  }


type alias ID = Int


type Msg
  = NoOp
  | Collapse ID
  | Move ID
  | Delete ID

build : String -> String -> ID -> Model
build title body id =
  Model title body id False

collapse : Model -> Model
collapse model =
  { model | collapsed = True}


update : Msg -> Model -> (Model, Cmd Msg)
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
          ({ model | collapsed = toggle }, Cmd.none)
        else
          (model, Cmd.none)
    Move id ->
      (model, Cmd.none)
    Delete _ ->
      (model, Cmd.none)
    NoOp ->
      (model, Cmd.none)


decode : Json.Decoder (ID -> Model)
decode =
  Json.object2 build
    ("title" := Json.string)
    ("body" := Json.string)

view : Model -> Html Msg
view card =
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
            , Html.Events.onClick (Delete card.id)
            ]
            []
        , span
           [ class "card-icon icon octicon octicon-pin"
           , Html.Events.onClick (Move card.id)
           ]
           []
        , span
           [ class collapseIcon
           , Html.Events.onClick (Collapse card.id)
           ]
           []
        , Markdown.toHtml [] cardBody
        ]
