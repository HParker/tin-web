module Card where

import Html exposing (Html, Attribute, text, toElement, div, input, span)
import Html.Attributes exposing (id, class)
import Signal exposing (Address)
import Markdown
import Html.Events
import Effects exposing (Effects)


type alias Model =
  { title : String
  , body : String
  , id : ID
  , collapsed : Bool
  }


type alias ID = Int


type Action
  = NoOp
  | Collapse ID
  | Move ID


build : String -> String -> ID -> Model
build title body id =
  Model title body id False


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
    Move id ->
      (model, Effects.none)
    NoOp ->
      (model, Effects.none)


view : Signal.Address Action -> Model -> Html
view address card =
  let
    (cardBody, collapseIcon) =
      if card.collapsed then
        ("", "icon octicon octicon-chevron-up")
      else
        (card.body, "icon octicon octicon-chevron-down")
  in
    div
      [class "card"]
        [ span [class "title"] [text card.title]
        , span
           [ class "icon octicon octicon-pin"
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
