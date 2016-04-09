module Card where

import Html exposing (Html, Attribute, text, toElement, div, input)
import Html.Attributes exposing (id, class)
import Signal exposing (Address)

type alias Model =
  { title : String
  , body : String
  }

type Action
  = NoOp


view : Address Action -> Model -> Html
view action model =
  div [class "card"]
    [ Html.h1 [class "title"] [text model.title]
    , Html.p [class "body"] [text model.body]
    ]
