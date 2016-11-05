import Html exposing (Html, Attribute)
import Html.App
import Html exposing (Html, Attribute, text, div, input, span, h1, img)
import Html.Attributes exposing (id, class, src)
import Html.App as Html
import Keyboard
import WebSocket
import Json.Decode as Json exposing ((:=))
import Http
import Task

import Cards
import Card
import Input

type alias Model =
  { input : Input.Model
  , cards : Cards.Model
  , image : String
  }

type Msg
  = Input Input.Msg
  | Cards Cards.Msg
  | HandleKeypress Int
  | Push String
  | LoggedIn String
  | NotLoggedIn Http.Error

type alias Flags =
    { image_url: String }

init : Flags -> (Model, Cmd Msg)
init flags =
  let
    (input, fx) = Input.init
  in
    (Model input Cards.init flags.image_url, Cmd.map Input fx)


update : Msg -> Model -> (Model, Cmd Msg)
update action model =
  case action of
    HandleKeypress keyCode ->
      if keyCode == 13 then
        makeRequest model
      else
        (model, Cmd.none)
    Push cardJson ->
      let
        cardsModel = model.cards
        newCardResult = (Json.decodeString Card.decode cardJson)
        newCards = case newCardResult of
          Ok newCard ->
            { cardsModel |
               cards = newCard cardsModel.nextID :: cardsModel.cards
            , nextID = cardsModel.nextID + 1
            }
          Err _ ->
            model.cards
      in
        ( {model |
            cards = newCards
          }
        , Cmd.none
        )
    Input msg ->
      let
        (input, fx) = Input.update msg model.input
      in
        ({ model | input = input}, Cmd.map Input fx)
    Cards act ->
      let
        (cards, fx) = Cards.update act model.cards
      in
        ({ model | cards = cards}, Cmd.map Cards fx)
    LoggedIn msg ->
        (model, Cmd.none)
    NotLoggedIn msg ->
        (model, Cmd.none)


view : Model -> Html Msg
view model =
  div
    [class "center"]
    [ div
      [class "logo"]
      [viewLogo model]
    , div
        [id "app"]
        [ Html.App.map Input (Input.view model.input)
        , Html.App.map Cards (Cards.view model.cards)
        ]
    ]

viewLogo : Model -> Html Msg
viewLogo model =
    if model.image == "" then
      h1 [] [text "Tin"]
    else
      img [src model.image] []

makeRequest : Model -> (Model, Cmd Msg)
makeRequest model =
  let
    newInput = Input.storeCommand "" model.input
    (cards, fx) = Cards.update (Cards.Get model.input.command) model.cards
  in
    ({ model |
           input = newInput
         , cards = cards
         , image = model.image
     }
    , Cmd.map Cards fx)

main =
  Html.programWithFlags
    { init = init
    , update = update
    , view = view
    , subscriptions = \_ -> Sub.batch
                      [Keyboard.presses HandleKeypress
                      , WebSocket.listen "ws://localhost:8020/my-channel" Push
                      ]
    }


userDecode : Json.Decoder String
userDecode =
  ("image_url" := Json.string)


getImage : Cmd Msg
getImage =
  Http.get userDecode ("api/me")
    |> Task.perform NotLoggedIn LoggedIn
