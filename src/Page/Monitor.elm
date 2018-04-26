module Page.Monitor exposing (Model, Msg, init, subscriptions, update, view)

import Data.MessageHeader as MessageHeader exposing (MessageHeader)
import Data.Session as Session exposing (Session)
import Json.Decode exposing (decodeString)
import Html exposing (..)
import WebSocket


-- import Html.Attributes exposing (..)
-- import Html.Events exposing (..)
-- MODEL --


type alias Model =
    { messages : List MessageHeader }


init : Model
init =
    { messages = [] }



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen "ws://192.168.1.10:3000/api/v1/monitor/messages"
        (decodeString MessageHeader.decoder >> NewMessage)



-- UPDATE --


type Msg
    = NewMessage (Result String MessageHeader)


update : Session -> Msg -> Model -> ( Model, Cmd Msg, Session.Msg )
update session msg model =
    case msg of
        NewMessage (Ok msg) ->
            ( { model | messages = msg :: model.messages }, Cmd.none, Session.None )

        NewMessage (Err err) ->
            ( model, Cmd.none, Session.SetFlash err )



-- VIEW --


view : Session -> Model -> Html Msg
view session model =
    div []
        [ h1 [] [ text "Inbucket Monitor" ]
        , p [] [ text "Messages will be listed here shortly after delivery." ]
        , table []
            [ thead []
                [ th [] [ text "Date" ]
                , th [] [ text "From" ]
                , th [] [ text "Mailbox" ]
                , th [] [ text "Subject" ]
                ]
            , tbody [] (List.map viewMessage model.messages)
            ]
        ]


viewMessage : MessageHeader -> Html Msg
viewMessage message =
    tr []
        [ td [] [ text message.date ]
        , td [] [ text message.from ]
        , td [] [ text message.mailbox ]
        , td [] [ text message.subject ]
        ]
