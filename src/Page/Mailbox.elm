module Page.Mailbox exposing (Model, Msg, init, load, update, view)

import Data.Message as Message exposing (Message)
import Data.MessageHeader as MessageHeader exposing (MessageHeader)
import Json.Decode as Decode exposing (Decoder)
import Html exposing (..)
import Html.Attributes exposing (class, classList, href, id, placeholder, rel, style, target, type_, value)
import Http exposing (Error)
import Html.Events exposing (..)


inbucketBase : String
inbucketBase =
    ""



-- MODEL --


type alias Model =
    { flash : String
    , headers : List MessageHeader
    , selected : Maybe MessageHeader
    , message : Maybe Message
    }


init : Model
init =
    Model "" [] Nothing Nothing


load : String -> Cmd Msg
load name =
    getMailbox name



-- UPDATE --


type Msg
    = SelectMessage MessageHeader
    | DeleteMessage Message
    | DeleteMessageResult (Result Http.Error ())
    | NewMailbox (Result Http.Error (List MessageHeader))
    | NewMessage (Result Http.Error Message)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SelectMessage msg ->
            ( { model | selected = Just msg }, getMessage msg )

        DeleteMessage msg ->
            deleteMessage model msg

        DeleteMessageResult (Ok _) ->
            ( model, Cmd.none )

        DeleteMessageResult (Err err) ->
            ( { model | flash = httpErrorString (err) }, Cmd.none )

        NewMailbox (Ok headers) ->
            ( { model | headers = headers }, Cmd.none )

        NewMailbox (Err err) ->
            ( { model | flash = httpErrorString (err) }, Cmd.none )

        NewMessage (Ok msg) ->
            ( { model | message = Just msg }, Cmd.none )

        NewMessage (Err err) ->
            ( { model | flash = httpErrorString (err) }, Cmd.none )


getMailbox : String -> Cmd Msg
getMailbox name =
    let
        url =
            inbucketBase ++ "/api/v1/mailbox/" ++ name
    in
        Http.send NewMailbox (Http.get url decodeMailbox)


decodeMailbox : Decoder (List MessageHeader)
decodeMailbox =
    Decode.list MessageHeader.decoder


deleteMessage : Model -> Message -> ( Model, Cmd Msg )
deleteMessage model msg =
    let
        url =
            inbucketBase ++ "/api/v1/mailbox/" ++ msg.mailbox ++ "/" ++ msg.id

        request =
            httpDelete url

        cmd =
            Http.send DeleteMessageResult request
    in
        ( { model
            | message = Nothing
            , selected = Nothing
            , headers = List.filter (\x -> x.id /= msg.id) model.headers
          }
        , cmd
        )


getMessage : MessageHeader -> Cmd Msg
getMessage msg =
    let
        url =
            inbucketBase ++ "/api/v1/mailbox/" ++ msg.mailbox ++ "/" ++ msg.id

        request =
            Http.get url Message.decoder
    in
        Http.send NewMessage request



-- VIEW --


view : Model -> Html Msg
view model =
    div [ id "page", class "mailbox" ]
        [ aside [ id "message-list" ] [ viewMailbox model ]
        , main_ [ id "message" ] [ viewMessage model ]
        ]


viewMailbox : Model -> Html Msg
viewMailbox model =
    div [] (List.map (viewHeader model) (List.reverse model.headers))


viewHeader : Model -> MessageHeader -> Html Msg
viewHeader mailbox msg =
    div
        [ classList
            [ ( "message-list-entry", True )
            , ( "selected", mailbox.selected == Just msg )
            , ( "unseen", not msg.seen )
            ]
        , onClick (SelectMessage msg)
        ]
        [ div [ class "subject" ] [ text msg.subject ]
        , div [ class "from" ] [ text msg.from ]
        , div [ class "date" ] [ text msg.date ]
        ]


viewMessage : Model -> Html Msg
viewMessage model =
    case model.message of
        Just message ->
            div []
                [ div [ class "button-bar" ]
                    [ button [ class "danger", onClick (DeleteMessage message) ] [ text "Delete" ]
                    , a
                        [ href
                            (inbucketBase
                                ++ "/mailbox/"
                                ++ message.mailbox
                                ++ "/"
                                ++ message.id
                                ++ "/source"
                            )
                        , target "_blank"
                        ]
                        [ button [] [ text "Source" ] ]
                    ]
                , dl [ id "message-header" ]
                    [ dt [] [ text "From:" ]
                    , dd [] [ text message.from ]
                    , dt [] [ text "To:" ]
                    , dd [] (List.map text message.to)
                    , dt [] [ text "Date:" ]
                    , dd [] [ text message.date ]
                    , dt [] [ text "Subject:" ]
                    , dd [] [ text message.subject ]
                    ]
                , article [] [ text message.body.text ]
                ]

        Nothing ->
            text ""



-- UTILS --


httpDelete : String -> Http.Request ()
httpDelete url =
    Http.request
        { method = "DELETE"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectStringResponse (\_ -> Ok ())
        , timeout = Nothing
        , withCredentials = False
        }


httpErrorString : Http.Error -> String
httpErrorString error =
    case error of
        Http.BadUrl str ->
            "Bad URL: " ++ str

        Http.Timeout ->
            "HTTP timeout"

        Http.NetworkError ->
            "HTTP Network error"

        Http.BadStatus res ->
            "Bad HTTP status: " ++ toString res.status.code

        Http.BadPayload msg res ->
            "Bad HTTP payload: "
                ++ msg
                ++ " ("
                ++ toString res.status.code
                ++ ")"
