module Page.Mailbox exposing (Model, Msg, init, load, update, view)

import Data.Message as Message exposing (Message)
import Data.MessageHeader as MessageHeader exposing (MessageHeader)
import Data.Session as Session exposing (Session)
import Json.Decode as Decode exposing (Decoder)
import Html exposing (..)
import Html.Attributes exposing (class, classList, href, id, placeholder, rel, style, target, type_, value)
import Html.Events exposing (..)
import Http exposing (Error)
import HttpUtil


inbucketBase : String
inbucketBase =
    ""



-- MODEL --


type alias Model =
    { headers : List MessageHeader
    , selected : Maybe MessageHeader
    , message : Maybe Message
    }


init : Session -> Model
init session =
    Model [] Nothing Nothing


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


update : Session -> Msg -> Model -> ( Model, Cmd Msg, Session.Msg )
update session msg model =
    case msg of
        SelectMessage msg ->
            ( { model | selected = Just msg }, getMessage msg, Session.None )

        DeleteMessage msg ->
            deleteMessage model msg

        DeleteMessageResult (Ok _) ->
            ( model, Cmd.none, Session.SetFlash "BELETED!" )

        DeleteMessageResult (Err err) ->
            ( model, Cmd.none, Session.SetFlash (HttpUtil.errorString err) )

        NewMailbox (Ok headers) ->
            ( { model | headers = headers }, Cmd.none, Session.None )

        NewMailbox (Err err) ->
            ( model, Cmd.none, Session.SetFlash (HttpUtil.errorString err) )

        NewMessage (Ok msg) ->
            ( { model | message = Just msg }, Cmd.none, Session.None )

        NewMessage (Err err) ->
            ( model, Cmd.none, Session.SetFlash (HttpUtil.errorString err) )


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


deleteMessage : Model -> Message -> ( Model, Cmd Msg, Session.Msg )
deleteMessage model msg =
    let
        url =
            inbucketBase ++ "/api/v1/mailbox/" ++ msg.mailbox ++ "/" ++ msg.id

        request =
            HttpUtil.delete url

        cmd =
            Http.send DeleteMessageResult request
    in
        ( { model
            | message = Nothing
            , selected = Nothing
            , headers = List.filter (\x -> x.id /= msg.id) model.headers
          }
        , cmd
        , Session.None
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


view : Session -> Model -> Html Msg
view session model =
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
