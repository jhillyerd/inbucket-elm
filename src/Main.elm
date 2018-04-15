module Main exposing (..)

import Data.Message as Message exposing (Message)
import Data.MessageHeader as MessageHeader exposing (MessageHeader)
import Html exposing (..)
import Html.Attributes exposing (class, classList, href, id, placeholder, rel, style, target, type_, value)
import Html.Events exposing (..)
import Http exposing (Error)
import Json.Decode as Decode exposing (Decoder)


inbucketBase : String
inbucketBase =
    ""



-- MODEL --


type alias Model =
    { flash : String
    , mailboxName : String
    , mailbox : Maybe Mailbox
    , message : Maybe Message
    }


type alias Mailbox =
    { name : String
    , headers : List MessageHeader
    , selected : Maybe MessageHeader
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            { flash = ""
            , mailboxName = "swaks"
            , mailbox = Nothing
            , message = Nothing
            }
    in
        ( model, Cmd.none )


type Msg
    = MailboxNameInput String
    | ViewMailbox
    | SelectMessage MessageHeader
    | DeleteMessage Message
    | DeleteMessageResult (Result Http.Error ())
    | NewMailbox (Result Http.Error (List MessageHeader))
    | NewMessage (Result Http.Error Message)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MailboxNameInput name ->
            ( { model | mailboxName = name }, Cmd.none )

        ViewMailbox ->
            ( model, getMailbox model.mailboxName )

        SelectMessage msg ->
            case model.mailbox of
                Just mailbox ->
                    ( { model | mailbox = Just { mailbox | selected = Just msg } }
                    , getMessage msg
                    )

                Nothing ->
                    ( model, Cmd.none )

        DeleteMessage msg ->
            deleteMessage model msg

        DeleteMessageResult (Ok _) ->
            ( model, Cmd.none )

        DeleteMessageResult (Err err) ->
            ( { model | flash = httpErrorString (err) }, Cmd.none )

        NewMailbox (Ok headers) ->
            ( { model | mailbox = Just (Mailbox model.mailboxName headers Nothing) }, Cmd.none )

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


deleteMessage : Model -> Message -> ( Model, Cmd Msg )
deleteMessage ({ mailbox } as model) msg =
    let
        url =
            inbucketBase ++ "/api/v1/mailbox/" ++ msg.mailbox ++ "/" ++ msg.id

        request =
            httpDelete url

        cmd =
            Http.send DeleteMessageResult request
    in
        case mailbox of
            Nothing ->
                ( { model | message = Nothing }, cmd )

            Just mb ->
                ( { model
                    | message = Nothing
                    , mailbox =
                        Just
                            { mb
                                | selected = Nothing
                                , headers = List.filter (\x -> x.id /= msg.id) mb.headers
                            }
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


decodeMailbox : Decoder (List MessageHeader)
decodeMailbox =
    Decode.list MessageHeader.decoder



-- VIEW --


view : Model -> Html Msg
view model =
    div [ id "app" ]
        [ node "link" [ rel "stylesheet", href "/inbucket.css" ] []
        , header []
            [ div [] [ viewMailboxInput model ]
            , div [] [ text ("Status: " ++ model.flash) ]
            ]
        , aside [ id "mailbox" ] [ viewMailbox model ]
        , main_ [ id "message" ] [ viewMessage model ]
        , footer []
            [ div [ id "footer" ]
                [ a [ href "https://www.inbucket.org" ] [ text "Inbucket" ]
                , text " is an open source projected hosted at "
                , a [ href "https://github.com/jhillyerd/inbucket" ] [ text "Github" ]
                , text "."
                ]
            ]
        ]


viewMailboxInput : Model -> Html Msg
viewMailboxInput model =
    div []
        [ form [ onSubmit ViewMailbox ]
            [ input
                [ type_ "text"
                , placeholder "mailbox"
                , value model.mailboxName
                , onInput MailboxNameInput
                ]
                []
            , button [ onClick ViewMailbox ] [ text "View" ]
            ]
        ]


viewMailbox : Model -> Html Msg
viewMailbox model =
    case model.mailbox of
        Nothing ->
            div [] [ text "Empty" ]

        Just mailbox ->
            div [] (List.map (viewHeader mailbox) (List.reverse mailbox.headers))


viewHeader : Mailbox -> MessageHeader -> Html Msg
viewHeader mailbox msg =
    div
        [ classList
            [ ( "mailbox-entry", True )
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



-- MAIN --


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



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
