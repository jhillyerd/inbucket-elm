module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (placeholder, type_, style)
import Html.Events exposing (..)
import Http exposing (Error)
import Json.Decode exposing (..)


inbucketBase : String
inbucketBase =
    "http://192.168.1.10:9000"


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias MessageHeader =
    { mailbox : String
    , id : String
    , from : String
    , to : List String
    , subject : String
    , date : String
    , size : Int
    , seen : Bool
    }


type alias Model =
    { flash : String
    , mailboxName : String
    , mailbox : Maybe Mailbox
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
            , mailboxName = ""
            , mailbox = Nothing
            }
    in
        ( model, Cmd.none )


type Msg
    = MailboxNameInput String
    | ClickViewMailbox
    | ClickViewMessage MessageHeader
    | NewMailbox (Result Http.Error (List MessageHeader))


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MailboxNameInput name ->
            ( { model | mailboxName = name }, Cmd.none )

        ClickViewMailbox ->
            ( model, getMailbox model.mailboxName )

        ClickViewMessage msg ->
            case model.mailbox of
                Just mailbox ->
                    ( { model | mailbox = Just { mailbox | selected = Just msg } }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        NewMailbox (Err err) ->
            ( { model | flash = httpErrorString (err) }, Cmd.none )

        NewMailbox (Ok result) ->
            ( { model | mailbox = Just (Mailbox model.mailboxName result Nothing) }, Cmd.none )


getMailbox : String -> Cmd Msg
getMailbox name =
    let
        url =
            inbucketBase ++ "/api/v1/mailbox/" ++ name

        request =
            Http.get url decodeMailbox
    in
        Http.send NewMailbox request


decodeMailbox : Decoder (List MessageHeader)
decodeMailbox =
    list
        (map8
            MessageHeader
            (field "mailbox" string)
            (field "id" string)
            (field "from" string)
            (field "to" (list string))
            (field "subject" string)
            (field "date" string)
            (field "size" int)
            (field "seen" bool)
        )


view : Model -> Html Msg
view model =
    div []
        [ div [] [ text ("Status: " ++ model.flash) ]
        , div [] [ viewMailboxInput model ]
        ]


viewMailboxInput : Model -> Html Msg
viewMailboxInput model =
    div []
        [ form [ onSubmit ClickViewMailbox ]
            [ input [ type_ "text", placeholder "mailbox", onInput MailboxNameInput ] []
            , button [ onClick ClickViewMailbox ] [ text "View" ]
            , viewMailbox model
            ]
        ]


viewMailbox : Model -> Html Msg
viewMailbox model =
    case model.mailbox of
        Nothing ->
            div [] [ text "Empty" ]

        Just mailbox ->
            div [] (List.map (viewMessage mailbox) (List.reverse mailbox.headers))


viewMessage : Mailbox -> MessageHeader -> Html Msg
viewMessage mailbox msg =
    let
        styles =
            if mailbox.selected == Just msg then
                [ ( "background", "#ccc" ) ]
            else
                []
    in
        div
            [ onClick (ClickViewMessage msg), style styles ]
            [ div [] [ text msg.subject ]
            , div [] [ text msg.from ]
            , div [] [ text msg.date ]
            ]


httpErrorString : Http.Error -> String
httpErrorString error =
    "HTTP: "
        ++ case error of
            Http.BadUrl str ->
                "bad URL: " ++ str

            Http.Timeout ->
                "timeout"

            Http.NetworkError ->
                "network error"

            Http.BadStatus res ->
                "bad status: " ++ (toString res.status.code)

            Http.BadPayload msg _ ->
                "bad payload: " ++ msg
