module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, classList, href, id, placeholder, rel, style, target, type_, value)


-- import Html.Events exposing (..)

import Http exposing (Error)


-- import Json.Decode as Decode exposing (Decoder)

import Navigation exposing (Location)
import Page.Home as Home
import Page.Mailbox as Mailbox
import Route exposing (Route)
import Data.Session as Session exposing (Session)


inbucketBase : String
inbucketBase =
    ""



-- MODEL --


type alias Model =
    { route : Route
    , session : Session
    , mailboxName : String
    , home : Home.Model
    , mailbox : Mailbox.Model
    }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        session =
            Session.init

        model =
            { route = Route.Home
            , session = session
            , mailboxName = ""
            , home = Home.init session
            , mailbox = Mailbox.init session
            }

        route =
            Route.fromLocation location
    in
        if route /= model.route then
            applySession (updateRoute route model)
        else
            ( model, Cmd.none )


type Msg
    = NewRoute Route
    | MailboxNameInput String
    | HomeMsg Home.Msg
    | MailboxMsg Mailbox.Msg


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    applySession
        (case msg of
            NewRoute route ->
                updateRoute route model

            MailboxNameInput name ->
                ( { model | mailboxName = name }, Cmd.none, Session.None )

            HomeMsg subMsg ->
                let
                    ( subModel, subCmd, sessionMsg ) =
                        Home.update model.session subMsg model.home
                in
                    ( { model | home = subModel }, Cmd.map HomeMsg subCmd, sessionMsg )

            MailboxMsg subMsg ->
                let
                    ( subModel, subCmd, sessionMsg ) =
                        Mailbox.update model.session subMsg model.mailbox
                in
                    ( { model | mailbox = subModel }, Cmd.map MailboxMsg subCmd, sessionMsg )
        )


updateRoute : Route -> Model -> ( Model, Cmd Msg, Session.Msg )
updateRoute route model =
    case route of
        Route.Unknown hash ->
            ( model, Cmd.none, Session.SetFlash ("Unknown route requested: " ++ hash) )

        Route.Mailbox name ->
            ( { model
                | route = route
                , mailboxName = name
              }
            , Cmd.map MailboxMsg (Mailbox.load name)
            , Session.None
            )

        _ ->
            ( { model | route = route }, Cmd.none, Session.None )


applySession : ( Model, Cmd Msg, Session.Msg ) -> ( Model, Cmd Msg )
applySession ( model, cmd, sessionMsg ) =
    ( { model | session = Session.update sessionMsg model.session }
    , cmd
    )



-- VIEW --


view : Model -> Html Msg
view model =
    div [ id "app" ]
        [ header [] [ div [] [ text ("Status: " ++ model.session.flash) ] ]
        , page model
        , footer []
            [ div [ id "footer" ]
                [ a [ href "https://www.inbucket.org" ] [ text "Inbucket" ]
                , text " is an open source projected hosted at "
                , a [ href "https://github.com/jhillyerd/inbucket" ] [ text "Github" ]
                , text "."
                ]
            ]
        ]


page : Model -> Html Msg
page model =
    case model.route of
        Route.Unknown _ ->
            Html.map HomeMsg (Home.view model.session model.home)

        Route.Home ->
            Html.map HomeMsg (Home.view model.session model.home)

        Route.Mailbox name ->
            Html.map MailboxMsg (Mailbox.view model.session model.mailbox)



-- MAIN --


main : Program Never Model Msg
main =
    Navigation.program (Route.fromLocation >> NewRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- UTILS --


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
