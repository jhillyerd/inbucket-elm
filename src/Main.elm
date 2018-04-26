module Main exposing (..)

import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, href, id, placeholder, type_, value)
import Html.Events as Events
import Navigation exposing (Location)
import Page.Home as Home
import Page.Mailbox as Mailbox
import Page.Monitor as Monitor
import Page.Status as Status
import Route exposing (Route)


-- MODEL --


type alias Model =
    { route : Route
    , session : Session
    , mailboxName : String
    , home : Home.Model
    , mailbox : Mailbox.Model
    , monitor : Monitor.Model
    , status : Status.Model
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
            , home = Home.init
            , mailbox = Mailbox.init
            , monitor = Monitor.init
            , status = Status.init
            }

        route =
            Route.fromLocation location
    in
        if route /= model.route then
            applySession (updateRoute route model)
        else
            ( model, Cmd.none )


type Msg
    = SetRoute Route
    | NewRoute Route
    | MailboxNameInput String
    | ViewMailbox
    | HomeMsg Home.Msg
    | MailboxMsg Mailbox.Msg
    | MonitorMsg Monitor.Msg
    | StatusMsg Status.Msg



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.route of
        Route.Monitor ->
            Sub.map MonitorMsg (Monitor.subscriptions model.monitor)

        Route.Status ->
            Sub.map StatusMsg (Status.subscriptions model.status)

        _ ->
            Sub.none



-- UPDATE --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    applySession
        (case msg of
            SetRoute route ->
                -- Updates broser URL to requested route.
                ( model, Route.newUrl route, Session.None )

            NewRoute route ->
                -- Responds to new browser URL.
                updateRoute route model

            MailboxNameInput name ->
                ( { model | mailboxName = name }, Cmd.none, Session.None )

            ViewMailbox ->
                ( { model | mailboxName = "" }
                , Route.newUrl (Route.Mailbox model.mailboxName)
                , Session.None
                )

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

            MonitorMsg subMsg ->
                let
                    ( subModel, subCmd, sessionMsg ) =
                        Monitor.update model.session subMsg model.monitor
                in
                    ( { model | monitor = subModel }, Cmd.map MonitorMsg subCmd, sessionMsg )

            StatusMsg subMsg ->
                let
                    ( subModel, subCmd, sessionMsg ) =
                        Status.update model.session subMsg model.status
                in
                    ( { model | status = subModel }, Cmd.map StatusMsg subCmd, sessionMsg )
        )


updateRoute : Route -> Model -> ( Model, Cmd Msg, Session.Msg )
updateRoute route model =
    case route of
        Route.Unknown hash ->
            ( model, Cmd.none, Session.SetFlash ("Unknown route requested: " ++ hash) )

        Route.Mailbox name ->
            ( { model | route = route }
            , Cmd.map MailboxMsg (Mailbox.load name)
            , Session.None
            )

        Route.Monitor ->
            ( { model | route = route, monitor = Monitor.init }
            , Cmd.none
            , Session.None
            )

        Route.Status ->
            ( { model | route = route }
            , Cmd.map StatusMsg (Status.load)
            , Session.None
            )

        _ ->
            -- Handle routes that require no special setup.
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
        [ viewHeader model
        , page model
        , viewFooter
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

        Route.Monitor ->
            Html.map MonitorMsg (Monitor.view model.session model.monitor)

        Route.Status ->
            Html.map StatusMsg (Status.view model.session model.status)


viewHeader : Model -> Html Msg
viewHeader model =
    header []
        [ ul [ class "nav", attribute "role" "navigation" ]
            [ li [] [ a [ Route.href Route.Home ] [ text "Home" ] ]
            , li [] [ a [ Route.href Route.Status ] [ text "Status" ] ]
            , li [] [ a [ Route.href Route.Monitor ] [ text "Monitor" ] ]
            , li []
                [ form [ Events.onSubmit ViewMailbox ]
                    [ input
                        [ type_ "text"
                        , placeholder "mailbox"
                        , value model.mailboxName
                        , Events.onInput MailboxNameInput
                        ]
                        []
                    ]
                ]
            ]
        , div [] [ text ("Status: " ++ model.session.flash) ]
        ]


viewFooter : Html Msg
viewFooter =
    footer []
        [ div [ id "footer" ]
            [ a [ href "https://www.inbucket.org" ] [ text "Inbucket" ]
            , text " is an open source projected hosted at "
            , a [ href "https://github.com/jhillyerd/inbucket" ] [ text "Github" ]
            , text "."
            ]
        ]



-- MAIN --


main : Program Never Model Msg
main =
    Navigation.program (Route.fromLocation >> NewRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
