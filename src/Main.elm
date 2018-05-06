module Main exposing (..)

import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, type_, value)
import Html.Events as Events
import Navigation exposing (Location)
import Page.Home as Home
import Page.Mailbox as Mailbox
import Page.Monitor as Monitor
import Page.Status as Status
import Route exposing (Route)


-- MODEL --


type Page
    = Home Home.Model
    | Mailbox Mailbox.Model
    | Monitor Monitor.Model
    | Status Status.Model


type alias Model =
    { page : Page
    , session : Session
    , mailboxName : String
    }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        session =
            Session.init

        model =
            { page = Home Home.init
            , session = session
            , mailboxName = ""
            }

        route =
            Route.fromLocation location
    in
        applySession (setRoute route model)


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
    case model.page of
        Monitor subModel ->
            Sub.map MonitorMsg (Monitor.subscriptions subModel)

        Status subModel ->
            Sub.map StatusMsg (Status.subscriptions subModel)

        _ ->
            Sub.none



-- UPDATE --


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    applySession <|
        case msg of
            SetRoute route ->
                -- Updates broser URL to requested route.
                ( model, Route.newUrl route, Session.none )

            NewRoute route ->
                -- Responds to new browser URL.
                if model.session.routing then
                    setRoute route model
                else
                    -- Skip once, but re-enable routing.
                    ( model, Cmd.none, Session.EnableRouting )

            MailboxNameInput name ->
                ( { model | mailboxName = name }, Cmd.none, Session.none )

            ViewMailbox ->
                ( { model | mailboxName = "" }
                , Route.newUrl (Route.Mailbox model.mailboxName)
                , Session.none
                )

            _ ->
                updatePage msg model


{-| Delegates incoming messages to their respective sub-pages.
-}
updatePage : Msg -> Model -> ( Model, Cmd Msg, Session.Msg )
updatePage msg model =
    let
        -- Handles sub-model update by calling toUpdate with subMsg & subModel, then packing the
        -- updated sub-model back into model.page.
        modelUpdate toPage toMsg subUpdate subMsg subModel =
            let
                ( newModel, subCmd, sessionMsg ) =
                    subUpdate model.session subMsg subModel
            in
                ( { model | page = toPage newModel }, Cmd.map toMsg subCmd, sessionMsg )
    in
        case ( msg, model.page ) of
            ( HomeMsg subMsg, Home subModel ) ->
                modelUpdate Home HomeMsg Home.update subMsg subModel

            ( MailboxMsg subMsg, Mailbox subModel ) ->
                modelUpdate Mailbox MailboxMsg Mailbox.update subMsg subModel

            ( MonitorMsg subMsg, Monitor subModel ) ->
                modelUpdate Monitor MonitorMsg Monitor.update subMsg subModel

            ( StatusMsg subMsg, Status subModel ) ->
                modelUpdate Status StatusMsg Status.update subMsg subModel

            ( _, _ ) ->
                -- Disregard messages destined for the wrong page.
                ( model, Cmd.none, Session.none )


setRoute : Route -> Model -> ( Model, Cmd Msg, Session.Msg )
setRoute route model =
    case route of
        Route.Unknown hash ->
            ( model, Cmd.none, Session.SetFlash ("Unknown route requested: " ++ hash) )

        Route.Home ->
            ( { model | page = Home Home.init }
            , Cmd.none
            , Session.none
            )

        Route.Mailbox name ->
            ( { model | page = Mailbox (Mailbox.init name Nothing) }
            , Cmd.map MailboxMsg (Mailbox.load name)
            , Session.none
            )

        Route.Message mailbox id ->
            ( { model | page = Mailbox (Mailbox.init mailbox (Just id)) }
            , Cmd.map MailboxMsg (Mailbox.load mailbox)
            , Session.none
            )

        Route.Monitor ->
            ( { model | page = Monitor Monitor.init }
            , Cmd.none
            , Session.none
            )

        Route.Status ->
            ( { model | page = Status Status.init }
            , Cmd.map StatusMsg (Status.load)
            , Session.none
            )


applySession : ( Model, Cmd Msg, Session.Msg ) -> ( Model, Cmd Msg )
applySession ( model, cmd, sessionMsg ) =
    ( { model | session = Session.update sessionMsg model.session }, cmd )



-- VIEW --


view : Model -> Html Msg
view model =
    frame model <|
        case model.page of
            Home subModel ->
                Html.map HomeMsg (Home.view model.session subModel)

            Mailbox subModel ->
                Html.map MailboxMsg (Mailbox.view model.session subModel)

            Monitor subModel ->
                Html.map MonitorMsg (Monitor.view model.session subModel)

            Status subModel ->
                Html.map StatusMsg (Status.view model.session subModel)


frame : Model -> Html Msg -> Html Msg
frame model wrapped =
    div [ id "app" ]
        [ header []
            [ ul [ id "navbar", class "navbg", attribute "role" "navigation" ]
                [ li [ id "navbar-brand" ] [ a [ Route.href Route.Home ] [ text "@ inbucket" ] ]
                , li [ navTabClasses "monitor" model ]
                    [ a [ Route.href Route.Monitor ] [ text "Monitor" ] ]
                , li [ navTabClasses "status" model ]
                    [ a [ Route.href Route.Status ] [ text "Status" ] ]
                , li [ id "navbar-mailbox" ]
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
        , div [ id "navbg" ] [ text "" ]
        , wrapped
        , footer []
            [ div [ id "footer" ]
                [ a [ href "https://www.inbucket.org" ] [ text "Inbucket" ]
                , text " is an open source projected hosted at "
                , a [ href "https://github.com/jhillyerd/inbucket" ] [ text "Github" ]
                , text "."
                ]
            ]
        ]


navTabClasses : String -> Model -> Attribute msg
navTabClasses page model =
    let
        active =
            case model.page of
                Home _ ->
                    "home"

                Mailbox _ ->
                    "mailbox"

                Monitor _ ->
                    "monitor"

                Status _ ->
                    "status"
    in
        classList [ ( "navbar-active", active == page ) ]



-- MAIN --


main : Program Never Model Msg
main =
    Navigation.program (Route.fromLocation >> NewRoute)
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
