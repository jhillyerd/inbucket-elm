module Views.Page exposing (ActivePage(..), frame)

import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (attribute, class, classList, href, id, placeholder, type_, value)
import Html.Events as Events
import Route exposing (Route)


type ActivePage
    = Other
    | Monitor
    | Status


frame : ( String -> msg, msg, String ) -> Session -> ActivePage -> Html msg -> Html msg
frame ( mailboxInput, viewMailbox, mailboxValue ) session page content =
    div [ id "app" ]
        [ header []
            [ ul [ id "navbar", class "navbg", attribute "role" "navigation" ]
                [ li [ id "navbar-brand" ] [ a [ Route.href Route.Home ] [ text "@ inbucket" ] ]
                , navbarLink page Route.Monitor [ text "Monitor" ]
                , navbarLink page Route.Status [ text "Status" ]
                , li [ id "navbar-mailbox" ]
                    [ form [ Events.onSubmit viewMailbox ]
                        [ input
                            [ type_ "text"
                            , placeholder "mailbox"
                            , value mailboxValue
                            , Events.onInput mailboxInput
                            ]
                            []
                        ]
                    ]
                ]
            , div [] [ text ("Status: " ++ session.flash) ]
            ]
        , div [ id "navbg" ] [ text "" ]
        , content
        , footer []
            [ div [ id "footer" ]
                [ a [ href "https://www.inbucket.org" ] [ text "Inbucket" ]
                , text " is an open source projected hosted at "
                , a [ href "https://github.com/jhillyerd/inbucket" ] [ text "Github" ]
                , text "."
                ]
            ]
        ]


navbarLink : ActivePage -> Route -> List (Html a) -> Html a
navbarLink page route linkContent =
    li [ classList [ ( "navbar-active", isActive page route ) ] ]
        [ a [ Route.href route ] linkContent ]


isActive : ActivePage -> Route -> Bool
isActive page route =
    case (Debug.log "isActive" ( page, route )) of
        ( Monitor, Route.Monitor ) ->
            True

        ( Status, Route.Status ) ->
            True

        _ ->
            False
