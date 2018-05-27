module Views.Page exposing (ActivePage(..), frame)

import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes
    exposing
        ( attribute
        , class
        , classList
        , href
        , id
        , placeholder
        , type_
        , selected
        , value
        )
import Html.Events as Events
import Route exposing (Route)


type ActivePage
    = Other
    | Monitor
    | Status


type alias FrameControls msg =
    { viewMailbox : String -> msg
    , mailboxOnInput : String -> msg
    , mailboxValue : String
    , recentOptions : List String
    , recentSelected : String
    }


frame : FrameControls msg -> Session -> ActivePage -> Html msg -> Html msg
frame controls session page content =
    div [ id "app" ]
        [ header []
            [ ul [ id "navbar", class "navbg", attribute "role" "navigation" ]
                [ li [ id "navbar-brand" ] [ a [ Route.href Route.Home ] [ text "@ inbucket" ] ]
                , navbarLink page Route.Monitor [ text "Monitor" ]
                , navbarLink page Route.Status [ text "Status" ]
                , li [ id "navbar-recent" ] [ recentSelect controls ]
                , li [ id "navbar-mailbox" ]
                    [ form [ Events.onSubmit (controls.viewMailbox controls.mailboxValue) ]
                        [ input
                            [ type_ "text"
                            , placeholder "mailbox"
                            , value controls.mailboxValue
                            , Events.onInput controls.mailboxOnInput
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
                , a [ href "https://github.com/jhillyerd/inbucket" ] [ text "GitHub" ]
                , text "."
                ]
            ]
        ]


{-| Renders list of recent mailboxes, selecting the currently active mailbox.
-}
recentSelect : FrameControls msg -> Html msg
recentSelect controls =
    let
        titleOption =
            option
                [ value ""
                , selected ("" == controls.recentSelected)
                ]
                [ text "Recent Mailboxes" ]

        recentOption mailbox =
            option
                [ value mailbox
                , selected (mailbox == controls.recentSelected)
                ]
                [ text mailbox ]
    in
        form []
            [ select
                [ value controls.recentSelected
                , Events.onInput controls.viewMailbox
                ]
                (titleOption :: List.map recentOption controls.recentOptions)
            ]


navbarLink : ActivePage -> Route -> List (Html a) -> Html a
navbarLink page route linkContent =
    li [ classList [ ( "navbar-active", isActive page route ) ] ]
        [ a [ Route.href route ] linkContent ]


isActive : ActivePage -> Route -> Bool
isActive page route =
    case ( page, route ) of
        ( Monitor, Route.Monitor ) ->
            True

        ( Status, Route.Status ) ->
            True

        _ ->
            False
