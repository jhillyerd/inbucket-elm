module Route exposing (Route(..), fromLocation, href, modifyUrl)

import Html exposing (Attribute)
import Html.Attributes as Attr
import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), Parser, parseHash, s, string)


type Route
    = Unknown String
    | Home
    | Mailbox String


matcher : Parser (Route -> a) a
matcher =
    Url.oneOf
        [ Url.map Home (s "")
        , Url.map Mailbox (s "mailbox" </> string)
        ]


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Unknown _ ->
                    []

                Home ->
                    []

                Mailbox name ->
                    [ "mailbox", name ]
    in
        "#/" ++ String.join "/" pieces



-- PUBLIC HELPERS --


href : Route -> Attribute msg
href route =
    Attr.href (routeToString route)


modifyUrl : Route -> Cmd msg
modifyUrl =
    routeToString >> Navigation.modifyUrl


fromLocation : Location -> Route
fromLocation location =
    if String.isEmpty location.hash then
        Home
    else
        case parseHash matcher location of
            Nothing ->
                Unknown location.hash

            Just route ->
                route
