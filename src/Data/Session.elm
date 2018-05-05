module Data.Session exposing (Session, Msg(..), init, none, update)


type alias Session =
    { flash : String
    , routing : Bool
    }


type Msg
    = None
    | SetFlash String
    | ClearFlash
    | DisableRouting
    | EnableRouting


init : Session
init =
    Session "" True


update : Msg -> Session -> Session
update msg session =
    case msg of
        None ->
            session

        SetFlash flash ->
            { session | flash = flash }

        ClearFlash ->
            { session | flash = "" }

        DisableRouting ->
            { session | routing = False }

        EnableRouting ->
            { session | routing = True }


none : Msg
none =
    None
