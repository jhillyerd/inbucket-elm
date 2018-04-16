module Data.Session exposing (Session, Msg(..), init, update)


type alias Session =
    { flash : String }


type Msg
    = None
    | SetFlash String
    | ClearFlash


init : Session
init =
    Session ""


update : Msg -> Session -> Session
update msg session =
    case msg of
        None ->
            session

        SetFlash flash ->
            { session | flash = flash }

        ClearFlash ->
            { session | flash = "" }
