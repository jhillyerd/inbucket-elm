module Page.Status exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Data.Session as Session exposing (Session)


-- import Html.Attributes exposing (..)
-- import Html.Events exposing (..)
-- MODEL --


type alias Model =
    {}


init : Session -> Model
init session =
    {}



-- UPDATE --


type Msg
    = Msg


update : Session -> Msg -> Model -> ( Model, Cmd Msg, Session.Msg )
update session msg model =
    ( model, Cmd.none, Session.None )



-- VIEW --


view : Session -> Model -> Html Msg
view session model =
    div [] [ text "This is the status page" ]
