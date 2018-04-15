module Page.Home exposing (Model, Msg, init, update, view)

import Html exposing (..)


-- import Html.Attributes exposing (..)
-- import Html.Events exposing (..)
-- MODEL --


type alias Model =
    {}


init : Model
init =
    {}



-- UPDATE --


type Msg
    = Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- VIEW --


view : Model -> Html Msg
view model =
    div [] [ text "This is the home page" ]
