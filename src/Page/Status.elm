module Page.Status exposing (Model, Msg, init, load, subscriptions, update, view)

import Data.Metrics as Metrics exposing (Metrics)
import Data.Session as Session exposing (Session)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error)
import Time exposing (Time)


-- import Html.Attributes exposing (..)
-- import Html.Events exposing (..)
-- MODEL --


type alias Model =
    { metrics : Maybe Metrics }


init : Session -> Model
init session =
    { metrics = Nothing }


load : Cmd Msg
load =
    getMetrics



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (5 * Time.second) Tick



-- UPDATE --


type Msg
    = NewMetrics (Result Http.Error Metrics)
    | Tick Time


update : Session -> Msg -> Model -> ( Model, Cmd Msg, Session.Msg )
update session msg model =
    case msg of
        NewMetrics (Ok metrics) ->
            ( { model | metrics = Just metrics }, Cmd.none, Session.None )

        NewMetrics (Err err) ->
            ( model, Cmd.none, Session.None )

        Tick time ->
            ( model, getMetrics, Session.SetFlash (toString time) )


getMetrics : Cmd Msg
getMetrics =
    Http.get "/debug/vars" Metrics.decoder
        |> Http.send NewMetrics



-- VIEW --


view : Session -> Model -> Html Msg
view session model =
    div [ id "page" ]
        [ h1 [] [ text "Inbucket Status" ]
        , case model.metrics of
            Nothing ->
                div [] [ text "Loading metrics..." ]

            Just metrics ->
                div []
                    [ framePanel "General Metrics"
                        [ viewLiveMetric "System Memory" metrics.sysMem
                        , viewLiveMetric "Heap Size" metrics.heapSize
                        , viewLiveMetric "Heap In-Use" metrics.heapInUse
                        , viewLiveMetric "Heap # Objects" metrics.heapObjects
                        , viewLiveMetric "Goroutines" metrics.goRoutines
                        , viewLiveMetric "Open WebSockets" metrics.webSockets
                        ]
                    , framePanel "SMTP Metrics"
                        [ viewLiveMetric "Current Connections" metrics.smtpCurrent
                        ]
                    ]
        ]


viewLiveMetric : String -> Int -> Html a
viewLiveMetric label value =
    div [ class "metric" ]
        [ div [ class "label" ] [ text label ]
        , div [ class "value" ] [ text (toString value) ]
        , div [ class "graph" ] [ text "~magic graph goes here~ (10min)" ]
        ]


framePanel : String -> List (Html a) -> Html a
framePanel name html =
    div [ class "metric-panel" ]
        [ h2 [] [ text name ]
        , div [ class "metrics" ] html
        ]
