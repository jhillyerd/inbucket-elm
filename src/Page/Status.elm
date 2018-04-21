module Page.Status exposing (Model, Msg, init, load, subscriptions, update, view)

import Data.Metrics as Metrics exposing (Metrics)
import Data.Session as Session exposing (Session)
import Filesize
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error)
import HttpUtil
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
            ( model, Cmd.none, Session.SetFlash (HttpUtil.errorString err) )

        Tick time ->
            ( model, getMetrics, Session.ClearFlash )


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
                        [ viewLiveMetric "System Memory" Filesize.format metrics.sysMem
                        , viewLiveMetric "Heap Size" Filesize.format metrics.heapSize
                        , viewLiveMetric "Heap In-Use" Filesize.format metrics.heapInUse
                        , viewLiveMetric "Heap # Objects" fmtInt metrics.heapObjects
                        , viewLiveMetric "Goroutines" fmtInt metrics.goRoutines
                        , viewLiveMetric "Open WebSockets" fmtInt metrics.webSockets
                        ]
                    , framePanel "SMTP Metrics"
                        [ viewLiveMetric "Current Connections" fmtInt metrics.smtpCurrent
                        ]
                    ]
        ]


viewLiveMetric : String -> (Int -> String) -> Int -> Html a
viewLiveMetric label formatter value =
    div [ class "metric" ]
        [ div [ class "label" ] [ text label ]
        , div [ class "value" ] [ text (formatter value) ]
        , div [ class "graph" ] [ text "~magic graph goes here~ (10min)" ]
        ]


framePanel : String -> List (Html a) -> Html a
framePanel name html =
    div [ class "metric-panel" ]
        [ h2 [] [ text name ]
        , div [ class "metrics" ] html
        ]



-- UTILS --


fmtInt : Int -> String
fmtInt n =
    thousands (toString n)


thousands : String -> String
thousands str =
    if String.length str <= 3 then
        str
    else
        (thousands (String.slice 0 -3 str)) ++ "," ++ (String.right 3 str)
