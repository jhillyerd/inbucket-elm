module Page.Status exposing (Model, Msg, init, load, subscriptions, update, view)

import Data.Metrics as Metrics exposing (Metrics)
import Data.Session as Session exposing (Session)
import Filesize
import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (Error)
import HttpUtil
import Sparkline exposing (sparkline, Point, DataSet, Size)
import Svg.Attributes as SvgAttrib
import Time exposing (Time)


-- MODEL --


type alias Model =
    { metrics : Maybe Metrics
    , xCounter : Float
    , sysMem : Metric
    , heapSize : Metric
    , heapUsed : Metric
    , heapObjects : Metric
    , goRoutines : Metric
    , webSockets : Metric
    }


type alias Metric =
    { label : String
    , value : Int
    , formatter : Int -> String
    , graph : DataSet -> Html Msg
    , history : DataSet
    , minutes : Int
    }


init : Session -> Model
init session =
    { metrics = Nothing
    , xCounter = 60
    , sysMem = Metric "System Memory" 0 Filesize.format graphZero initDataSet 10
    , heapSize = Metric "Heap Size" 0 Filesize.format graphZero initDataSet 10
    , heapUsed = Metric "Heap Used" 0 Filesize.format graphZero initDataSet 10
    , heapObjects = Metric "Heap # Objects" 0 fmtInt graphZero initDataSet 10
    , goRoutines = Metric "Goroutines" 0 fmtInt graphZero initDataSet 10
    , webSockets = Metric "Open WebSockets" 0 fmtInt graphZero initDataSet 10
    }


initDataSet : DataSet
initDataSet =
    List.range 0 59
        |> List.map (\x -> ( toFloat (x), 0 ))


load : Cmd Msg
load =
    getMetrics



-- SUBSCRIPTIONS --


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (1 * Time.second) Tick



-- UPDATE --


type Msg
    = NewMetrics (Result Http.Error Metrics)
    | Tick Time


update : Session -> Msg -> Model -> ( Model, Cmd Msg, Session.Msg )
update session msg model =
    case msg of
        NewMetrics (Ok metrics) ->
            ( updateMetrics metrics model, Cmd.none, Session.None )

        NewMetrics (Err err) ->
            ( model, Cmd.none, Session.SetFlash (HttpUtil.errorString err) )

        Tick time ->
            ( model, getMetrics, Session.ClearFlash )


{-| Update all metrics in Model; increment xCounter.
-}
updateMetrics : Metrics -> Model -> Model
updateMetrics metrics model =
    let
        x =
            model.xCounter
    in
        { model
            | metrics = Just metrics
            , xCounter = x + 1
            , sysMem = updateMetric model.sysMem x metrics.sysMem
            , heapSize = updateMetric model.heapSize x metrics.heapSize
            , heapUsed = updateMetric model.heapUsed x metrics.heapUsed
            , heapObjects = updateMetric model.heapObjects x metrics.heapObjects
            , goRoutines = updateMetric model.goRoutines x metrics.goRoutines
            , webSockets = updateMetric model.webSockets x metrics.webSockets
        }


{-| Update a single Metric.
-}
updateMetric : Metric -> Float -> Int -> Metric
updateMetric metric x value =
    { metric
        | value = value
        , history = addPoint metric.history ( x, (toFloat value) )
    }


addPoint : DataSet -> Point -> DataSet
addPoint data point =
    case List.tail data of
        Just newData ->
            newData ++ [ point ]

        Nothing ->
            [ point ]


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
                        [ viewMetric model.sysMem
                        , viewMetric model.heapSize
                        , viewMetric model.heapUsed
                        , viewMetric model.heapObjects
                        , viewMetric model.goRoutines
                        , viewMetric model.webSockets
                        ]
                    , framePanel "SMTP Metrics"
                        [ viewLiveMetric "Open Connections" fmtInt metrics.smtpCurrent graphNull
                        , viewLiveMetric "Total Connections" fmtInt metrics.smtpConnectsTotal graphNull
                        , viewLiveMetric "Messages Received" fmtInt metrics.smtpReceivedTotal graphNull
                        , viewLiveMetric "Errors Logged" fmtInt metrics.smtpErrorsTotal graphNull
                        , viewLiveMetric "Warnings Logged" fmtInt metrics.smtpWarnsTotal graphNull
                        ]
                    ]
        ]


viewMetric : Metric -> Html Msg
viewMetric metric =
    div [ class "metric" ]
        [ div [ class "label" ] [ text metric.label ]
        , div [ class "value" ] [ text (metric.formatter metric.value) ]
        , div [ class "graph" ]
            [ metric.graph metric.history
            , text ("(" ++ toString metric.minutes ++ "min)")
            ]
        ]


viewLiveMetric : String -> (Int -> String) -> Int -> Html a -> Html a
viewLiveMetric label formatter value graph =
    div [ class "metric" ]
        [ div [ class "label" ] [ text label ]
        , div [ class "value" ] [ text (formatter value) ]
        , div [ class "graph" ]
            [ graph
            , text "(10min)"
            ]
        ]


graphNull : Html a
graphNull =
    div [] []


graphSize : Size
graphSize =
    ( 180, 16, 0, 0 )


graphStyle : Sparkline.Param a -> Sparkline.Param a
graphStyle =
    Sparkline.Style
        [ SvgAttrib.fill "rgba(50,100,255,0.3)"
        , SvgAttrib.stroke "rgba(50,100,255,1.0)"
        , SvgAttrib.strokeWidth "1.0"
        , SvgAttrib.alignmentBaseline "baseline"
        ]


zeroStyle : Sparkline.Param a -> Sparkline.Param a
zeroStyle =
    Sparkline.Style
        [ SvgAttrib.stroke "rgba(0,0,0,0.2)"
        , SvgAttrib.strokeWidth "1.0"
        ]



-- graphFloat : DataSet -> Html a
-- graphFloat data =
--     sparkline graphSize [ Sparkline.Line data |> graphStyle ]


graphZero : DataSet -> Html a
graphZero data =
    let
        -- Used with Domain to stop sparkline forgetting about zero; continue scrolling graph.
        x =
            case List.head data of
                Nothing ->
                    0

                Just point ->
                    Tuple.first point
    in
        sparkline graphSize
            [ Sparkline.Area data |> graphStyle
            , Sparkline.ZeroLine |> zeroStyle
            , Sparkline.Domain [ ( x, 0 ), ( x, 1 ) ]
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
