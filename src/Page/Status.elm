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


-- import Html.Attributes exposing (..)
-- import Html.Events exposing (..)
-- MODEL --


type alias Model =
    { metrics : Maybe Metrics
    , xCounter : Float
    , heapObjects : DataSet
    , goRoutines : DataSet
    }


init : Session -> Model
init session =
    { metrics = Nothing
    , xCounter = 60
    , heapObjects = initDataSet
    , goRoutines = initDataSet
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
    Time.every (5 * Time.second) Tick



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


updateMetrics : Metrics -> Model -> Model
updateMetrics metrics model =
    let
        x =
            model.xCounter
    in
        { model
            | metrics = Just metrics
            , xCounter = x + 1
            , heapObjects = addPoint model.heapObjects ( x, toFloat (metrics.heapObjects) )
            , goRoutines = addPoint model.goRoutines ( x, toFloat (metrics.goRoutines) )
        }


addPoint : DataSet -> Point -> DataSet
addPoint data point =
    data ++ [ point ]


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
                        [ viewLiveMetric "System Memory" Filesize.format metrics.sysMem graphNull
                        , viewLiveMetric "Heap Size" Filesize.format metrics.heapSize graphNull
                        , viewLiveMetric "Heap In-Use" Filesize.format metrics.heapInUse graphNull
                        , viewLiveMetric "Heap # Objects"
                            fmtInt
                            metrics.heapObjects
                            (graphFloat model.heapObjects)
                        , viewLiveMetric "Goroutines"
                            fmtInt
                            metrics.goRoutines
                            (graphZero
                                model.goRoutines
                            )
                        , viewLiveMetric "Open WebSockets" fmtInt metrics.webSockets graphNull
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
        [ SvgAttrib.stroke "rgba(100,100,255,1.0)"
        , SvgAttrib.strokeWidth "1.5"
        ]


zeroStyle : Sparkline.Param a -> Sparkline.Param a
zeroStyle =
    Sparkline.Style
        [ SvgAttrib.stroke "rgba(200,200,200,1.0)"
        , SvgAttrib.strokeWidth "1.5"
        ]


graphFloat : DataSet -> Html a
graphFloat data =
    sparkline graphSize [ Sparkline.Line data |> graphStyle ]


graphZero : DataSet -> Html a
graphZero data =
    sparkline graphSize
        [ Sparkline.ZeroLine |> zeroStyle
        , Sparkline.Line data |> graphStyle
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
