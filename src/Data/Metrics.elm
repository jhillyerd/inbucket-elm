module Data.Metrics exposing (..)

import Json.Decode as Decode exposing (..)
import Json.Decode.Pipeline exposing (..)


type alias Metrics =
    { sysMem : Int
    , heapSize : Int
    , heapInUse : Int
    , heapObjects : Int
    , goRoutines : Int
    , webSockets : Int
    , smtpCurrent : Int
    }


decoder : Decoder Metrics
decoder =
    decode Metrics
        |> requiredAt [ "memstats", "Sys" ] int
        |> requiredAt [ "memstats", "HeapAlloc" ] int
        |> requiredAt [ "memstats", "HeapSys" ] int
        |> requiredAt [ "memstats", "HeapObjects" ] int
        |> requiredAt [ "goroutines" ] int
        |> requiredAt [ "http", "WebSocketConnectsCurrent" ] int
        |> requiredAt [ "smtp", "ConnectsCurrent" ] int
