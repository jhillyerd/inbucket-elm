module Data.MessageHeader exposing (..)

import Json.Decode as Decode exposing (..)


type alias MessageHeader =
    { mailbox : String
    , id : String
    , from : String
    , to : List String
    , subject : String
    , date : String
    , size : Int
    , seen : Bool
    }


decoder : Decoder MessageHeader
decoder =
    map8
        MessageHeader
        (field "mailbox" string)
        (field "id" string)
        (field "from" string)
        (field "to" (list string))
        (field "subject" string)
        (field "date" string)
        (field "size" int)
        (field "seen" bool)
