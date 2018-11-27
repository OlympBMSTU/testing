module Data.Variant exposing (Variant, VariantId, decodeVariant, decodeVariantId, encodeVariant, encodeVariantId, encodeVariantAnswer)

import Data.Problem as Data
import Json.Decode as D
import Json.Encode as E


type alias VariantId =
    String


encodeVariantId : VariantId -> E.Value
encodeVariantId =
    E.string


decodeVariantId : D.Decoder VariantId
decodeVariantId =
    D.string


type alias Variant =
    { id : VariantId
    , timeLeft : Int
    , done : Bool
    , problems : List Data.Problem
    }


encodeVariant : Variant -> E.Value
encodeVariant variant =
    E.object
        [ ( "id", encodeVariantId variant.id )
        , ( "time_left", E.int variant.timeLeft )
        , ( "done", E.bool variant.done )
        , ( "problems", E.list Data.encodeProblem variant.problems )
        ]

encodeVariantAnswer : Variant -> E.Value
encodeVariantAnswer variant =
    E.object
        [ ( "id", encodeVariantId variant.id )
        , ( "time_left", E.int variant.timeLeft )
        , ( "done", E.bool variant.done )
        , ( "problems", E.list Data.encodeProblemAnswer variant.problems )
        ]

decodeVariant : D.Decoder Variant
decodeVariant =
    D.map4 Variant
        (D.field "id" decodeVariantId)
        (D.field "time_left" D.int)
        (D.field "done" D.bool)
        (D.field "problems" (D.list Data.decodeProblem))
