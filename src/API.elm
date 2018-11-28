module API exposing (Token, commitVariantRequest, encodeToken, getVariantRequest, mkTokenHeader, updateVariantRequest)

import Data.Problem exposing (..)
import Data.Variant exposing (..)
import Http
import Json.Encode as E


type alias Token =
    String


encodeToken : Token -> E.Value
encodeToken =
    E.string


getVariantRequest : Token -> Http.Request Variant
getVariantRequest token =
    Http.request
        { method = "GET"
        , headers = mkTokenHeader token
        , url = "https://olymp.bmstu.ru/api/testing/get"
        , body = Http.emptyBody
        , expect = Http.expectJson decodeVariant
        , timeout = Nothing
        , withCredentials = True
        }


updateVariantRequest : Token -> Variant -> Http.Request ()
updateVariantRequest token variant =
    Http.request
        { method = "POST"
        , headers = mkTokenHeader token
        , url = "https://olymp.bmstu.ru/api/testing/update"
        , body = Http.jsonBody (encodeVariantAnswer variant)
        , expect = Http.expectStringResponse << always <| Ok ()
        , timeout = Nothing
        , withCredentials = True
        }


commitVariantRequest : Token -> Variant -> Http.Request ()
commitVariantRequest token variant =
    Http.request
        { method = "POST"
        , headers = mkTokenHeader token
        , url = "https://olymp.bmstu.ru/api/testing/commit"
        , body = Http.jsonBody (encodeVariantAnswer variant)
        , expect = Http.expectStringResponse << always <| Ok ()
        , timeout = Nothing
        , withCredentials = True
        }


mkTokenHeader token =
    [ Http.header "Authorization" ("Bearer " ++ token) ]
