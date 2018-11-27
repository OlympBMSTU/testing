module Main exposing (Flags, Model, Msg(..), init, main, subscriptions, update, view, reactor)

import API
import Browser
import Data.Problem exposing (..)
import Data.Variant exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Http
import List
import List.Extra as List
import RemoteData exposing (RemoteData(..), WebData)
import View.Variant as Variant

import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Navbar as Navbar



type alias Flags =
    { token : String}


defaultFlags = {token = "1234"}

reactor : Program () Model Msg
reactor =
    Browser.document
        { init = \_ -> init defaultFlags
        , view = view
        , update = update
        , subscriptions = subscriptions
        }

main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { token : String
    , variant : Variant.Model
    , navbarState : Navbar.State
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        token =
            flags.token

        (variantModel, variantCmd) =
            Variant.init token

        (navbarState, navbarCmd)
            = Navbar.initialState NavbarMsg
    in
    ( Model token variantModel navbarState, Cmd.batch [Cmd.map VariantMsg variantCmd, navbarCmd] )


type Msg
    = VariantMsg Variant.Msg
    | NavbarMsg Navbar.State


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        VariantMsg vmsg ->
            let
                ( updatedVariant, cmd ) =
                    Variant.update vmsg model.variant
            in
            ( { model | variant = updatedVariant }, Cmd.map VariantMsg cmd )
        NavbarMsg state ->
            ( { model | navbarState = state }, Cmd.none )



subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Navbar.subscriptions model.navbarState NavbarMsg
        , Sub.map VariantMsg (Variant.subscriptions model.variant)
    ]

viewNavbar : Model -> Html Msg
viewNavbar model =
    Grid.container [][
        Navbar.config NavbarMsg
            |> Navbar.withAnimation
            |> Navbar.brand [ href "#"]
                [img
                    [ src "/bmstu.png"
                    , class "d-inline-block align-top"
                    , style "width" "30px"
                    ]
                    []
                , text " Тестирование"
                ]
            |> Navbar.view model.navbarState]


view : Model -> Browser.Document Msg
view model =
    { title = "Тестирование"
    , body =
        [ viewNavbar model
        , Grid.container []
            [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
            , Html.map VariantMsg <| Variant.view model.variant]
        ]
    }
