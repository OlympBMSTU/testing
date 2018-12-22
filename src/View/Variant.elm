module View.Variant exposing (Model, Msg(..), getVariant, init, problemView, subscriptions, update, updateVariant, view)

import API
import Bootstrap.Badge as Badge
import Bootstrap.Button as Button
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.Modal as Modal
import Bootstrap.Utilities.Spacing as Spacing
import Data.Problem exposing (..)
import Data.Variant exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import List
import List.Extra as List
import Maybe
import Regex
import RemoteData exposing (RemoteData(..), WebData)
import String
import Task
import Time


type alias Model =
    { token : API.Token
    , variant : WebData Variant
    , valid : List Bool
    , modalVisibility : Modal.Visibility
    }


init : API.Token -> ( Model, Cmd Msg )
init token =
    ( Model token NotAsked [] Modal.hidden, getVariant token )


type Msg
    = LostFocus
    | InputAnswer Int String
    | GetVariant (WebData Variant)
    | UpdatedVariant (WebData ())
    | CommitVariant
    | CommitedVariant (WebData ())
    | Tick Time.Posix
    | CloseModal


getVariant : API.Token -> Cmd Msg
getVariant token =
    API.getVariantRequest token
        |> RemoteData.sendRequest
        |> Cmd.map GetVariant


updateVariant : API.Token -> Variant -> Cmd Msg
updateVariant token variant =
    API.updateVariantRequest token variant
        |> RemoteData.sendRequest
        |> Cmd.map UpdatedVariant


commitVariant : API.Token -> Variant -> Cmd Msg
commitVariant token variant =
    API.commitVariantRequest token variant
        |> RemoteData.sendRequest
        |> Cmd.map CommitedVariant


numberRegex : Regex.Regex
numberRegex =
    Maybe.withDefault Regex.never <| Regex.fromString "^[+-]?\\d+(\\,\\d+)?$"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model.variant ) of
        ( LostFocus, Success variant ) ->
            let
                zip =
                    List.zip variant.problems model.valid

                ( cleanedProblems, _ ) = List.unzip <|
                    List.map clean zip

                clean ( problem, valid ) =
                    if valid then
                        ( problem, True )

                    else
                        ( { problem | answer = "" }, True )

                cleanedVariant =
                    { variant | problems = cleanedProblems }
            in
            ( model, updateVariant model.token cleanedVariant )

        ( InputAnswer i answer, Success variant ) ->
            let
                problems =
                    variant.problems

                mProblem =
                    List.getAt i problems

                isNumber =
                    if answer == "" then
                        True

                    else
                        Regex.contains numberRegex answer
            in
            case mProblem of
                Just problem ->
                    let
                        updatedProblem =
                            { problem | answer = answer }

                        updatedProblems =
                            List.setAt i updatedProblem problems

                        updatedVariant =
                            { variant | problems = updatedProblems }

                        valid =
                            List.setAt i isNumber model.valid
                    in
                    ( { model | variant = Success updatedVariant, valid = valid }, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        ( GetVariant result, _ ) ->
            let
                valid =
                    case result of
                        Success variant ->
                            List.repeat (List.length variant.problems) True

                        _ ->
                            []
            in
            ( { model | variant = result, valid = valid }, Cmd.none )

        ( CommitVariant, Success variant ) ->
            if List.all identity model.valid then
                let
                    doneVariant =
                        { variant | done = True }

                    doneModel =
                        { model | variant = Success doneVariant }
                in
                ( doneModel, commitVariant model.token doneVariant )

            else
                ( { model | modalVisibility = Modal.shown }, Cmd.none )

        ( CommitedVariant (Success _), Success variant ) ->
            let
                updatedVariant =
                    { variant | done = True }
            in
            ( { model | variant = Success updatedVariant }, Cmd.none )

        ( Tick _, Success variant ) ->
            let
                updatedVariant =
                    { variant | timeLeft = variant.timeLeft - 1 }
            in
            if updatedVariant.timeLeft == 0 then
                let
                    doneVariant =
                        { updatedVariant | done = True }
                in
                ( { model | variant = Success doneVariant }, commitVariant model.token doneVariant )

            else
                ( { model | variant = Success updatedVariant }, Cmd.none )

        ( CloseModal, _ ) ->
            ( { model | modalVisibility = Modal.hidden }, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 Tick


view : Model -> Html.Html Msg
view model =
    div
        []
        [ viewVariant model
        , Modal.config CloseModal
            |> Modal.small
            |> Modal.hideOnBackdropClick True
            |> Modal.h3 [] [ text "Ошибка" ]
            |> Modal.body [] [ p [] [ text "Некорректный формат некоторых полей" ] ]
            |> Modal.footer []
                [ Button.button
                    [ Button.outlinePrimary
                    , Button.attrs [ onClick CloseModal ]
                    ]
                    [ text "Закрыть" ]
                ]
            |> Modal.view model.modalVisibility
        ]


viewVariant : Model -> Html.Html Msg
viewVariant model =
    case model.variant of
        NotAsked ->
            text "Инициализация."

        Loading ->
            text "Загрузка."

        Failure err ->
            text "Ошибка. Возможно у вас нет активных тестов или время вышло. Попробуйте вернуться в личный кабинет"

        Success variant ->
            if variant.done then
                viewDone

            else
                div [] <|
                    List.indexedMap problemView (List.zip variant.problems model.valid)
                        ++ [ variantCommit, viewTimer variant.timeLeft ]


zeroPadNumber ticks =
    String.fromInt ticks |> String.padLeft 2 '0'


viewTimer : Int -> Html.Html Msg
viewTimer ticks =
    let
        seconds =
            modBy 60 ticks |> zeroPadNumber

        minutes =
            modBy 3600 ticks // 60 |> zeroPadNumber

        hours =
            ticks // 3600 |> zeroPadNumber

        time =
            hours ++ ":" ++ minutes ++ ":" ++ seconds
    in
    h1
        [ style "position" "fixed"
        , style "top" "0px"
        , style "left" "0px"
        ]
        [ Badge.badgePrimary [ Spacing.m2 ] [ text <| time ] ]


viewDone : Html.Html Msg
viewDone =
    Grid.container []
        [ Grid.row
            [ Row.attrs [ Spacing.m2 ], Row.centerMd ]
            [ Grid.col [ Col.md8 ]
                [ text "Завершено"
                ]
            ]
        ]


variantCommit : Html.Html Msg
variantCommit =
    Grid.container []
        [ Grid.row
            [ Row.attrs [ Spacing.m2 ], Row.centerMd ]
            [ Grid.col [ Col.md12 ]
                [ Button.button
                    [ Button.primary
                    , Button.onClick CommitVariant
                    , Button.attrs [ class "float-right" ]
                    ]
                    [ text "Завершить тест" ]
                ]
            ]
        ]


problemView : Int -> ( Problem, Bool ) -> Html.Html Msg
problemView i ( problem, valid ) =
    let
        classes =
            if problem.answer == "" then
                "form-control"

            else if valid then
                "form-control is-valid"

            else
                "form-control is-invalid"

        validateMessage =
            if valid then
                ""

            else
                "Некорректный формат"
    in
    Grid.container []
        [ Grid.row [ Row.attrs [ Spacing.mt5 ], Row.centerMd ]
            [ Grid.col [ Col.md12 ]
                [ iframe
                    [ style "height" "600px"
                    , style "width" "100%"
                    , src ("https://olymp.bmstu.ru/exercises/files/" ++ problem.url)
                    ]
                    []
                ]
            ]
        , Grid.row [ Row.attrs [ Spacing.mb5 ], Row.centerMd ]
            [ Grid.col [ Col.md1 ]
                [ p [ Spacing.p2 ] [ text "Ответ:" ] ]
            , Grid.col [ Col.md11 ]
                [ input
                    [ style "width" "100%"
                    , class classes
                    , onBlur LostFocus
                    , onInput (InputAnswer i)
                    , value problem.answer 
                    ]
                    [ ]
                , div
                    [ class "invalid-feedback" ]
                    [ text validateMessage ]
                ]
            ]
        ]
