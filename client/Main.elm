module Main exposing (FromServer(..), FromUi(..), Model, Msg(..), fromServer, init, main, update, view, viewItem)

import Api exposing (..)
import Browser
import Dict exposing (Dict)
import Html exposing (Html, button, div, input, li, text, ul)
import Html.Attributes exposing (value)
import Html.Events exposing (onClick, onInput)
import Http


main : Program () Model Msg
main =
    Browser.element
        { init = \() -> init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



-- MODEL


type alias Model =
    { items : Dict Int Item
    , addItemInput : String
    , error : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( Model Dict.empty "" Nothing
    , Api.getApiItem (fromServer Initial)
    )



-- UPDATE


type Msg
    = FromServer FromServer
    | FromUi FromUi
    | Error String


type FromServer
    = Initial (List ItemId)
    | NewItem Item
    | Delete ItemId


type FromUi
    = AddItemInputChange String
    | AddItemButton
    | Done ItemId


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FromServer fromServerMsg ->
            case fromServerMsg of
                Initial itemIds ->
                    ( model
                    , itemIds
                        |> List.map (\id -> getApiItemByItemId id (fromServer NewItem))
                        |> Cmd.batch
                    )

                NewItem item ->
                    ( { model | items = Dict.insert item.id item model.items }
                    , Cmd.none
                    )

                Delete id ->
                    ( { model | items = Dict.remove id model.items }
                    , Cmd.none
                    )

        FromUi fromUi ->
            case fromUi of
                AddItemButton ->
                    let
                        itemName =
                            model.addItemInput
                    in
                    if itemName == "" then
                        update (Error "empty field") model

                    else
                        ( { model | addItemInput = "" }
                        , postApiItem itemName (fromServer (\id -> NewItem (Item id itemName)))
                        )

                AddItemInputChange t ->
                    ( { model | addItemInput = t, error = Nothing }
                    , Cmd.none
                    )

                Done id ->
                    ( model
                    , deleteApiItemByItemId id (fromServer (\() -> Delete id))
                    )

        Error error ->
            ( { model | error = Just error }, Cmd.none )


fromServer : (a -> FromServer) -> Result Http.Error a -> Msg
fromServer msgConstructor result =
    case result of
        Ok content ->
            FromServer <| msgConstructor content

        Err error ->
            Error <| httpErrorToString error


httpErrorToString : Http.Error -> String
httpErrorToString error =
    case error of
        Http.BadUrl s ->
            "bad url: " ++ s

        Http.Timeout ->
            "timeout"

        Http.NetworkError ->
            "network error"

        Http.BadStatus status ->
            "bad status: " ++ String.fromInt status

        Http.BadBody response ->
            "bad payload: " ++ response



-- VIEW


view : Model -> Html Msg
view model =
    let
        items =
            List.map (viewItem << Tuple.second) (Dict.toList model.items)

        error =
            model.error
                |> Maybe.map viewError
                |> Maybe.withDefault (Html.text "")
    in
    div []
        [ ul [] items
        , input [ onInput (FromUi << AddItemInputChange), value model.addItemInput ] []
        , button [ onClick (FromUi AddItemButton) ] [ text "add item" ]
        , error
        ]


viewItem : Item -> Html Msg
viewItem item =
    li []
        [ text item.text
        , text " - "
        , button [ onClick (FromUi <| Done item.id) ] [ text "done" ]
        ]


viewError : String -> Html msg
viewError error =
    div
        []
        [ text <| "Error: " ++ error ]
