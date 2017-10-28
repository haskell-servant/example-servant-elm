module Main exposing (..)

import Dict exposing (..)
import Html exposing (..)
import Html exposing (program)
import Html.Events exposing (..)
import Http
import Api exposing (..)


main : Program Never Model Msg
main =
    program
        { init = init
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


type alias ItemId =
    Int


init : ( Model, Cmd Msg )
init =
    let
        fetch =
            Http.send (FromServer << Initial) Api.getApiItem

        state =
            { items = empty, addItemInput = "", error = Nothing }
    in
        ( state, fetch )



-- UPDATE


type Msg
    = FromServer FromServer
    | FromUi FromUi
    | Error String


type FromServer
    = Initial (Result Http.Error (List ItemId))
    | NewItem (Result Http.Error Item)
    | CreatedItem (Result Http.Error ItemId)
    | Delete (Result Http.Error ItemId)


type FromUi
    = AddItemInputChange String
    | AddItemButton
    | Done ItemId


update : Msg -> Model -> ( Model, Cmd Msg )
update message s =
    case message of
        FromServer fromServer ->
            case fromServer of
                Initial result ->
                    case result of
                        Ok itemIds ->
                            let
                                cmd : Cmd Msg
                                cmd =
                                    itemIds
                                        |> List.map getApiItemByItemId
                                        |> List.map (Http.send (FromServer << NewItem))
                                        |> Cmd.batch
                            in
                                ( s, cmd )

                        Err error ->
                            update (Error <| toString error) s

                NewItem result ->
                    case result of
                        Ok item ->
                            { s | items = insert item.id item s.items } ! []

                        Err error ->
                            update (Error <| toString error) s

                CreatedItem result ->
                    case result of
                        Ok itemId ->
                            s ! [ Http.send (FromServer << NewItem) (getApiItemByItemId itemId) ]

                        Err error ->
                            update (Error <| toString error) s

                Delete result ->
                    case result of
                        Ok id ->
                            { s | items = remove id s.items } ! []

                        Err error ->
                            update (Error <| toString error) s

        FromUi fromUi ->
            case fromUi of
                AddItemButton ->
                    let
                        new =
                            s.addItemInput

                        cmd =
                            Http.send (FromServer << CreatedItem) (postApiItem new)

                        newState =
                            { s | addItemInput = "" }
                    in
                        if new == "" then
                            update (Error "empty field") s
                        else
                            ( newState, cmd )

                AddItemInputChange t ->
                    { s | addItemInput = t } ! []

                Done id ->
                    let
                        cmd =
                            Http.send (FromServer << Delete) (deleteApiItemByItemId id)
                    in
                        ( s, cmd )

        Error msg ->
            ( { s | error = Just msg }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view state =
    div [] <|
        [ text (toString state)
        , br [] []
        ]
            ++ List.map (viewItem << Tuple.second) (toList state.items)
            ++ [ input [ onInput (FromUi << AddItemInputChange) ] []
               , button [ onClick (FromUi AddItemButton) ] [ text "add item" ]
               ]


viewItem : Item -> Html Msg
viewItem item =
    div [] <|
        [ text item.text
        , text " - "
        , button [ onClick (FromUi <| Done item.id) ] [ text "done" ]
        ]
