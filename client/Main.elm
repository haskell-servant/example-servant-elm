module Main exposing (FromServer(..), FromUi(..), ItemId, Model, Msg(..), fromServer, init, main, update, view, viewItem)

import Browser
import Api exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Html.Events exposing (..)
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


type alias ItemId =
    Int


init : ( Model, Cmd Msg )
init =
    let
        fetch =
            Http.send (fromServer Initial) Api.getApiItem

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
    = Initial (List ItemId)
    | NewItem Item
    | Delete ItemId


type FromUi
    = AddItemInputChange String
    | AddItemButton
    | Done ItemId


update : Msg -> Model -> ( Model, Cmd Msg )
update message s =
    case message of
        FromServer fromServerMsg ->
            case fromServerMsg of
                Initial itemIds ->
                    ( s
                    , itemIds
                        |> List.map getApiItemByItemId
                        |> List.map (Http.send (fromServer NewItem))
                        |> Cmd.batch
                    )

                NewItem item ->
                    ( { s | items = insert item.id item s.items }
                    , Cmd.none
                    )

                Delete id ->
                    ( { s | items = remove id s.items }
                    , Cmd.none
                    )

        FromUi fromUi ->
            case fromUi of
                AddItemButton ->
                    let
                        new =
                            s.addItemInput

                        cmd =
                            Http.send (fromServer (\id -> NewItem (Item id new))) (postApiItem new)

                        newState =
                            { s | addItemInput = "" }
                    in
                    if new == "" then
                        update (Error "empty field") s

                    else
                        ( newState, cmd )

                AddItemInputChange t ->
                    ( { s | addItemInput = t }
                    , Cmd.none
                    )

                Done id ->
                    ( s, Http.send (fromServer (\NoContent -> Delete id)) (deleteApiItemByItemId id) )

        Error msg ->
            ( { s | error = Just msg }, Cmd.none )


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

        Http.BadStatus response ->
            "bad status: " ++ response.status.message

        Http.BadPayload debugString response ->
            "bad payload: " ++ debugString


-- VIEW


view : Model -> Html Msg
view state =
    div [] <|
        [ text (Debug.toString state)
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
