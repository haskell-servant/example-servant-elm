module Main exposing (..)

import Debug exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Task exposing (Task, perform)
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
            Http.send Init <| Api.getApiItem

        state =
            { items = empty, addItemInput = "", error = Nothing }
    in
        ( state, fetch )



-- UPDATE


type Msg
    = Init (Result Http.Error (List Int))
    | NewItem (Result Http.Error Item)
    | Error String



-- type FromServer
--     = Initial (List ItemId)
--     | NewItem Item
--     | Delete ItemId
--
--
-- type FromUi
--     = AddItemInputChange String
--     | AddItemButton
--     | Done ItemId


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Init (Ok itemIds) ->
            (model ! getAllItems itemIds)

        Init (Err err) ->
            ( { model | error = handleHttpError err }, Cmd.none )

        NewItem (Ok item) ->
            let
                items_ =
                    insert item.id item model.items
            in
                ( { model | items = items_ }, Cmd.none )

        NewItem (Err err) ->
            ( { model | error = handleHttpError err }, Cmd.none )

        Error msg ->
            ( { model | error = Just msg }, Cmd.none )


getAllItems : List Int -> List (Cmd Msg)
getAllItems itemIds =
    List.map
        (\itemId ->
            Http.send NewItem <| Api.getApiItemByItemId itemId
        )
        itemIds


handleHttpError : Http.Error -> Maybe String
handleHttpError err =
    case err of
        Http.BadUrl message ->
            Just message

        Http.Timeout ->
            Just "A timeout happened"

        Http.NetworkError ->
            Just "A NetworkError occurred"

        Http.BadStatus status ->
            Just status.status.message

        Http.BadPayload msg _ ->
            Just msg



-- toServer : (a -> FromServer) -> Task Http.Error a -> Cmd Msg
-- toServer tag task =
--     perform (Error << toString) (FromServer << tag) task
-- VIEW


view : Model -> Html Msg
view state =
    div [] <|
        [ text (toString state)
        , br [] []
        ]



-- ++ (List.map viewItem (toList state.items))
-- ++ [ input [ onInput (FromUi << AddItemInputChange) ] []
--    , button [ onClick (FromUi AddItemButton) ] [ text "add item" ]
--    ]


viewItem : Int -> Item -> Html Msg
viewItem index item =
    div [] <|
        [ text (item.text)
        , text " - "
          -- , button [ onClick (FromUi <| Done item.id) ] [ text "done" ]
        ]
