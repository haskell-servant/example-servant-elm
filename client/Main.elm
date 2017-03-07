module Main exposing (..)

import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
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
            Http.send Init <| Api.getApiItem

        state =
            { items = empty, addItemInput = "", error = Nothing }
    in
        ( state, fetch )



-- UPDATE


type Msg
    = Init (Result Http.Error (List Int))
    | CreateNewItem (Result Http.Error Int)
    | GotNewItem (Result Http.Error Item)
    | DeleteItem (Result Http.Error NoContent)
    | AddItemInputChange String
    | AddItemButton
    | Done ItemId
    | Error String


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Init (Ok itemIds) ->
            (model ! getAllItems itemIds)

        Init (Err err) ->
            ( { model | error = handleHttpError err }, Cmd.none )

        CreateNewItem (Ok itemId) ->
            (model ! getAllItems [ itemId ])

        CreateNewItem (Err err) ->
            ( { model | error = handleHttpError err }, Cmd.none )

        GotNewItem (Ok item) ->
            let
                items_ =
                    insert item.id item model.items
            in
                ( { model | items = items_ }, Cmd.none )

        GotNewItem (Err err) ->
            ( { model | error = handleHttpError err }, Cmd.none )

        DeleteItem (Ok _) ->
            ( model, Cmd.none )

        DeleteItem (Err err) ->
            ( { model | error = handleHttpError err }, Cmd.none )

        AddItemButton ->
            if (model.addItemInput /= "") then
                ( { model | addItemInput = "" }, Http.send CreateNewItem <| postApiItem model.addItemInput )
            else
                ( model, Cmd.none )

        AddItemInputChange addItemInput ->
            ( { model | addItemInput = addItemInput }, Cmd.none )

        Done itemId ->
            let
                items =
                    (Dict.remove itemId model.items)
            in
                ( { model | items = items }, Http.send DeleteItem <| deleteApiItemByItemId itemId )

        Error msg ->
            ( { model | error = Just msg }, Cmd.none )


getAllItems : List Int -> List (Cmd Msg)
getAllItems itemIds =
    List.map
        (\itemId ->
            Http.send GotNewItem <| Api.getApiItemByItemId itemId
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



-- VIEW


view : Model -> Html Msg
view state =
    div [] <|
        [ input [ onInput (AddItemInputChange), value state.addItemInput ] []
        , button [ onClick AddItemButton ] [ text "add item" ]
        ]
            ++ (List.map viewItem (toList state.items))


viewItem : ( Int, Item ) -> Html Msg
viewItem ( index, item ) =
    div [] <|
        [ text (item.text)
        , text " - "
        , button [ onClick <| Done item.id ] [ text "done" ]
        ]
