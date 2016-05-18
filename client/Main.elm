
import           Debug exposing (..)
import           Dict exposing (..)
import           Html exposing (..)
import           Html.App exposing (program)
import           Html.Attributes exposing (..)
import           Html.Events exposing (..)
import           Http
import           Task exposing (Task, perform)

import           Api exposing (..)

main : Program Never
main = program {
    init = init,
    update = update,
    subscriptions = \ _ -> Sub.none,
    view = view
  }

type Either left right
  = Left left
  | Right right

type alias State = {
    items : Dict Int Item,
    addItemInput : String,
    error : Maybe String
  }

type alias ItemId = Int

type Message
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

init : (State, Cmd Message)
init =
  let
    fetch = toServer Initial Api.getApiItem
    state = {items = empty, addItemInput = "", error = Nothing}
  in (state, fetch)

update : Message -> State -> (State, Cmd Message)
update message s = case message of
  FromServer fromServer -> case fromServer of
    Initial itemIds ->
      let
        cmd : Cmd Message
        cmd = Cmd.batch <|
          List.map (toServer NewItem << getApiItemByItemId) itemIds
      in (s, cmd)
    NewItem item -> noop {s | items = insert item.id item s.items}
    Delete id -> noop {s | items = remove id s.items}

  FromUi fromUi -> case fromUi of
    AddItemButton ->
      let
        new = s.addItemInput
        cmd = toServer (\ id -> NewItem (Item id new)) (postApiItem new)
        newState = {s | addItemInput = ""}
      in if new == ""
        then update (Error "empty field") s
        else (newState, cmd)
    AddItemInputChange t -> noop {s | addItemInput = t}
    Done id ->
      let
        cmd = toServer (always (Delete id)) (deleteApiItemByItemId id)
      in (s, cmd)

  Error msg -> ({s | error = Just msg}, Cmd.none)

noop : s -> (s, Cmd m)
noop s = (s, Cmd.none)

toServer : (a -> FromServer) -> Task Http.Error a -> Cmd Message
toServer tag task = perform (Error << toString) (FromServer << tag) task

view : State -> Html Message
view state =
  div [] <|
    text (toString state) ::
    br [] [] ::
    (List.map (viewItem << snd) (toList state.items)) ++
    input [onInput (FromUi << AddItemInputChange)] [] ::
    button [onClick (FromUi AddItemButton)] [text "add item"] ::
    []

viewItem : Item -> Html Message
viewItem item = div [] <|
  text (item.text) ::
  text " - " ::
  button [onClick (FromUi <| Done item.id)] [text "done"] ::
  []
