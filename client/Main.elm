
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
  = Initial (List ItemId)
  | NewItem Item
  | Error String
  | AddItemButton
  | AddItemInputChange String
  | Done ItemId
  | Delete ItemId

init : (State, Cmd Message)
init =
  let
    fetch = perform (Error << toString) Initial Api.getApiItem
    state = {items = empty, addItemInput = "", error = Nothing}
  in (state, fetch)

update : Message -> State -> (State, Cmd Message)
update message s = case message of
  Initial itemIds ->
    let
      cmd : Cmd Message
      cmd = Cmd.batch <|
        List.map (perform (Error << toString) NewItem << getApiItemByItemId) itemIds
    in (s, cmd)
  NewItem item -> noop {s | items = insert item.id item s.items}
  Error msg -> ({s | error = Just msg}, Cmd.none)
  AddItemButton -> (\ cmd -> (s, cmd)) <|
    let
      new = s.addItemInput
    in perform (Error << toString)
        (\ id -> NewItem (Item id new))
        (postApiItem new)
  AddItemInputChange t -> noop {s | addItemInput = t}
  Done id -> (s, perform (Error << toString) Delete (deleteApiItemByItemId id))
  Delete id -> noop <| {s | items = remove id s.items}

noop : s -> (s, Cmd m)
noop s = (s, Cmd.none)

view : State -> Html Message
view state =
  div [] <|
    text (toString state) ::
    br [] [] ::
    (List.map (viewItem << snd) (toList state.items)) ++
    input [onInput AddItemInputChange] [] ::
    button [onClick AddItemButton] [text "add item"] ::
    []

mkUl : List (Html m) -> Html m
mkUl list = ul [] (List.map (\ item -> li [] [item]) list)

viewItem : Item -> Html Message
viewItem item = div []
  [text (item.text), text " - ", button [onClick (Done item.id)] [text "done"]]
