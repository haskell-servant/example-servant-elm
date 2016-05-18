
import           Debug exposing (..)
import           Html exposing (..)
import           Html.App exposing (program)
import           Html.Attributes exposing (..)
import           Html.Events exposing (..)
import           Task exposing (perform)

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
    message : Either String TodoItem
  }

type Message
  = Item TodoItem
  | Error String

init : (State, Cmd Message)
init =
  let
    fetch = perform (Error << toString) Item Api.getApi
  in ({message = Left "waiting..."}, fetch)

update : Message -> State -> (State, Cmd Message)
update message s =
  let
    new = case message of
      Item item -> {s | message = Right item}
      Error msg -> {s | message = Left msg}
  in (new, Cmd.none)

view : State -> Html Message
view state =
  let
    err =
      case state.message of
        Left error -> div [] [text error]
        Right item -> div [] [text (toString item.id)]
  in div [] <|
    err ::
    []
