module Tests exposing (tests)

import Expect
import Main exposing (FromUi(..), init, update)
import Test exposing (..)


tests : Test
tests =
    describe "client test suite"
        [ test "clean add item field on AddItemButton" <|
            \_ ->
                let
                    model =
                        Tuple.first init

                    ( updatedModel, _ ) =
                        { model | addItemInput = "foo" }
                            |> update (Main.FromUi AddItemButton)
                in
                updatedModel.addItemInput
                    |> Expect.equal ""
        ]
