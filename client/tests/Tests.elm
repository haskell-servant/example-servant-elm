module Tests exposing (addItemButton, tests)

import Expect
import Main exposing (..)
import Test exposing (..)


tests : Test
tests =
    describe "client test suite" [ addItemButton ]


addItemButton : Test
addItemButton =
    test "clean add item field on AddItemButton" <|
        \_ ->
            let
                initState =
                    Tuple.first init

                s =
                    { initState | addItemInput = "foo" }

                ( new, _ ) =
                    update (FromUi AddItemButton) s

                result =
                    new.addItemInput
            in
            Expect.equal "" result
