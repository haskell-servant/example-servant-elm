module Tests exposing (..)

import ElmTest exposing (..)
import Main exposing (..)


main : Program Never
main =
    runSuite tests


tests : Test
tests =
    suite "client test suite"
        <| [ addItemButton ]


addItemButton : Test
addItemButton =
    test "clean add item field on AddItemButton"
        <| let
            initState =
                fst init

            s =
                { initState | addItemInput = "foo" }

            ( new, _ ) =
                update (FromUi AddItemButton) s

            result =
                new.addItemInput
           in
            assertEqual "" result
