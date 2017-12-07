module Tests exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)
import Formula exposing (..)


--import Fuzz exposing (Fuzzer, int, list, string)


getValue : Node -> Maybe Int
getValue node =
    case node of
        Nil ->
            Nothing

        Element val operation ->
            Just val


getOperation : Node -> Maybe Operation
getOperation node =
    case node of
        Nil ->
            Nothing

        Element val operation ->
            Just operation


suite : Test
suite =
    describe "fromula tests"
        [ describe "formula tree"
            [ test "when (set 1 Plus) then operation Just Plus"
                (\_ ->
                    Expect.equal (Just Plus) ((Formula.set 1 Plus Empty) |> Formula.get |> getOperation)
                )
            , test "when (set 1 Plus) then value 1"
                (\_ ->
                    Expect.equal (Just 1) ((Formula.set 1 Plus Empty) |> Formula.get |> getValue)
                )
            ]
        ]
