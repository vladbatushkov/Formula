module Tests exposing (..)

import Test exposing (..)
import Expect exposing (Expectation)
import Formula exposing (..)


--import Fuzz exposing (Fuzzer, int, list, string)


suite : Test
suite =
    describe "fromula tests"
        [ describe "formula tree"
            [ test "value of Empty Tree is 0"
                (\_ ->
                    Expect.equal 0 <| getValue <| Formula.head <| Last 0
                )
            , test "operation of Empty Tree is Nothing"
                (\_ ->
                    Expect.equal Nothing <| getOperation <| Formula.head <| Last 0
                )
            , test "when add (1, Plus) to Empty then head operation equals Just Plus"
                (\_ ->
                    Expect.equal (Just Plus)
                        ((Formula.add (Element 1 Plus) <| Last 0)
                            |> Formula.head
                            |> getOperation
                        )
                )
            , test "when add (1, Plus) to Empty then head value equals 1"
                (\_ ->
                    Expect.equal 1
                        ((Formula.add (Element 1 Plus) <| Last 0)
                            |> Formula.head
                            |> getValue
                        )
                )
            , test "when add (1, Plus) to Tree(2) then calucalte equals 3"
                (\_ ->
                    Expect.equal 3
                        (calculate <|
                            (Formula.add (Element 1 Plus) <|
                                Last 2
                            )
                        )
                )
            , test "when add (1, Plus) to Tree(2, (3, Multiply)) then calucalte equals 7"
                (\_ ->
                    Expect.equal 7
                        (calculate <|
                            (Formula.add (Element 1 Plus) <|
                                Formula.add (Element 2 Multiply) <|
                                    Last 3
                            )
                        )
                )
            ]
        ]
