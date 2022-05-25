module List.Sort.Test exposing (suite)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html.Attributes exposing (name)
import List.Sort
import Order
import Test exposing (Test, test)


suite : Test
suite =
    let
        orderByNameString =
            Order.by ( .name, Order.string { case_ = Order.lowerUpper } )
    in
    Test.describe
        "`List` sort basic"
        [ Test.fuzz
            (Fuzz.list (Fuzz.map (\name -> { name = name }) Fuzz.string))
            "quick"
            (\list ->
                list
                    |> List.Sort.quick orderByNameString
                    |> Expect.equalLists
                        (list |> List.sortWith orderByNameString)
            )
        , Test.describe
            "merge"
            [ Test.fuzz
                (Fuzz.list (Fuzz.map (\name -> { name = name }) Fuzz.string))
                "full"
                (\list ->
                    [ { name = "A" }, { name = "B" }, { name = "C" }, { name = "D" } ]
                        |> List.Sort.merge orderByNameString
                        |> Expect.equalLists
                            ([ { name = "A" }, { name = "B" }, { name = "C" }, { name = "D" } ]
                                |> List.sortWith orderByNameString
                            )
                )
            , test
                "each2OrderMerge"
                (\() ->
                    ( [ 1, 8, 61 ], [ 2, 7, 13 ] )
                        |> List.Sort.each2OrderMerge compare
                        |> Expect.equalLists [ 1, 2, 7, 8, 13, 61 ]
                )
            ]
        , Test.fuzz
            (Fuzz.list (Fuzz.map (\name -> { name = name }) Fuzz.string))
            "bubble"
            (\list ->
                [ { name = "A" }, { name = "B" }, { name = "C" }, { name = "D" } ]
                    |> List.Sort.bubble orderByNameString
                    |> Expect.equalLists
                        ([ { name = "A" }, { name = "B" }, { name = "C" }, { name = "D" } ]
                            |> List.sortWith orderByNameString
                        )
            )
        , Test.fuzz
            (Fuzz.list (Fuzz.map (\name -> { name = name }) Fuzz.string))
            "insertion"
            (\list ->
                [ { name = "A" }, { name = "B" }, { name = "C" }, { name = "D" } ]
                    |> List.Sort.insertion orderByNameString
                    |> Expect.equalLists
                        ([ { name = "A" }, { name = "B" }, { name = "C" }, { name = "D" } ]
                            |> List.sortWith orderByNameString
                        )
            )
        ]
