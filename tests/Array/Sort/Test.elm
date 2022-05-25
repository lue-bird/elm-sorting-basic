module Array.Sort.Test exposing (suite)

import Array
import Array.Sort
import Expect
import Fuzz
import Order
import Test exposing (Test)


suite : Test
suite =
    Test.describe
        "`Array` sort basic"
        [ Test.fuzz
            (Fuzz.array Fuzz.int)
            "selection"
            (\array ->
                array
                    |> Array.Sort.selection Order.int
                    |> Expect.equal
                        (array
                            |> Array.toList
                            |> List.sortWith Order.int
                            |> Array.fromList
                        )
            )
        ]
