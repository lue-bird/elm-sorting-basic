module List.Sort exposing (bubble, each2OrderMerge, insertion, merge, quick)

import Order exposing (Ordering)


{-| Runtime `n^2`
-}
bubble : Ordering element -> List element -> List element
bubble elementOrder =
    \list ->
        case list |> bubbleStep elementOrder of
            [] ->
                []

            head :: tail ->
                head :: (tail |> bubble elementOrder)


{-| Runtime `n`
-}
bubbleStep : Ordering element -> List element -> List element
bubbleStep elementOrder =
    \list ->
        case list of
            [] ->
                []

            [ onlyElement ] ->
                [ onlyElement ]

            leftHead :: rightHead :: listFrom2 ->
                let
                    listFrom2Sorted =
                        listFrom2 |> bubbleStep elementOrder
                in
                case elementOrder leftHead rightHead of
                    LT ->
                        leftHead :: rightHead :: listFrom2Sorted

                    EQ ->
                        leftHead :: rightHead :: listFrom2Sorted

                    GT ->
                        rightHead :: leftHead :: listFrom2Sorted


{-| Runtime `n^2`
-}
insertion : Ordering element -> List element -> List element
insertion elementOrder =
    List.foldl
        (\element insertionList ->
            (element :: insertionList) |> bubbleStep elementOrder
        )
        []


{-| Runtime `n * log(n)`
-}
quick : Ordering element -> List element -> List element
quick elementOrder =
    \list ->
        case list of
            [] ->
                []

            head :: tail ->
                let
                    ( before, after ) =
                        tail
                            |> List.partition
                                (\tailElement ->
                                    elementOrder tailElement head == LT
                                )
                in
                (before |> quick elementOrder)
                    ++ (head :: (after |> quick elementOrder))


{-| Runtime `n * log(n)`.

TODO: figure out why this leads to a

> RangeError: Maximum call stack size exceeded

-}
merge : Ordering element -> List element -> List element
merge elementOrder =
    \list ->
        list
            |> List.indexedMap Tuple.pair
            |> mergeHelp
                { elementOrder = elementOrder
                , length = list |> List.length
                }


mergeHelp :
    { elementOrder : Ordering element
    , length : Int
    }
    -> List ( Int, element )
    -> List element
mergeHelp { elementOrder, length } =
    \list ->
        case list of
            [] ->
                []

            [ ( _, onlyElement ) ] ->
                [ onlyElement ]

            element0 :: element1 :: listFrom2 ->
                let
                    rightLength =
                        length // 2

                    leftLength =
                        length - rightLength

                    ( left, right ) =
                        (element0 :: element1 :: listFrom2)
                            |> List.partition
                                (\( index, _ ) -> index <= leftLength - 1)

                    leftSorted =
                        left |> mergeHelp { elementOrder = elementOrder, length = leftLength }

                    rightSorted =
                        right |> mergeHelp { elementOrder = elementOrder, length = rightLength }
                in
                ( leftSorted, rightSorted ) |> each2OrderMerge elementOrder


{-| Runtime `n`
-}
each2OrderMerge :
    Ordering element
    -> ( List element, List element )
    -> List element
each2OrderMerge elementOrder =
    \listsLeftRight ->
        case listsLeftRight of
            ( [], [] ) ->
                []

            ( [], rightHead :: rightTail ) ->
                rightHead :: rightTail

            ( leftHead :: leftTail, [] ) ->
                leftHead :: leftTail

            ( leftHead :: leftTail, rightHead :: rightTail ) ->
                let
                    tailsEach2OrderMerged =
                        ( leftTail, rightTail ) |> each2OrderMerge elementOrder
                in
                case elementOrder leftHead rightHead of
                    LT ->
                        leftHead :: rightHead :: tailsEach2OrderMerged

                    EQ ->
                        leftHead :: rightHead :: tailsEach2OrderMerged

                    GT ->
                        rightHead :: leftHead :: tailsEach2OrderMerged
