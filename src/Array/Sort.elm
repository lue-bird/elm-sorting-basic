module Array.Sort exposing (selection)

import Array exposing (Array)
import Order exposing (Ordering)


{-| Runtime `n^2 * log(n)` because swapping takes `log(n)` time
-}
selection : Ordering element -> Array element -> Array element
selection elementOrder =
    selectionHelp { elementOrder = elementOrder, firstUnsortedIndex = 0 }


selectionHelp :
    { elementOrder : Ordering element, firstUnsortedIndex : Int }
    -> Array element
    -> Array element
selectionHelp { elementOrder, firstUnsortedIndex } =
    \array ->
        case array |> Array.get firstUnsortedIndex of
            -- firstUnsortedIndex >= length → array sorted
            Nothing ->
                array

            Just firstUnsortedElement ->
                let
                    fromUnsortedMinimum =
                        array
                            |> minimumFrom
                                { elementOrder = elementOrder
                                , firstUnsorted =
                                    { index = firstUnsortedIndex
                                    , element = firstUnsortedElement
                                    }
                                }
                in
                array
                    |> Array.set fromUnsortedMinimum.index firstUnsortedElement
                    |> Array.set firstUnsortedIndex fromUnsortedMinimum.element
                    |> selectionHelp
                        { elementOrder = elementOrder
                        , firstUnsortedIndex = firstUnsortedIndex + 1
                        }


minimumFrom :
    { elementOrder : Ordering element
    , firstUnsorted : { index : Int, element : element }
    }
    -> Array element
    -> { index : Int, element : element }
minimumFrom { elementOrder, firstUnsorted } =
    \array ->
        let
            final =
                array
                    |> Array.foldl
                        (\element { index, soFarMinimum } ->
                            { index = index + 1
                            , soFarMinimum =
                                { index = soFarMinimum.index
                                , element =
                                    if index < firstUnsorted.index then
                                        soFarMinimum.element

                                    else
                                        case elementOrder element soFarMinimum.element of
                                            LT ->
                                                soFarMinimum.element

                                            EQ ->
                                                soFarMinimum.element

                                            GT ->
                                                element
                                }
                            }
                        )
                        { index = 0, soFarMinimum = firstUnsorted }
        in
        final.soFarMinimum
