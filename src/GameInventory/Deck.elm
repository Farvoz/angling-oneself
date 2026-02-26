module GameInventory.Deck exposing (..)

import Array
import Random



-- Функция перемешивания списка (Fisher-Yates shuffle)


shuffleList : Random.Seed -> List a -> ( List a, Random.Seed )
shuffleList seed list =
    let
        length =
            List.length list

        array =
            Array.fromList list
    in
    shuffleArray seed array 0 length


shuffleArray : Random.Seed -> Array.Array a -> Int -> Int -> ( List a, Random.Seed )
shuffleArray seed array currentIndex length =
    if currentIndex >= length then
        ( Array.toList array, seed )

    else
        let
            ( randomIndex, newSeed ) =
                Random.step (Random.int currentIndex (length - 1)) seed

            swappedArray =
                swap array currentIndex randomIndex
        in
        shuffleArray newSeed swappedArray (currentIndex + 1) length


swap : Array.Array a -> Int -> Int -> Array.Array a
swap array i j =
    case ( Array.get i array, Array.get j array ) of
        ( Just a, Just b ) ->
            array
                |> Array.set i b
                |> Array.set j a

        _ ->
            array



-- Достать верхнюю карту из колоды


drawCard : List card -> ( List card, Maybe card )
drawCard deck =
    case deck of
        [] ->
            ( [], Nothing )

        first :: rest ->
            ( rest, Just first )



-- Достать до n карт из колоды приёмов


drawTechniqueCards : Int -> List card -> ( List card, List card )
drawTechniqueCards n deck =
    let
        toTake =
            min n (List.length deck)

        drawn =
            List.take toTake deck

        rest =
            List.drop toTake deck
    in
    ( rest, drawn )


moveTopToBottom : Int -> List a -> List a
moveTopToBottom n deck =
    let
        top =
            List.take n deck

        rest =
            List.drop n deck
    in
    rest ++ top


removeAt : Int -> List a -> List a
removeAt index list =
    List.take index list ++ List.drop (index + 1) list



-- Перемешать список и добавить в начало колоды


shuffleAndPrepend : Random.Seed -> List card -> List card -> ( List card, Random.Seed )
shuffleAndPrepend seed cardsToShuffle deck =
    let
        ( shuffled, newSeed ) =
            shuffleList seed cardsToShuffle
    in
    ( shuffled ++ deck, newSeed )
