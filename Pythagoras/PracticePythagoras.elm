module PracticePythagoras exposing (..)
import Html exposing (Html)
import Char exposing (..)
import String exposing (..)
import List exposing (..)
import Tuple exposing (..)

sqr : Int -> Int
sqr x = 
    x ^ 2

isTriple: Int -> Int -> Int -> Bool
isTriple a b c = 
    if (a ^ 2 + b ^ 2 == c ^ 2) then
        True
    else
        False

leg1 : Int -> Int -> Int
leg1 x y =
    (x ^ 2) - (y ^ 2)
    
leg2 : Int -> Int -> Int
leg2 x y = 
    2 * y * x

hyp : Int -> Int -> Int
hyp x y = 
    (x ^ 2) + (y ^ 2)


pythTriple : (Int, Int) -> (Int, Int, Int)
pythTriple tuple =
    ((leg1 (first tuple) (second tuple)), (leg2 (first tuple) (second tuple)), (hyp (first tuple) (second tuple)))

isTripleTuple : (Int, Int, Int) -> Bool
isTripleTuple (a, b, c) =
    isTriple a b c

pythTriplesRec : List (Int, Int) -> List (Int, Int, Int)
pythTriplesRec list =
    case list of 
        [] ->
            []
        (x :: xs) ->
            (pythTriple x) :: pythTriplesRec xs

pythTriplesMap : List (Int, Int) -> List (Int, Int, Int)
pythTriplesMap list = 
    if (list == []) then 
        []
    else 
        List.map pythTriple list

arePythTriplesFilter : List (Int, Int, Int) -> List (Int, Int, Int)
arePythTriplesFilter list = 
    if (list == []) then
        []
    else
        List.filter isTripleTuple list


arePythTriplesRec : List (Int, Int, Int) -> List (Int, Int, Int)
arePythTriplesRec list =
    case list of 
        [] ->
            []
        (x :: xs) ->
            if (isTripleTuple x) then
                x :: arePythTriplesRec xs
            else
                arePythTriplesRec xs

my_results: List String
my_results =
    [
      "isTriple fun test",
      pr <| isTriple 3 4 5,
      "isTriple fun test using leg1 leg2 & hyp",
      pr <| isTriple (leg1 5 4) (leg2 5 4) (hyp 5 4),
      "pythTriple test",
      pr <| pythTriple (5,4),
      "isTripleTuple test",
      pr <| isTripleTuple (pythTriple (5,4)),
      "pyth Triple recursive test",
      pr <| pythTriplesRec [(5,4),(2,1),(35,7)],
      "map test",
      pr <| pythTriplesMap [(5,4),(2,1),(35,7)],
      "arePythTriplesFilter",
      pr <| arePythTriplesFilter [(1,2,3), (9,40,41), (3,4,5), (100,2,500)],
      "recursive test",
      pr <| arePythTriplesRec [(1,2,3), (9,40,41), (3,4,5), (100,2,500)]
      
      
    ] 
    
page_width = 200

to_wrap: String -> String
to_wrap my_value =
    if (String.length my_value <= page_width) then
        (String.left page_width my_value)
    else
        (String.left page_width my_value) ++ ("\n") ++ to_wrap (String.dropLeft page_width my_value)

to_div: String -> Html msg
to_div my_value = 
    Html.div [] [(to_wrap my_value) |> Html.text]

pr = Debug.toString

main: Html msg
main = Html.pre 
        []
        (List.map to_div my_results)