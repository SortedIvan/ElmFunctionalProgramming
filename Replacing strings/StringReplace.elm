module StringReplace exposing (..)
import Html exposing (Html)
import Char exposing (..)
import String exposing (..)
import List exposing (..)
import Tuple exposing (..)

removeAt : Int -> List a -> List a
removeAt index list =
     removeAtHelper index 0 list

removeAtHelper :Int -> Int -> List a -> List a
removeAtHelper index counter list =
    case list of 
        [] -> 
           []
        x :: xs -> 
            if (index /= counter) then
               x :: removeAtHelper index (counter + 1) xs
            else
                removeAtHelper index (counter + 1) xs

dropWhile : (a -> Bool) -> List a -> List a
dropWhile predicate list = 
  case list of
    [] -> []
    (x::xs) -> 
      if predicate x then 
        dropWhile predicate xs 
      else list

takeWhile : (a -> Bool) -> List a -> List a
takeWhile p list = 
    case list of
    [] -> 
        []
    (x::xs) ->
        if p x then 
             x :: takeWhile p xs 
        else 
            []

splitString : String -> (String, String)
splitString str =
     let
        listChar =  String.toList(str)
        dropVar = removeAt 0 (dropWhile ((/=) '/') listChar)
     in
      (String.fromList(takeWhile ((/=) '/') listChar) , String.fromList(dropVar))

my_results: List String
my_results =
    [
        pr <| splitString "Hello/Fontys"
      
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