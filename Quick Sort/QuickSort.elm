module Insert exposing (..)
import Html exposing (Html)
import Char exposing (..)
import String exposing (..)
import List exposing (..)
import Tuple exposing (..)

quicksort : List comparable -> List comparable
quicksort list = 
    case list of
        [] -> []
        (x::xs) ->
            let 
                smallerSorted = quicksort (List.filter ((>) x) xs)
                biggerSorted = quicksort (List.filter ((<=) x) xs)
            in  
                smallerSorted ++ [x] ++ biggerSorted

      
my_results: List String
my_results =
    [
        "Testing",
        pr <| insert -10 [1,3,5,7,9]
      
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