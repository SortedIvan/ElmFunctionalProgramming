module MergeSortPractice exposing (..)
import Html exposing (Html)
import Tuple exposing (..)

-- result = ([],[])
halve : List a -> (List a, List a)
halve list =
    case list of 
        [] ->
            ([],[])
        [_] ->
            ([],[])
        (x :: y :: xs) ->
            let
              result = halve xs
            in
              (x :: first result, y :: second result) 


merge : List comparable -> List comparable -> List comparable
merge list1 list2 =
    case (list1, list2) of 
        ([],[]) ->
            []
        (_, []) ->
            list1
        ([], _) ->
            list2
        (x :: xs, y :: ys) ->
            -- x :: y :: merge xs ys
            if (x < y) then
                y :: merge list2 ys
            else 
                x :: merge xs list2



msort : List comparable -> List comparable
msort list =
    if (list == []) then
        []
    else
        let
            result = halve xs

my_results: List String
my_results =
    [
        pr <| halve [20, 17, 23, 33, 19, 92, 10],
        pr <| merge [20, 18] [10, 9]
    ] 
    
-- Boiler-plate below:

-- update this values for long output lines, or small browser windows
page_width = 80

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
