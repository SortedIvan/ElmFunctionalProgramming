module MergeSort exposing (..)
import Html exposing (Html)
import Tuple exposing (..)

halve : List Int -> (List Int, List Int)
halve list =
    halveSplit list 1 [] [] -- we call the helper function and pass in the list we want to split
                            -- Furthermore, 1 is passed as a starting number, together with two empty lists (left and right)

halveSplit : List Int -> Int -> List Int -> List Int -> (List Int, List Int)
halveSplit complete startNr left right =
    case complete of 
        [] ->
            (left, right) -- After the list is traversed, we return the final combined tuple of left and right
        x :: xs ->
            if (modBy 2 startNr  /= 0) then
                halveSplit xs (startNr + 1) (x :: left) right -- If the number we pass is not even, then add the number to the left list
                                                              -- right remains the same
            else 
                halveSplit xs (startNr + 1) left (x :: right) -- else if the nr is odd, 
                                                              -- left remains the same, number gets added to right
    

merge : (List Int, List Int) -> List Int -- Take in a tuple of two lists, returns a list 
merge list =
  case (first list, second list) of -- Empty case exception
      ([],[]) ->
        []
      (x :: xs, []) -> -- case there are elements in the left list, we use return the first list
        first list
      ([], y :: ys) -> -- case there are elements in the right list, we use tuple.second
        second list
      (x::xs, y::ys) -> -- case there are elements in both, we iterate through them and check for all x and y's
        if x < y  then
          x :: merge (xs, (second list)) -- if x < y, we call the function recursively, passing the remaining xs and the second list
        else
          y :: merge ((first list), ys) -- other way around
       


msort : List Int -> List Int
msort list =
  case list of
    [] ->   -- empty case exception
      list

    [_] -> -- everything else exception (array)
      list

    _ -> -- case of nrs, we merge together the first halves and the second halves using both msort and halving
        merge ( (msort (first (halve list))) , (msort (second (halve list))))
        -- first and second here represent elm's tuple.first and tuple.second



my_results: List String
my_results =
    [
        "TEST SPLITTING [38, 27, 43, 3, 9, 82, 10]",
        pr <| msort [38, 27, 43, 3, 9, 82, 10],
        "Testing already sorted list [1, 4, 6, 9, 8, 9, 98]",
        pr <| msort [1, 4, 6, 9, 8, 9, 98],
        "Testing repeating numbers list [4, 2, 2, 2, 2, 2, 3]",
        pr <| msort [4, 2, 2, 2, 2, 2, 3],
        "Testing empty list",
        pr <| msort []
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
