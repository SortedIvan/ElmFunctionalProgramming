module Main exposing (..)
import Html exposing (Html)
import Char exposing (fromCode, isUpper, isAlpha,  isLower, toCode)

--Caesar, part 1

encode : Int -> Char -> Char
encode offset char =
    if (isAlpha char) then
        if (isUpper char) then
        fromCode(modBy 26 (toCode(char) - toCode('A') + offset) + toCode('A'))
        else 
        fromCode(modBy 26 (toCode(char) - toCode('a') + offset) + toCode('a'))
    else '0'


decode : Int -> Char -> Char
decode offset char = 
    encode -offset char


my_results: List String
my_results =
    [
        "-- Hello-Elm output --\n\n  Caesar cipher:",

        pr <| encode 5 'x',
        pr <| decode 5 'c',
        pr <| encode 7 'T',
        pr <| decode 7 'A',
        pr <| encode -2 'a',
        pr <| decode -2 'a',
        pr <| encode 1 '{',
        pr <| encode -258 'i',
        pr <| decode -11 '#',
        pr <| decode -11 'W',

        "\n-- end --"
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