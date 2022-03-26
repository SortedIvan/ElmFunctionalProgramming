module Main exposing (..)
import Html exposing (Html)
import Char exposing (..)
import String exposing (..)
import List exposing (..)

--Caesar, part 2
normalize : String -> List Char
normalize str = 
   toList(String.filter isAlpha str) 

encrypt : Int -> String -> String
encrypt offset str = 
   fromList(List.map fromCode(List.map 
  (\c -> if (isUpper c) then modBy 26 (toCode(c) - toCode('A') + offset) + toCode('A') 
         else modBy 26 (toCode(c) - toCode('a') + offset) + toCode('a') ) (normalize str)))

decrypt : Int -> String -> String
decrypt offset str = 
  encrypt -offset str

my_results: List String
my_results =
    [
        "-- Hello-Elm output --\n\n  Caesar cipher:",
        pr <| encrypt -256 "AaBbCcDd avsgvasgdv 1c564a65",
        pr <| decrypt -256 "EeFfGgHh ezwkzewkhz 1g564e65",
        pr <| encrypt -38 "Hello, my name is Elm!",
        pr <| decrypt -38 "VszzcamboaswgSza",
        pr <| encrypt 3 "ABCZabcdz",
        pr <| decrypt 3 "DEFCdefgc",
        pr <| encrypt 1 "",
        pr <| decrypt 1 "",
        pr <| encrypt -4881 "!15 S81 51",
        pr <| decrypt -4881 "!15 Z81 51",
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