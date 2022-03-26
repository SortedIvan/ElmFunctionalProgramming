module Caesar3 exposing (..)
import Html exposing (Html)
import Char exposing (..)
import String exposing (..)
import List exposing (..)
import Regex exposing (..)
import Tuple exposing (..)

--Caesar, part 3
encriptionList = [1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]

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

compare: List Char -> List Char -> List Char -> Bool
compare wholeKey key str =                          -- the key is sent twice to the params, this way the function
       case (key, str) of                           -- always has the original key even throughout the recusrion
        ([], []) ->                                 -- if the key and the input string are both empty they match
          True                                      
        (_ :: _, [])->                              -- if the input is empty there is no match
          False
        ([], _ :: ys) ->                            -- if the key exceeded its length repeat the operation
          compare wholeKey key ys                   -- with the same key and the rest of the input string
        (x :: xs, y::ys) ->                         -- if the characters are the same 
          if (x == y) then                          -- if some part of the key matches with the input then continue
            compare wholeKey xs ys                  -- with the rest of the key and input
          else                                      -- if some part of the key and the input do not match
            compare wholeKey wholeKey ys            -- try again with the whole key and the rest of the input
    
convertStringToLists: String -> String -> Bool      -- This function converts the comparable key and string to 
convertStringToLists key str =                      -- char lists and sends them to the compare function
    compare(String.toList(key)) 
    (String.toList(key)) (String.toList(str))


-- This function maps every key in the list of patterns/keys and maps every tuple returned from the func
-- "encriptedString". The function sends every key/pattern with every possible encription of the input string
-- to the function "convertStringToLists" -> then to "compare"
-- The putput of this func is a list with lists for every elements of the pattern/key list and it's matching
-- encriptions.
candidates : List String -> String -> List (List ( Int, String ))
candidates patterns str =
 List.map (\pattern -> (List.filter 
        (\c -> (convertStringToLists pattern (second c))) (encriptedString str)) ) patterns

-- This function encripts the input string with every key from 1 to 25 
-- and puts them into a list of tuples (tuple: key, encripted string)
encriptedString : String -> List ( Int, String )
encriptedString str =
    List.map2 Tuple.pair encriptionList (List.map (\c -> (encrypt c str)) encriptionList)

my_results: List String
my_results =
    [
        "\n-- Caesar cipher part 3: --",
        pr <| candidates ["THE"] "DGGADBCOOCZYMJHZYVMTOJOCZHVS",
        pr <| candidates ["THE", "AND"] "DGGADBCOOCZYMJHZYVMTOJOCZHVSXKA",
        pr <| candidates ["Hello", "WeAreGroupTen"] "ssdcFAFEbiiladbcajbcuoabcoHBVVvhvHZhDuhJurxsWhqVHIViv",
        pr <| candidates ["a", "s", "z", "1"] "f1",
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


