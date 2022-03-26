module MathFun exposing (..)
import Html exposing (Html)
import Char exposing (..)
import String exposing (..)
import List exposing (..)
import Tuple exposing (..)


above100: Int -> Bool
above100 x =
    x > 100

double: Int -> Int
double x =
    x * 2

repeatUntil: (a -> Bool) -> (a -> a) -> a -> a
repeatUntil predicate fun initVal =
  let 
    x1 = fun initVal
  in
    if predicate x1 /= True then
        repeatUntil predicate fun x1
    else
      fun initVal

aboveValue: Int -> Int -> Bool
aboveValue x y =
    x > y

repeatUntilValue: (a -> a -> Bool) -> (a -> a) -> a -> a -> a
repeatUntilValue predicate fun n initVal =
  let 
    x1 = fun initVal
  in
    if (predicate n x1) /= True then
        repeatUntilValue predicate fun n x1
    else
      fun initVal

collatzNr: Int -> Int
collatzNr x = 
 if (modBy 2 x == 0) then
      x // 2
  else 
      (3 * x) + 1

result = []
collatzList : List Int -> List Int
collatzList x = 
  if (modBy 2 x == 0) then
    x // 2 :: result
  else
    ((3 * x) + 1) :: result 

my_results: List String
my_results =
    [
      "Testing 1st example",
      pr <| repeatUntil above100 double 7,
      "Testing 2nd example",
      pr <| repeatUntil above100 ((+) 1) 42,
      "Testing repeating until given value",
      pr <| repeatUntilValue aboveValue double 200 7,
      "Testing myCollatz function with 1 number",
      pr <| repeatUntil ((==) 1) collatzNr 19,
      "Testing collatz list function",
      pr <| repeatUntil ((==) 1) collatzList [19]
      
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