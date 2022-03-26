module Main exposing (..)
import Html exposing (Html)

-- Pythagoras:
sqr: Int -> Int
sqr x =
    x^2

isTriple: (Int, Int, Int) -> Bool
isTriple (x,y,z) =
    if (x <= 0 || y <= 0 || z <= 0) then
    False
    else 
        if (sqr x + sqr y == sqr z) then
        True
        else
        False

positiveIntegers: Int -> Int -> Bool
positiveIntegers x y =   
     if ( x > 0 && y > 0 && x > y) then
     True
     else False 

leg1: Int -> Int -> Int
leg1 x y =
    if (positiveIntegers x y) then
    sqr x - sqr y
    else 0
    
leg2: Int -> Int -> Int
leg2 x y = 
    if (positiveIntegers x y) then
    2 * x * y
    else 0

hyp: Int -> Int -> Int
hyp x y = 
    if (positiveIntegers x y) then
    sqr x + sqr y
    else 0

pythTriple: (Int, Int) -> (Int, Int, Int)
pythTriple (x,y) =
    (leg1 x y, leg2 x y, hyp x y)
    
    
my_results: List String
my_results =
    [
        "-- Hello-Elm output --",
        "\n\n Leg/Hyp test cases",
        pr <| leg1 4 3,
        pr <| leg2 4 3,
        pr <| hyp 4 3,
        pr <| isTriple (3, 4, 5),
        pr <| pythTriple (4, 3) ,
        pr <| isTriple (pythTriple (4, 3)),
        
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
    
