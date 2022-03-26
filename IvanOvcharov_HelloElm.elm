module IvanOvcharov_HelloElm exposing (..)
import String exposing (fromInt)
-- steps:
--      install Elm: https://guide.elm-lang.org/install/elm.html
--      start a terminal
--      run "elm init"
--      copy this file to the new ./src directory
--      rename this file to your own name, and rename the module (see top line above) accordingly


import Html exposing (Html)

-- your functions:
type Expression
  = Plus  Expression Expression
  | Mult  Expression Expression
  | Val   Int
  | Var   String

-- isSorted function with list recursion
-- We compare 2 values from the list x1 x2
isSorted: List number -> Bool
isSorted inputList = 
    case inputList of 
        [] ->
            True
        [_] -> 
            True
        (x1 :: x2 :: xs) ->
            if (x1 <= x2) then
                isSorted xs
            else False


lookup: String -> List (String, Int) -> Int
lookup name list =
    case list of 
        [] ->
            0
        ((x,y) :: xs) ->
            if (name == x) then
                y
            else if (name /= x) then
                lookup name xs
            else 0

bubbleOnePass: List number -> List number
bubbleOnePass input = 
    case input of 
        [] ->
            []
        [_] ->
            []
        (x :: y :: xs) ->
            if(x > y) then
                y :: x :: bubbleOnePass xs
            else
                x :: y :: bubbleOnePass xs


--  COMMENTED BECAUSE NOT FINISHED
-- repeatUntil: (a -> Bool) -> (a -> a) -> List number -> List number
-- repeatUntil predicate fun list =
--     if (predicate list) then
--       list
--     else
--       repeatUntil predicate fun (fun list)

-- -- bubbleSort: List number -> List number -> List number
-- -- bubbleSort input =
-- --     if (isSorted input) then
-- --         input
-- --     else
-- --         repeatUntil isSorted bubbleOnePass 
        

toString: Expression -> String
toString expr =
   case expr of
       Plus a b ->
         "(" ++ toString a ++ " + " ++ toString b ++ ")"
       Mult a b ->
         "(" ++ toString a ++ " * " ++ toString b ++ ")"
       Val a ->
         "(" ++ fromInt a ++ ")"
       Var a ->
          "(" ++ a ++ ")"


toStringPrio: Expression -> String
toStringPrio expr =
   case expr of
       Mult a b ->
         "(" ++ toStringPrio a ++ " * " ++ toStringPrio b ++ ")"
       Plus a b ->
         "(" ++ toStringPrio a ++ " + " ++ toStringPrio b ++ ")"
       Val a ->
         fromInt a
       Var a ->
          a
        
-- eval: List (String, Int) -> Expression -> Int



cube: Int -> Int
cube x =
    x^3

reverse: List a -> List a
reverse xs =
    List.reverse xs

reverseCube: List Int -> List Int
reverseCube xs =
    reverse (List.map cube xs)


-- collecting results for printing:

-- arbitrary list:
my_list = [2, 7, 5, 1, 42, 73, 6, 19]
ex0 = Plus (Val 5) (Var "B")
ex1 = Plus (Mult ex0 (Var "Z")) (Var "A") 
ex2 = Plus (Var "C") (Val -73) 
-- ex3 = Mult ex0 (Mult (Plus (Val 6) (Var "C")))
ex3 = Mult ex0 (Mult (Plus (Val 6) (Var "C")) (Plus (Val 7) (Var "D"))) 
ex4 = Plus (Mult (Val 5) (Var "B")) (Plus (Mult (Val 6) (Var "C")) (Mult (Val 7) (Var "D")))

my_results: List String
my_results =
    [
        "-- Hello-Elm output --\n\n  cube calculations:",
        pr <| cube -3,
        pr <| cube 25,
        "  reversing:",
        pr <| my_list ,
        pr <| reverse my_list,
        pr <| reverseCube my_list,
        "----------------------------------",
        "isSorted - sorted list test",
        pr <| isSorted [1,3,5,5,9],
        "isSorted - unsorted list test",
        pr <| isSorted [5,3,6,2],
        "isSorted - extra test",
        pr <| isSorted [1,2],
        "BubbleOnePass test",
        pr <| bubbleOnePass [5,1,4,2,8],
        "Look up test - existing name",
        pr <| lookup "B" [("A", 5), ("B", 42), ("Z", -5)],
        "Look up test - non-existing name",
        pr <| lookup "K" [("A", 5), ("B", 42), ("Z", -5)],
        
        "ToString tests",
        pr <| toString ex0,
        pr <| toString ex1,
        pr <| toString ex2,
        pr <| toString ex3,
        "ToStringPrio tests",
        pr <| toStringPrio ex3,
        pr <| toStringPrio ex4,
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
    
