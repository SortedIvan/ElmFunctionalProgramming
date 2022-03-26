module Main exposing (..)
import Html exposing (Html)
import Char exposing (..)
import String exposing (..)
import List exposing (..)
import Tuple exposing (..)

type alias Variable = Float                             -- We introduce a custom type variable called Function.
type Function                                           -- The different 'expressions' of this type use 'functions'
  = Mult  Function Function                             -- as their parameters. And the functions break down to 
  | Plus  Function Function                             -- simple float variables.
  | Minus Function Function                             -- The expressions are the regular mathematical operations.
  | Poly  Function Function                             -- Const is also type Function, it holds a simple float.
  | Div   Function Function                             -- Every function can be broken down to a Const function.
  | Const Variable                                      -- X is also a function that holds a string for the 'x'
  | X String                                            -- parameter.


print : Function -> String                              -- The function print turns a Function into a String 
print expr =
    case expr of
        Mult a b ->                                     -- If the function is Mult the func returns "a * b"
            "(" ++ print a ++ " * " ++ print b ++ ")"   -- The function keeps calling itself untill the parameters a and b 
                                                        -- are broken down to Const functions.
        Plus a b ->                                     -- The function Plus returns "a + b"
           "(" ++ print a ++ " + " ++ print b ++ ")"

        Minus a b ->                                    -- The function Minus returns "a - b"
            "(" ++ print a ++ " - " ++ print b ++ ")"

        Poly a b ->                                     -- The function Poly returns "a ^ b"
            "(" ++ print a ++ " ^ " ++ print b ++ ")"

        Div a b ->                                      -- The function Div returns "a / b"
            "(" ++ print a ++ " / " ++ print b ++ ")"

        Const a ->                                      -- The function Const returns the float that the function holds
           String.fromFloat a
          
        X a ->                                          -- The function X returns the String "x"
           a

eval : Float -> Function -> Float                       -- The function eval calculates a given function for a given x value
eval fl expr =                                          -- and returns the result
   case expr of                                         -- This func works in the same way as the print functions
        Mult a b ->                                     -- but instead of returning strings, 
            eval fl a * eval fl b                       -- the function calculates the float values.

        Plus a b ->
            eval fl a + eval fl b 

        Minus a b ->
            eval fl a - eval fl b 

        Poly a b ->
            eval fl a ^ eval fl b 

        Div a b ->
            eval fl a / eval fl b 

        Const a ->
            a
          
        X a ->
            fl

ordinateRange : Int -> Int -> Float                         -- This func receives a min and max Y values and returns 
ordinateRange minY maxY =                                   -- their range value
    if (maxY > 0) then
        Basics.toFloat (maxY - minY)
    else if (minY <= 0 && maxY <= 0) then 
        Basics.toFloat (abs((abs maxY) - (abs minY)))
    else -1

middleGround : Int -> Int -> Float                          -- This func receives a min and max Y values and returns
middleGround minY maxY =                                    -- and returns their middle value
    if (maxY > 0) then  
        (Basics.toFloat ((abs maxY) - (abs minY))) / 2
    else if (minY <= 0 && maxY <= 0) then 
        (Basics.toFloat ((abs maxY) - (abs minY))) / 2
    else -1

roundedHalfRange : Int -> Int -> Int                        -- This func receives a min and max Y values and returns
roundedHalfRange minY maxY =                                -- and returns their rounded half range value
    (Basics.round(ordinateRange minY maxY)) // 2

result = ""
graph : Function -> Int -> Int -> Int -> Int -> String                             -- This func turns a given function and given
graph expr minX maxX minY maxY =                                                   -- x and y ranges into a graph
    if (minX == maxX) then                                                         -- For every minX we calculate the function
        result                                                                     -- then we increase the minX with 1.
    else if ((eval (Basics.toFloat minX) expr) > (Basics.toFloat maxY)) then       -- If the result is more than the maxY we print 
       (String.repeat (Basics.round (ordinateRange minY maxY)) "*") ++             -- a whole line of starts *
       ("\n") ++                                                                   -- the lines' size is equal to the ordinateRange 
       graph expr (minX + 1) maxX minY maxY                                        -- then we recall the func with the new minX
    else if ((eval (Basics.toFloat minX) expr) < (Basics.toFloat minY)) then       -- If the result is smaller than the minY we print
       (String.repeat (Basics.round (ordinateRange minY maxY)) "-") ++             -- a whole line of minuses -
       ("\n") ++ 
       graph expr (minX + 1) maxX minY maxY 
    else if ((eval (Basics.toFloat minX) expr) > (middleGround minY maxY)) then                                         -- If the result is smaller than maxY bit bigger 
      (String.repeat (roundedHalfRange minY maxY) "*") ++                                                               -- than the middleGround then we print half line of stara
      (String.repeat (Basics.round (eval (Basics.toFloat minX) expr)) "*") ++                                           -- then we print as much stars as the result and the rest of the line is minuses
      (String.repeat ((roundedHalfRange minY maxY) - (Basics.round (eval (Basics.toFloat minX) expr))) "-") ++
      ("\n") ++ 
      graph expr (minX + 1) maxX minY maxY
    else if ((eval (Basics.toFloat minX) expr) < (middleGround minY maxY)) then                                         -- If the result is bigger than minY bit smaller than 
      (String.repeat ((roundedHalfRange minY maxY) - (abs (Basics.round (eval (Basics.toFloat minX) expr)))) "*") ++    -- the middleGround then we print (the halfRange - the result) in stars
      (String.repeat (abs (Basics.round (eval (Basics.toFloat minX) expr))) "-") ++                                     -- then we print the result in minuses and the rest of the line is in minuses
      (String.repeat (roundedHalfRange minY maxY) "-") ++
      ("\n") ++ 
      graph expr (minX + 1) maxX minY maxY
    else graph expr (minX + 1) maxX minY maxY  



my_results: List String
my_results =
    [
        pr <| print (Plus (Mult (Plus (Const 3) (X "x")) (Minus (X "x") (Poly (X "x") (Const 5)))) (Const 2)),
        pr <| eval 2 (Plus (Mult (Plus (Const 3) (X "x")) (Minus (X "x") (Poly (X "x") (Const 5)))) (Const 2)),
        pr <| print (Plus (Minus (Poly (Minus (Div (X "x") (Const 5)) (Const 1)) (Const 4)) (Poly (Plus (Div (X "x") (Const -2)) (Const 2)) (Const 2)))(Const 6)),
        pr <| eval -10 (Plus (Minus (Poly (Minus (Div (X "x") (Const 5)) (Const 1)) (Const 4)) (Poly (Plus (Div (X "x") (Const -2)) (Const 2)) (Const 2)))(Const 6)),
        "graph of: (x/5 - 1) ^ 4 – ((x/-2) + 2) ^ 2 + 6",
        graph (Plus (Minus (Poly (Minus (Div (X "x") (Const 5)) (Const 1)) (Const 4)) (Poly (Plus (Div (X "x") (Const -2)) (Const 2)) (Const 2)))(Const 6)) -10 20 -10 10 ,
        "graph of: x^2/5",
        graph (Div (Poly (X "x") (Const 2)) (Const 5)) -10 10 -10 10
         
    ] 
    
page_width = 1000

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



        