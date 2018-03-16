module Problem3 exposing (..)
import Functions
import Basics
import Debug

do: Int -> List Int
do  number =
    get_prime_factors number

get_prime_factors: Int -> List Int
get_prime_factors number =
    get_next_prime_factor (get_up_limit number) [] (toFloat number) 2

get_up_limit: Int -> Int
get_up_limit number = 
    number
    |> Basics.toFloat
    |> Basics.sqrt 
    |> Basics.ceiling

get_next_prime_factor:  Int -> List Int -> Float -> Int -> List Int
get_next_prime_factor up_limit current_factors rest_number eval_factor = 

    if (eval_factor <= up_limit) && (not (rest_number == 1)) then
      let 
        _ = Debug.log "log" (eval_factor, current_factors, rest_number)
        (updated_factors, updated_eval_factor, updated_rest_number) = 
            get_if_prime_and_append_to_factor_list current_factors rest_number eval_factor
      in
        get_next_prime_factor up_limit updated_factors updated_rest_number updated_eval_factor
    else
        current_factors

get_if_prime_and_append_to_factor_list: List Int -> Float -> Int -> (List Int, Int, Float)
get_if_prime_and_append_to_factor_list current_factors rest_number eval_factor  = 
    case Functions.is_multiple eval_factor (round rest_number) of
        True -> 
            (List.append current_factors [eval_factor],  eval_factor, rest_number/(toFloat eval_factor))
        False -> (current_factors, (eval_factor + 1), rest_number) 