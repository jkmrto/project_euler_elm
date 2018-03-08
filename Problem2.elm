module Problem2 exposing (..)
import Array

fib: Int -> Int
fib max_range =
    recurrent_fib 1 1 max_range  

recurrent_fib: Int -> Int -> Int -> Int
recurrent_fib to_sum previous_acc max_range = 
    case (previous_acc + to_sum) <  max_range of
        True -> recurrent_fib previous_acc (previous_acc + to_sum) max_range
        False -> previous_acc


sum_two_first_elements: Array.Array Int -> Maybe Int
sum_two_first_elements elements = 
    Maybe.map2 (+) (Array.get 0 elements) (Array.get 1 elements)

is_in_range: Maybe Int ->  Int -> Maybe Bool 
is_in_range value range =
     Maybe.map2 (<) value (Just range)


recurrent_fib_with_array: Array.Array Int -> Int -> Array.Array Int
recurrent_fib_with_array fib_elements max_range =
    let 
        new_value = sum_two_first_elements fib_elements
    in
        case  (is_in_range new_value max_range) of
 --           Just True -> 
 --               Array.fromList [new_value]
 --               |> recurrent_fib_with_array fib_elements
            Just False -> 
                fib_elements
            Nothing -> Array.fromList [1,1]
