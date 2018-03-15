module Problem2 exposing (..)
import Array
import Functions


type Result error value
  = Err error
  | Ok value

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

is_in_range: Int ->  Int -> Bool 
is_in_range value range =
     value < range

get_fibonacci_array: Int -> Array.Array Int
get_fibonacci_array range = 
    recurrent_fib_with_array (Array.fromList [1,1]) range

recurrent_fib_with_array: Array.Array Int -> Int -> Array.Array Int
recurrent_fib_with_array fib_elements max_range =
    case (sum_two_first_elements fib_elements) of
        Just new_value -> update_fib max_range new_value fib_elements
        Nothing -> Array.slice 0 -1 (Array.fromList [1])

update_fib: Int -> Int ->  Array.Array Int -> Array.Array Int
update_fib max_range new_value fib_elements =
    case (is_in_range new_value max_range) of
        True -> 
            recurrent_fib_with_array (Array.append (Array.fromList [new_value]) fib_elements) max_range
        False -> 
            Array.slice 0 -1 fib_elements

do: Int
do =
    get_fibonacci_array  (4 * 1000 * 1000)
    |> Array.filter Functions.is_even 
    |> Array.foldl (+) 0 
    
        