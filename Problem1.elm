module Problem1 exposing (..)

import Functions exposing (..)

do: Int -> Int
do max_range = 
    let 
        list = List.range 1 (max_range - 1)
        filtered_list =  List.filter is_multiple_by_5_or_3 list 
    in
        List.foldr (+) 0 filtered_list
