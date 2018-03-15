module Functions exposing (..)

is_multiple: Int -> Int -> Bool
is_multiple = \factor num -> num % factor == 0

is_multiple_by_5_or_3: Int -> Bool
is_multiple_by_5_or_3 = \num -> is_multiple 5 num || is_multiple 3 num

is_even: Int -> Bool
is_even = \num -> num % 2 == 1