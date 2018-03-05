module Types where

data Weekday  = Mon | Tue | Wed | Thu | Fri | Sat | Sun 

isWeekend :: Weekday -> Bool
isWeekend Sun = True
isWeekend Sat = True
isWeekend x = False




