module Drunk
( atIndices
  , everyOther
  , disjoint
  , stretch
  , drunk
  ) where

    import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
      , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
      , not , (&&) , (||)
      , (.) , ($)
      , flip , curry , uncurry
      , otherwise , error , undefined
    )
    import ExList
    import Data.List as L
    -- example:
    -- atIndices [1,4,5] "Tchauzinho"
    -- = "cuz"
    atIndices :: [Int] -> [a] -> [a]
    atIndices [x] xs = (xs L.!! x) : []
        atIndices [] xs = []
        atIndices (n:ns) xs = atIndices [n] xs L.++ atIndices ns xs

        -- example:
        -- everyOther 2 "Hello There"
        -- = "HloTee"
        everyOther :: Int -> [a] -> [a]
        everyOther _ [] = []
        everyOther n (x:xs) = (((x:xs) !! n) : [] ) L.++ everyOther n xs

                                                    -- examples:
                                                    -- disjoint [1,5,9] [2,4 ..]
                                                    -- = True
                                                    -- ASSUMPTIONS FOR disjoint xs ys:
                                                    --   xs and ys are sorted
                                                    disjoint :: Ord a => [a] -> [a] -> Bool
                                                    disjoint [x] [y] = x /= y
                                                    disjoint [x] (y:ys) = x /= y && disjoint [x] ys
                                                    disjoint (x:xs) ys = disjoint [x] ys && disjoint
 xs ys

                                                    -- example:
                                                    -- stretch 3 "Gustavo"
                                                    -- = "GGGuuussstttaaavvvooo"
                                                    stretch :: Int -> [a] -> [a]
    stretch n [x] = L.take n $ L.repeat x
stretch n (x:xs) = (stretch n [x]) L.++ (stretch n xs)

    -- example:
    -- drunk 3 "Gustavo"
    -- = "GusGtuasvtoavo"
    -- drunk 5 "Gustavo"
    -- = "GustaGvuostavo"
    -- To understand these string, either get drunk or look at the markings:
    --       , , , , ,,,
    --   "GusGtuasvtoavo"
    --    ''' ' ' ' '
    --         , , ,,,,,
    --   "GustaGvuostavo"
    --    ''''' ' '
    --
    -- drunk 2 "abcd"
    -- = "AbAdbc"
    drunk :: Int -> [a] -> [a]
    drunk n xs = firsts   L.++ lasts
    where firsts       = L.take n xs
    lasts        = L.drop n xs
    drunkpart    = firsts ++ xs
    where firsts =
