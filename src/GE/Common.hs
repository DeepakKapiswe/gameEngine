module GE.Common where



-- | delete an element if its present otherwise it will
--   add the given element at head

searchDeleteAdd :: Eq a => a -> [a]-> [a]
searchDeleteAdd a xs | elem a xs = dropWhile (/= a) xs
                     | otherwise = a:xs 