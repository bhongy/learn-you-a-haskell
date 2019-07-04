insertionSort :: (Ord a) => [a] -> [a]
insertionSort [] = []
insertionSort (x:xs) = insert x $ insertionSort xs
  where
    insert m [] = [m]
    insert m (n:ns)
      | m < n = m : n : ns
      | otherwise = n : insert m ns

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
-- TODO: avoid (++) use (:)
quickSort (x:xs) = quickSort smaller ++ [x] ++ quickSort larger
  -- TODO: use partition to iterate through `xs` once instead of twice
  where smaller = [y | y <- xs, y <= x]
        larger  = [y | y <- xs, y > x]
