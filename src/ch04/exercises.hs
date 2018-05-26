import Data.Char

safeHead:: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x


safeTail::[a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs

safeLast::[a] -> Maybe a
safeLast [] = Nothing
safeLast [x] = Just x
safeLast (_:xs) = safeLast xs

safeInit::[a] -> Maybe [a]
safeInit [] = Nothing
safeInit [x] = Just []
safeInit (x:xs) = fmap (x:) $ safeInit xs


splitWith:: (a -> Bool) ->  [a] -> [[a]]
splitWith _ [] = []
splitWith f xs = pre : splitWith f rest
  where (pre, suf) = break f xs
        rest = dropWhile f suf

asInt_fold:: String -> Int
asInt_fold ('-':xs) = - asInt_fold xs
asInt_fold xs = foldl (\acc x -> 10 * acc + digitToInt x) 0 xs
