--factorial using pattern matching and recursion
factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial(n - 1)

length' :: (Num b) => [a] -> b
length' [] = 0
length' (_:xs) = 1 + length' xs

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

--Guards
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | weight / height ^ 2 <= 18.5 = "Under"
    | weight / height ^ 2 <= 25.0 = "Normal"
    | weight / height ^ 2 <= 30.0 = "Over"
    | otherwise   = "Whale!"

initials :: ([Char]) -> [Char] -> [Char] 
initials firstname lastname = [f] ++ "." ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname
