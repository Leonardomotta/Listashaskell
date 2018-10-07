--Exemplos de expressoes lambda
square = \x -> x*x

--Implemente as funções anteriormente escritas usando expressões lambda
--consulte suas implementacoes anteriores para a documentacao dessas funcoes

pow = \x -> \y -> x ** y

fatorial = \x -> if x == 0 then 1
                 else  x * (fatorial (x-1))

isPrime = \x -> if length [z|z<-[1..x],x`mod`z == 0] > 2 then False
                else True
                
fib = \x -> if x == 0  then 0
            else if x == 1 then 1
            else (fib (x-1)) + (fib(x-2))


mdc = \x y -> if  (y == 0) then x
                  else if (x == 0) then y
                  else mdc y (x `mod`y)


mmc = \x y -> (x*y) `div` (mdc x y)

coprimo = \ x y -> if(mdc x y ) == 1 then True
                   else False

goldbach = \x -> [(z ,y)|z <- [1..x] , y <- [1..x], z+y == x]

--Implemente as funções sobre listas escritas previsamente usando expressões lambda
--consulte suas implementacoes anteriores para a documentacao dessas funcoes
isEmpty s| s == [] = True
         | otherwise = False

meuLast = \(x:s) -> if (length s) == 0  then x
                    else meuLast s
penultimo = \(x:s) -> if (length s) == 1  then x
            else penultimo s

elementAt = \i xs-> if i == 1 then head xs
                    else elementAt (i-1) (tail xs)
meuLength:: Eq a => [a] ->Int
meuLength = \(x:s) -> if (s == []) then 1
                      else 1 + (meuLength s)

meuLength' = \(xs) -> case xs of [] -> 0
                                 (x:s)-> 1 + meuLength' s

meuReverso :: Eq a => [a] -> [a]
meuReverso = \xs -> case xs of [] -> []
                               (xs)-> (([last xs]) ++ meuReverso(init xs))
 
 
isPalindrome :: Eq a=> [a] -> Bool
isPalindrome = \xs -> xs == meuReverso  xs

conjunto z [] = z
conjunto z x 
             | elem (head x) z  = conjunto z (tail x)
             | otherwise = conjunto (z ++ [head x]) (tail x)

compress :: Eq a => [a] -> [a]
compress = \xs ->  (conjunto [head xs] (tail xs))

removeAll :: Eq a => [a]-> a -> [a]
removeAll = \xs a -> if (xs == []) then []
                    else if (head xs ) == a then [] ++ (removeAll (tail xs) a)
                    else  ([head xs]) ++ (removeAll (tail xs) a)

compact :: Eq a => [a] -> [a]
compact = \xs -> if (xs == []) then []
                 else [k|k <- xs,k==(head xs)] ++ (compact (removeAll xs (head xs)))

encode :: Eq a => [a] -> [(a,Int)]
encode = \xs ->if (xs == []) then []
              else [(head (a xs) , length (a xs) )] ++ (encode(removeAll xs (head xs)))
              
a :: Eq a => [a]->[a]
a = \xs -> [k|k <- xs, k == (head xs)]

split::Eq a => [a]->Int->[[a]]
split = \xs i -> [take i xs , drop i xs]


slice xs imin imax = [x|x <-xs , (not)(x `elem`((take (imin-1)xs) ++ (drop (imax) xs) ) )    ]


insertAt = \el pos xs -> ((take (pos-1) xs) ++ [el] ++ (drop (pos-1) xs))

minList [x] = x
minList (x:xs) = if (x < (minList xs)) then x else minList xs

remove e (x:xs) | e == x = xs
                | otherwise = x:(remove e xs)
sort [] = []
sort xs = x:ys 
    where
        x = minList xs
        ys = sort (remove x xs) 


mySum = \xs -> if length xs == 0 then 0
               else (head xs) + (mySum  (tail xs)) 

m :: Int -> Int -> Int
m = \a b -> if a > b then a else b              

maxList = \xs -> if length (tail xs) == 0 then head xs
                 else (m (head xs) (maxList (tail xs)))

buildPalindrome =  \xs -> xs ++ help xs

help = \xs -> if length xs == 0 then []
              else ([last xs]) ++ (help (init xs))

mean = \xs -> (mySum xs ) `div` (meuLength xs)

myAppend :: [a] -> [a] -> [a]
myAppend = \xs ys -> (foldr(:)  ys xs)  