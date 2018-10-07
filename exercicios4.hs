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


compress = undefined
compact xs = undefined
encode xs = undefined
split xs i = undefined
slice xs imin imax = undefined
insertAt el pos xs = undefined
sort xs = undefined
mySum xs = undefined
maxList xs = undefined
buildPalindrome xs = undefined
mean xs = undefined
myAppend xs ys = undefined