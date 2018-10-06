xor a b  =  (a || b) && (a/=b)

impl a b = (not a ) || b

eq a b = (impl a b) && (impl b a)

square x = x*x

{-
- Implemente a funcao potencia, que retorna o resultado de x elevado a y 
-}
pow x 0 = 1
pow x y 
    | y > 0 = x*(pow x(y-1))
    |otherwise = 1/(pow x(-y))


{-
- Implemente a funcao fatorial que calcula o fatorial de um numero 
-}
fatorial 0 = 1
fatorial x =  x * fatorial (x-1)

{-
- Determina se um numero eh primo ou nao. Preocupe-se apenas em resolver o problema.
- Nao precisa usar conhecimentos mais sofisticados da teoria dos numeros. Voce pode trabalhar com listas.
-}
divisores x = [y| y<- [1..x] , x `mod` y == 0] 
isPrime x = length(divisores x) <= 2

{-
- Calcula um termo da sequencia de Fibonnacci. Voce pode trabalhar com listas. 
-}
fib 0 = 0
fib 1 = 1
fib x = fib(x-1) + fib(x-2)

{-
- Calcula um MDC de dois numeros usando o algoritmo de Euclides. 
-}
mdc 0 y = y
mdc x 0 = x
mdc x y = mdc y r 
    where
        r = mod x y

{-
- Calcula um MMC de dois numeros. 
-}
divisor x y n = (mod  n x == 0)  && (mod n  y== 0)
mmc x y = head ys
    where ys =  filter (divisor x y) [(min x y)..x*y]


{-
- Determina se dois numeros inteiros positivos sao co-primos. Dois numeros sao co-primos se 
- o mdc deles for igual a 1. Ex: coprimo 35 64 = True 
-}
coprimo x y = mdc x y == 1

{-
- Calcula a conjectura de Goldbach, 
que diz que um numero par maior 
que 2 pode ser escrito como a soma de dois numeros primos. Ex: 28 = 5 + 23.
-}
goldbach x = [(z,y)|z <- ys, y <- ys,z+y==x]
        where ys = filter isPrime [1..x]