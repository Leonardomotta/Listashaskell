--Escreva a declaracao para o tipo Triple, contendo tres elementos, todos de tipos diferentes.
--Escreva funcoes tripleFst, tripleSnd, tripleThr para extrair respectivamente o primeiro, segundo e terceiro
-- elementos de uma triple.
data Triple a b c = Triple a b c deriving (Eq,Show)

tripleFst (Triple a b c) = a
tripleSnd  (Triple a b c) = b
tripleThr  (Triple a b c) = c

--Escreva um tipo Quadruple que contem 4 elementos: dois de um mesmo tipo e outros dois de outro tipo
--Escreva as funcoes frstTwo e secondTwo que retornam os dois primeiros e os dois ultimos, respectivamente
data Quadruple a b = Quadruple a a b b  deriving(Eq,Show)

firstTwo  (Quadruple a b c d) = (a,b)
secondTwo (Quadruple a b c d) = (c,d)

--Escreva um tipo de dados que pode conter um, dois, tres ou quatro elementos, dependendo do construtor
--Implemente funções tuple1 até tuple4 que que retornam Just <valor> ou Nada se o valor nao existe
data Tuple a b c d = Tuple1 a | Tuple2 a b | Tuple3 a b c | Tuple4 a b c d deriving(Eq,Show)

data Maybe a = Nada | Maybe a deriving(Eq,Show)

tuple1 (Tuple1 a) = Maybe a
tuple1 (Tuple2 a b) = Maybe a
tuple1 (Tuple3 a b c) = Maybe a
tuple1 (Tuple4 a b c d) = Maybe a

tuple2 (Tuple1 a ) = Nada
tuple2 (Tuple2 a b) = Maybe b
tuple2 (Tuple3 a b c) = Maybe b
tuple2 (Tuple4 a b c d) = Maybe b

tuple3 (Tuple1 a ) = Nada
tuple3 (Tuple2 a b) = Nada
tuple3 (Tuple3 a b c) = Maybe c
tuple3 (Tuple4 a b c d) = Maybe c

tuple4 (Tuple1 a ) = Nada
tuple4 (Tuple2 a b) = Nada
tuple4 (Tuple3 a b c) = Nada
tuple4 (Tuple4 a b c d) = Maybe d

data List a = Nil | Cons a (List a) deriving (Eq,Show)

listLength Nil = 0
listLength (Cons x xs) = 1 + listLength xs

listHead Nil = error "Empty list"
listHead (Cons x xs) = x

listTail Nil = Nil
listTail (Cons x xs) = xs

listFoldr f v Nil = v
listFoldr f v (Cons x xs) = f x (listFoldr f v xs)


listFoldl f v Nil = v
listFoldl f v (Cons x xs) = listFoldl f (f v x) xs 

--Escreva as funcoes sobre a estrutura de dados binary tree
data BinaryTree a = NIL | Node a (BinaryTree a) (BinaryTree a)
 deriving (Eq,Show)

sizeBST NIL = 0
sizeBST (Node a left right) = 1 + sizeBST left + sizeBST right

--verifica se uma BT é uma BST
isBST (Node a NIL NIL) = True
isBST (Node a NIL c) = (raiz c > a)&& isBST c
isBST (Node a b NIL) = (raiz b < a)&& isBST b
isBST (Node a b c ) = isBST(b) && isBST(c) && ((raiz b) <= a < (raiz c))

raiz (Node a b c) = a
insere  el = (Node el NIL NIL)

--insere uma nova chave na BST retornando a BST modificada
insert (Node a b c) el = if el < a then (Node a (insert b el) c)
                         else (Node a b (insert c el))
insert (NIL) el = insere el

--retorna o Node da BST contendo o dado procurado ou entao NIL
search el NIL = NIL
search el (Node a b c) = if (a == el) then Node a b c 
                         else if (a > el) then search el b
                         else search el c

--retorna o elmento maximo da BST
maximun (Node a _ NIL) = a
maximum (Node a _ c) = maximum c


--retorna o elemento minimo da BST
minimum  (Node a NIL _) = a
minimum  (Node a b Nil) = minimum c

--retorna o predecessor de um elemento da BST, caso o elemento esteja na BST
predecessor el = undefined

--retorna o sucessor de um elemento da BST, caso o elemento esteja na BST
successor = undefined

--remove ume lemento da BST
remove (Node a b c) el= undefined

--retorna uma lista com os dados da BST nos diversos tipos de caminhamento
preOrder = undefined
order = undefined
postOrder = undefined