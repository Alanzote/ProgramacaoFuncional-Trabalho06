-- Aluno: Alan Renato Bunese
-- Disciplina: Programação Funcional
-- Professor: Frank Alcantara
module Main (main) where

{- 1.
 - Usando List Comprehension escreva uma função, chamada divisoresden, que devolva
 - uma lista dos divisores de um número dado.
 -}
divisoresden :: Int -> [Int]
divisoresden n = [x | x <- [1 .. n], n `mod` x == 0]

{- 2.
 - Usando List Comprehension escreva uma função, chamada contaCaractere, que conte a
 - ocorrência de um caractere específico, em uma string dada.
 -}
contaCaractere :: Char -> String -> Int
contaCaractere c s = length [x | x <- s, x == c]

{- 3.
 - Usando List Comprehension escreva uma função, chamada dobroNaoNegativo, que devolve
- o dobro dos valores dos elementos não negativos da lista de inteiros dada.
 -}
dobroNaoNegativo :: [Int] -> [Int]
dobroNaoNegativo l = [x * 2 | x <- l, x >= 0]

{- 4.
 - Usando List Comprehension escreva uma função, chamada pitagoras, que devolva uma
 - lista de triplas, não repetidas, contendo os lados dos triângulos retângulos possíveis
 - de serem construídos por inteiros entre 1 e um número inteiro dado.
 -}
pitagoras :: Int -> [(Int, Int, Int)]
pitagoras n = [(x, y, z) | x <- [1 .. n], y <- [1 .. n], z <- [1 .. n], x^2 + y^2 == z^2]

{- 5.
 - Números perfeitos são aqueles cuja soma dos seus divisores é igual ao próprio número.
 - Usando List Comprehension escreva uma função, chamada numerosPerfeitos, que devolva
 - uma lista contendo todos os números perfeitos menores que um número dado. Lembre-se
 - que você já tem uma função que devolve uma lista de divisores de um número dado.
 -}
numerosPerfeitos :: Int -> [Int]
numerosPerfeitos n = [x | x <- [1 .. n], sum (divisoresden x) - x == x]

{- 6.
 - Usando List Comprehension escreva uma função, chamada produtoEscalar, que devolva o
 - produto escalar entre duas listas de inteiros. Lembre-se, existem as funções fst, snd e
 - zip no prelude que podem ser úteis.
 -}
produtoEscalar :: [Int] -> [Int] -> Int
produtoEscalar l1 l2 = sum [fst x * snd x | x <- zip l1 l2]

{- 7.
 - Usando List Comprehension escreva uma função, chamada primeirosPrimos, que devolva
 - uma lista contendo os n primeiros números primos a partir do número 2.
 -}
primeirosPrimos :: Int -> [Int]
primeirosPrimos n = [x | x <- [2 .. n], divisoresden x == [1, x]]

{- 8.
 - Usando List Comprehension escreva uma função, chamada paresOrdenados, que devolva
 - uma lista de par ordenados contendo uma potência de 2 e uma potência de 3 até um
 - determinado número dado. Observe que estes números podem ser bem grandes.
 -}
paresOrdenados :: Int -> [(Int, Int)]
paresOrdenados n = [(2^x, 3^y) | x <- [0 .. n], y <- [0 .. n]]

-- Main...
main :: IO()
main = do
    -- Testes...
    print ("divisoresden: entrada: 10; resultado: " ++ show (divisoresden 10))
    print ("divisoresden: entrada: 50; resultado: " ++ show (divisoresden 50))
    
    print ("contaCaractere: entrada: 'a' \"banana\"; resultado: " ++ show (contaCaractere 'a' "banana"))
    print ("contaCaractere: entrada: 'b' \"banana\"; resultado: " ++ show (contaCaractere 'b' "banana"))

    print ("dobroNaoNegativo: entrada: [1, -2, 3, -4, 5]; resultado: " ++ show (dobroNaoNegativo [1, -2, 3, -4, 5]))
    print ("dobroNaoNegativo: entrada: [-1, -2, -3, -4, -5]; resultado: " ++ show (dobroNaoNegativo [-1, -2, -3, -4, -5]))

    print ("pitagoras: entrada: 10; resultado: " ++ show (pitagoras 10))
    print ("pitagoras: entrada: 20; resultado: " ++ show (pitagoras 20))

    print ("numerosPerfeitos: entrada: 1000; resultado: " ++ show (numerosPerfeitos 1000))
    print ("numerosPerfeitos: entrada: 10000; resultado: " ++ show (numerosPerfeitos 10000))

    print ("produtoEscalar: entrada: [1, 2, 3] [4, 5, 6]; resultado: " ++ show (produtoEscalar [1, 2, 3] [4, 5, 6]))
    print ("produtoEscalar: entrada: [4, 5, 6] [7, 8, 9]; resultado: " ++ show (produtoEscalar [4, 5, 6] [7, 8, 9]))

    print ("primeirosPrimos: entrada: 10; resultado: " ++ show (primeirosPrimos 10))
    print ("primeirosPrimos: entrada: 20; resultado: " ++ show (primeirosPrimos 20))

    print ("paresOrdenados: entrada: 10; resultado: " ++ show (paresOrdenados 10))
    print ("paresOrdenados: entrada: 5; resultado: " ++ show (paresOrdenados 5))
