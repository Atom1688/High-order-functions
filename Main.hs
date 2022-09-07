--RAFAEL BAUER SAMPAIO

--1. Escreva uma função chamada fatorialn que usando o operador range e a função foldr e devolva o fatorial de n.
fatorialn :: Int -> Int
fatorialn n = foldr (*) 1 [1..n]

--2. Usando a função map escreva uma função, chamada quadradoReal que recebe uma lista de números reais, positivos e negativos e devolva uma lista com o quadrado de cada um dos reais listados.
quadradoReal :: [Int] -> [Int]
quadradoReal lista = map (^2) lista

--3. Usando a função map escreva uma função, comprimentoPalavras que recebe uma lista de palavras e devolve uma lista com o comprimento de cada uma destas palavras.
comprimentoPalavras :: [String] -> [Int]
comprimentoPalavras lista = map length lista

--4. Usando a função filter escreva uma função, chamada maiorMultiploDe29 devolva o maior número entre 0 e 100000 que seja divisivel por 29. -maiorMultiploDe29 :: Int
maiorMultiploDe29 :: Int
maiorMultiploDe29 = maximum(filter(\x -> mod x 29 == 0)[0..100000])

--5. Usando a função filter escreva uma função, chamada maiorMultiploDe que recebe um inteiro e devolva o maior número entre 0 e 100000 que seja divisivel por este inteiro.
maiorMultiploDe :: Int -> Int
maiorMultiploDe y = maximum(filter(\x -> mod x y == 0)[0..100000])

--6. Usando Haskell e a função foldr defina uma função, chamada somaQuadrados que devolva a soma dos quadrados dos itens de uma lista de números naturais de comprimento n. De tal forma que: 𝑠𝑜𝑚𝑎𝑄𝑢𝑎𝑑𝑟𝑎𝑑𝑜𝑠 = 1^2 + 2^2 + 3^2 + 4^2... + 𝑛^2.
somaQuadrados :: Int -> Int
somaQuadrados x = (foldr (+) 0 (map (^2) [1..x]))

--7. Usando Haskell e a função foldl defina uma função, chamada comprimento, que devolva o comprimento (cardinalidade) de uma lista dada.
comprimento :: [Int] -> Int
comprimento l = foldl acc (0) l
  where
    acc :: Int -> Int -> Int
    acc _ x = x + 1
    


{-8. Esta é uma tarefa de pesquisa: você deve encontrar e executar exemplos em Haskell do uso das seguintes funções disponíveis no Prelude: flip, ord, max, min, curry, uncurry. Para cada uma destas funções você deverá encontrar, executar e testar no mínimo dois exemplos.
--flip
flip (/) 2 10
flip (-) 2 5

--ord
funcao nao encontrada no Prelude

--max
max 10 5
max 2 3

--min
min 10 5
min 2 3

--curry
curry (\ (x,y) -> x-y+1) 3 4
curry (\ (x,y) -> 2*x*y) 1 2

--uncurry
uncurry (\ x y -> x-y+1) (3,4)
uncurry (\ x y -> 2*x*y) (1,2)
-}

main = do
  print("1. Fatorial, entrada: 5, saida: ", fatorialn 5)
  print("2. Quadrado real, entrada: [2,-3,5,-1], saida: ", quadradoReal[2,-3,5,-1])
  print("3. Comprimento palavras, entrada: ['haskell', 'puc', 'programacao'], saida: ", comprimentoPalavras["haskell", "puc", "programacao"])
  print("4. Maior multiplo de 29, entrada: , entrada: , saida:", maiorMultiploDe29)
  print("5. Maior multiplo de x, entrada: 8, saida: ", maiorMultiploDe 8)
  print("6. Soma quadrados, entrada: 4, saida: ", somaQuadrados 4)
  print("7. Comprimento, entrada: [1,2,3,4,5,6], saida: ", comprimento[1,2,3,4,5])
  print(" ")
  print("8. Flip1, entrada: flip / 2 10, saida: ", flip (/) 2 10)
  print("8. Flip2, entrada: flip (-) 2 5, saida: ", flip (-) 2 5)
  print("8. Max1, entrada: max 10 5, saida: ", max 10 5)
  print("8. Max2, entrada: max 2 3, saida: ", max 2 3)
  print("8. Min1, entrada: min 10 5, saida: ", min 10 5)
  print("8. Min2, entrada: min 2 3, saida: ", min 2 3)
  print("8. Curry1, entrada: curry ((x,y) -> x-y+1) 3 4), saida: ", curry (\ (x,y) -> x-y+1) 3 4)
  print("8. Curry2, entrada: curry ((x,y) -> 2*x*y) 1 2, saida: ", curry (\ (x,y) -> 2*x*y) 1 2)
  print("8. Uncurry1, entrada: uncurry (x y -> x-y+1) (3,4), saida: ", uncurry (\ x y -> x-y+1) (3,4))
  print("8. Uncurry2, entrada: uncurry (x y -> 2*x*y) (1,2) 1 2, saida: ",uncurry (\ x y -> 2*x*y) (1,2))

