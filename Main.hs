-- Henrique Levandoski Richa

{- 1. Escreva uma função chamada fatorialn que usando o operador range e a função foldr
devolva o fatorial de n. -}
fatorialn :: Int -> Int
fatorialn n = foldr (*) 1 [1..n]

{- 2. Usando a função map escreva uma função, chamada quadradoReal que recebe uma lista de
números reais, positivos e negativos e devolva uma lista com o quadrado de cada um dos
reais listados -}
quadradoReal :: [Float] -> [Float]
quadradoReal = map (\x -> (*) x x)

{- 3. Usando a função map escreva uma função, comprimentoPalavras que recebe uma lista de
palavras e devolve uma lista com o comprimento de cada uma destas palavras. -}
comprimentoPalavras :: [String] -> [Int]
comprimentoPalavras = map (length)

{- 4. Usando a função filter escreva uma função, chamada maiorMultiploDe29 devolva o maior
número entre 0 e 100000 que seja divisivel por 29. -}
maiorMultiploDe29 :: [Int] -> Int
maiorMultiploDe29 = maximum . filter (\x -> mod x 29 == 0)

{- 5. Usando a função filter escreva uma função, chamada maiorMultiploDe que recebe um
inteiro e devolva o maior número entre 0 e 100000 que seja divisivel por este inteiro. -}
maiorMultiploDe :: Int -> Int
maiorMultiploDe n = maximum (filter(\x -> mod x n == 0) [0..100000])

{- 6. Usando Haskell e a função foldr defina uma função, chamada somaQuadrados que devolva
a soma dos quadrados dos itens de uma lista de números naturais de comprimento n. De
tal forma que: 𝑠𝑜𝑚𝑎𝑄𝑢𝑎𝑑𝑟𝑎𝑑𝑜𝑠 = 1^2 + 2^2 + 3^2 + 4^2 + ... + n^2. -}
somaQuadrados :: Int -> Int
somaQuadrados n = foldr (+) 0 (map (^2) ([0..n]))

{- 7. Usando Haskell e a função foldl defina uma função, chamada comprimento, que devolva o
comprimento (cardinalidade) de uma lista dada. -}
comprimento :: [Int] -> Int
comprimento = foldl (\x _ -> x + 1) 0

{- 8. Esta é uma tarefa de pesquisa: você deve encontrar e executar exemplos em Haskell do uso
das seguintes funções disponíveis no Prelude: flip, ord, max, min, curry, uncurry. Para cada
uma destas funções você deverá encontrar, executar e testar no mínimo dois exemplos. -}


main = do
  putStr "Func. 1: entrada:5, resultado: "
  print(fatorialn 5)

  putStr "Func. 2: entrada:[1,2,3,4,5], resultado: "
  print(quadradoReal [1,2,3,4,5])

  putStr "Func. 3: entrada: [\"teste\", \"professor\", \"maca\"], resultado: "
  print(comprimentoPalavras ["teste", "professor", "maca"])

  putStr "Func. 4: entrada: [0..100000], resultado: "
  print(maiorMultiploDe29 [0..100000])

  putStr "Func. 5: entrada: 45, resultado: "
  print(maiorMultiploDe 45)

  putStr "Func. 6: entrada: 5, resultado: "
  print(somaQuadrados 5)

  putStr "Func. 7: entrada: [1,2,3,4,5,6,7,8], resultado: "
  print(comprimento [1,2,3,4,5,6,7,8])

  putStr "Func. 8: entrada: 5 2, resultado: "
  print(flip (>) 5 2)

  putStr "Func. 8: entrada: \"Hello\" \"World\", resultado: "
  print(flip (++) "Hello" "World")

  putStr "Func. 8: entrada: 5 10, resultado: "
  print(max 5 10)

  putStr "Func. 8: entrada 7 2, resultado: "
  print(max 7 2)

  putStr "Func. 8: entrada: 5 10, resultado: "
  print(min 5 10)

  putStr "Func. 8: entrada 7 2, resultado: "
  print(min 7 2)

  putStr "Func. 8: entrada 7 2, resultado: "
  print(curry fst 5 10)

  putStr "Func. 8: entrada 7 2, resultado: "
  print(curry snd 5 10)

  putStr "Func. 8: entrada: 2 4; resultado: "
  print(uncurry mod (5,2))

  putStr "Func. 8: entrada: 1 2; resultado: "
  print(uncurry (+) (1, 2))