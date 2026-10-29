-- Definire una funzione swap che, applicata ad una coppia di numeri interi, restituisca la coppia  scambiata.
swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

-- Creare una funzione che, dati due punti (Float, Float), ne calcola la distanza.
type Point = (Float, Float)
distance :: Point -> Point -> Float
distance (x1, y1) (x2, y2) = sqrt ((x1 - x2)^2 + (y1-y2)^2)

-- Definire la funzione lunghezza che calcola la lunghezza di una lista.
--- Funzione length in Prelude
lunghezza :: [a] -> Int
lunghezza [] = 0
lunghezza (_ : xs) = 1 + lunghezza xs

-- Definire la funzione somma che calcola la somma di tutti gli elementi di una lista.
--- Funzione sum in Prelude
somma :: [Int] -> Int
somma [] = 0
somma (x : xs) = x + somma xs

-- Definire una funzione che, applicata a una lista di coppie di numeri interi, produca la lista dei prodotti degli elementi delle coppie.
prodlista :: [(Int, Int)] -> [Int]
prodlista [] = []
prodlista ((x, y) : xs) = (x * y) : prodlista xs

-- Definire una funzione chiudi che, applicata a due liste xs e ys, produca la lista delle coppie degli elementi corrispondenti di xs e ys.
--- Funzione zip in Prelude
chiudi :: [a] -> [b] -> [(a,b)]
chiudi _ [] = []
chiudi [] _ = []
chiudi (x : xs) (y : ys) = (x, y) : chiudi xs ys

-- Definire una funzione apri che, applicata a una lista di coppie, produca la coppia delle liste degli argomenti.
--- Funzione unzip in Prelude
apri :: [(a, b)] -> ([a], [b])
apri [] = ([], [])
apri ((x, y) : xs) = (x : fst (apri xs), y : snd (apri xs))

-- Usando l’operatore ++ definire la funzione inversa che inverte l’ordine degli elementi di una lista.
--- Funzione reverse in Prelude
inversa :: [a] -> [a]
inversa [] = []
inversa (x : xs) = inversa xs ++ [x]