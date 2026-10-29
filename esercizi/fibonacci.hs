-- Si scriva una funzione fibonacci i che calcoli l’i-esimo elemento della sequenza di fibonacci.

-- Fibonacci con complessità esponenziale.

fibonacciEsponenziale :: Int -> Integer
fibonacciEsponenziale 1 = 1
fibonacciEsponenziale 2 = 1
fibonacciEsponenziale i = fibonacciEsponenziale (i - 1) + fibonacciEsponenziale (i - 2)






-- Fibonacci con complessità lineare.
-- Creiamo una "finestra" che comprende gli ultimi due valori della sequenza.
-- Ogni volta calcoliamo la loro somma e includiamo il risultato nella finestra, ignorando il valore "più vecchio".
fibonacciLineare :: Int -> Integer
fibonacciLineare i = aux 0 1 i
    where 
        aux m _ 0 = m
        aux m n k = aux n (m + n) (k - 1)





-- Fibonacci con complessità logaritmica. 

-- Presa una matrice A = ((1,1), e sia F la sequenza di Fibonacci, A^k = ((F_{k+1}, F_k),
--                        (1,0))                                          (F_k, F_{k-1}))
-- Se riusciamo a trovare un modo efficiente per calcolare A^k, possiamo estrarre il valore k-esimo della sequenza di fibonacci dalla matrice generata.
-- A^0 = I
-- A^k = A × A^{k-1}           se k è dispari
-- A^k = A^{k/2} × A^{k/2}     se k è pari

type Vector = (Integer, Integer)
type Matrix = (Vector, Vector)
-- Se il nome della funzione è solo simboli e la funzione è binaria (accetta solo due argomenti), possiamo indicare il nome della funzione tra parentesi.
-- In questo modo, sarà poi possibile chiamarla in modo infisso.
-- Es.: E1 ⋅ E2
-- La stessa cosa è possibile se nel nome della funzione ci sono caratteri alfanumerici e la funzione è binaria, è comunque possibile usarla in modo infisso,
-- invocandola tra due backticks. `
-- Es.: E1 `funzione` E2
(⋅) :: Vector -> Vector -> Integer
(⋅) (a, b) (c, d) = a * c + b * d

identity :: Matrix
identity = ((1,0), (0,1))

transpose :: Matrix -> Matrix
transpose ((a,b), (c,d)) = ((a,c), (b,d))

(×) :: Matrix -> Matrix -> Matrix
(×) (r1, r2) m = ((r1 ⋅ c1, r1 ⋅ c2), (r2 ⋅ c1, r2 ⋅ c2))
    where
        (c1, c2) = transpose m

(^^^) :: Matrix -> Int -> Matrix
(^^^) _ 0 = identity
(^^^) m k | even k = a × a
    where
        a = m ^^^ (div k 2)
(^^^) m k = m × (m ^^^ (k - 1))

fibonacciLogaritmico :: Int -> Integer
fibonacciLogaritmico k = snd (fst (m ^^^ k))
    where 
        m = ((1,1), (1,0))

-- Genera l'intera sequenza di fibonacci.
fibonacciSequenza :: [Integer]
fibonacciSequenza = [fibonacciLogaritmico n | n <- [1..]]

-- Genera l'intera sequenza di fibonacci con indice.
fibonacciSequenzaConIndice :: [(Int, Integer)]
fibonacciSequenzaConIndice = [(n, fibonacciLogaritmico n) | n <- [1..]]