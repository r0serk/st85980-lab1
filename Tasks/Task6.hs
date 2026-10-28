{-|
Module      : Tasks.Task6
Description : Matricas transponēšana.

Uzdevums Nr. 6 (variants 7-8).

Matrica tiek attēlota kā sarakstu saraksts, kur visām rindām ir vienāds
garums. Funkcija 'transpose' pārveido i-to elementu no j-tā saraksta par
j-to elementu no i-tā saraksta, tas ir, rindas kļūst par kolonnām.

Piezīme par uzdevumā doto funkciju @zipWith@: tās rekursīvajā izsaukumā
trūkst argumenta @op@ (rakstīts @zipWith xs ys@), tāpēc tā nekompilējas.
Modulī izmantota izlabotā versija 'zipWithFixed'.

Robežgadījumi, kas tiek pārbaudīti testos:

* tukša matrica, rindas ar garumu 0, matrica 1x1;
* viena rinda un viena kolonna;
* simbolu matrica (funkcija ir polimorfa);
* nevienāda garuma rindas (tā nav matrica);
* burti skaitļu vietā, tukša rinda, tukša ievade.
-}
module Tasks.Task6
  ( zipWithFixed
  , transpose
  , validateMatrix
  , parseMatrix
  , drawMatrix
  , runTask6
  ) where

import Control.Monad (when)
import qualified Data.List as L

import Utils.Input (askYesNo, parseIntList, promptUntilValid, splitOn, trim, withDefault)
import Utils.Testing

{-|
Izlabotā uzdevumā dotās funkcijas @zipWith@ versija: savieno divus
sarakstus pa elementiem, pielietojot operāciju @op@.

Parametri:

* @op@ - divu argumentu funkcija;
* pirmais un otrais saraksts.

Atgrieztā vērtība:

* rezultātu saraksts; tā garums ir vienāds ar īsākā saraksta garumu.

Sarežģītība:

* Laika sarežģītība: \(O(\min(n, m))\).
-}
zipWithFixed :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithFixed op (x : xs) (y : ys) = (x `op` y) : zipWithFixed op xs ys
zipWithFixed _  _        _        = []

{-|
Transponē matricu.

Ideja: ja pārējās rindas jau ir transponētas (katra no tām tagad ir
kolonna), tad pirmās rindas i-tais elements tiek pievienots i-tās
kolonnas sākumā - tieši to dara @zipWithFixed (:)@.

Parametri:

* @matrix@ - matrica kā rindu saraksts (rindām jābūt vienāda garuma,
  to pārbauda 'validateMatrix').

Atgrieztā vērtība:

* transponētā matrica; matricai ar izmēru r x k rezultāta izmērs ir k x r.

Sarežģītība:

* Laika sarežģītība: \(O(r \cdot k)\).
-}
transpose :: [[a]] -> [[a]]
transpose []           = []                        -- tukša matrica
transpose [row]        = map (\x -> [x]) row       -- viena rinda kļūst par kolonnu
transpose (row : rows) = zipWithFixed (:) row (transpose rows)

{-|
Pārbauda, vai sarakstu saraksts ir korekta matrica (visas rindas vienāda
garuma). Bez šīs pārbaudes 'zipWithFixed' klusi nogrieztu garākās rindas.

Parametri:

* @rows@ - pārbaudāmais sarakstu saraksts.

Atgrieztā vērtība:

* @Right rows@ - ja visas rindas ir vienāda garuma;
* @Left err@ - paziņojums ar rindu garumiem.
-}
validateMatrix :: [[a]] -> Either String [[a]]
validateMatrix [] = Right []
validateMatrix rows@(firstRow : _)
  | all (\row -> length row == length firstRow) rows = Right rows
  | otherwise = Left ("rows have different lengths " ++ show (map length rows)
                      ++ " - this is not a matrix")

{-|
Pārveido lietotāja ievadi matricā.

Formāts: rindas atdala ar semikolu, elementus rindā - ar atstarpēm vai
komatiem, piem. @1 2 3; 4 5 6@.

Parametri:

* @raw@ - lietotāja ievadītā virkne.

Atgrieztā vērtība:

* @Right matrix@ - korekta matrica;
* @Left err@ - ja ievade ir tukša, kāda rinda ir tukša, satur nekorektu
  elementu (norādot rindu un elementu) vai rindas ir dažāda garuma.
-}
parseMatrix :: String -> Either String [[Int]]
parseMatrix raw
  | null text = Left "empty input - a matrix was expected"
  | otherwise = do
      rows <- mapM parseRow (zip [1 :: Int ..] (map trim (splitOn ';' text)))
      validateMatrix rows
  where
    text = trim raw
    parseRow (index, row)
      | null row  = Left ("row " ++ show index ++ " is empty")
      | otherwise = case parseIntList row of
          Right numbers -> Right numbers
          Left err      -> Left ("row " ++ show index ++ ", " ++ err)

{-|
Sagatavo matricu izdrukai: katra rinda atsevišķā rindiņā, skaitļi
līdzināti pa labi.

Parametri:

* @matrix@ - izdrukājamā matrica.

Atgrieztā vērtība:

* teksta rindu saraksts.
-}
drawMatrix :: Show a => [[a]] -> [String]
drawMatrix []     = ["    (empty matrix)"]
drawMatrix matrix = map drawRow matrix
  where
    width     = maximum (1 : [ length (show x) | row <- matrix, x <- row ])
    cell x    = let s = show x in replicate (width - length s) ' ' ++ s
    drawRow r = "    | " ++ unwords (map cell r) ++ " |"

-- | Matrica m x n ar elementiem 1, 2, 3, ... (testiem).
numberedMatrix :: Int -> Int -> [[Int]]
numberedMatrix m n = [ [ i * n + j + 1 | j <- [0 .. n - 1] ] | i <- [0 .. m - 1] ]

-- | Uzdevuma piemēra matrica.
exampleMatrix :: [[Int]]
exampleMatrix = [[1, 2, 3], [4, 5, 6]]

-- | Funkcijas 'transpose' testi, ieskaitot robežgadījumus.
functionTests :: [TestResult]
functionTests =
  [ testCase "Matrix 2x3" "[[1,2,3],[4,5,6]]" [[1, 4], [2, 5], [3, 6]] (transpose exampleMatrix)
  , testCase "Square matrix 3x3" "[[1,2,3],[4,5,6],[7,8,9]]" [[1, 4, 7], [2, 5, 8], [3, 6, 9]]
      (transpose (numberedMatrix 3 3))
  , testCase "Single row" "[[1,2,3]]" [[1], [2], [3]] (transpose [[1, 2, 3 :: Int]])
  , testCase "Single column" "[[1],[2],[3]]" [[1, 2, 3]] (transpose [[1], [2], [3 :: Int]])
  , testCase "Matrix 1x1" "[[7]]" [[7]] (transpose [[7 :: Int]])
  , testCase "Empty matrix" "[]" [] (transpose ([] :: [[Int]]))
  , testCase "Rows of length 0" "[[],[]]" [] (transpose ([[], []] :: [[Int]]))
  , testCase "Matrix of characters" "[\"abc\",\"def\"]" ["ad", "be", "cf"]
      (transpose ["abc", "def"])
  , testCase "transpose (transpose m) == m" "all matrices from 1x1 to 6x6" True
      (all (\m -> transpose (transpose m) == m) testMatrices)
  , testCase "Same result as standard Data.List.transpose" "all matrices from 1x1 to 6x6" True
      (all (\m -> transpose m == L.transpose m) testMatrices)
  , testCase "Element [i][j] becomes element [j][i]" "all matrices from 1x1 to 6x6" True
      (and [ (transpose m !! j) !! i == (m !! i) !! j
           | m <- testMatrices, i <- [0 .. length m - 1], j <- [0 .. length (firstRow m) - 1] ])
  ]
  where
    testMatrices = [ numberedMatrix m n | m <- [1 .. 6], n <- [1 .. 6] ]
    firstRow (row : _) = row
    firstRow []        = []

-- | Lietotāja ievades pārbaudes testi (robežgadījumi).
inputTests :: [TestResult]
inputTests =
  [ test "Valid matrix" "1 2 3; 4 5 6" (Right [[1, 2, 3], [4, 5, 6]])
  , test "Commas inside rows" "1,2; 3,4" (Right [[1, 2], [3, 4]])
  , test "Rows of different length" "1 2 3; 4 5"
      (Left "rows have different lengths [3,2] - this is not a matrix")
  , test "Letter instead of a number" "1 2; 3 x"
      (Left "row 2, element 2: 'x' is not a number (letters are not allowed)")
  , test "Empty row" "1 2;;3 4" (Left "row 2 is empty")
  , test "Empty input" "" (Left "empty input - a matrix was expected")
  ]
  where
    test name input expected =
      testCaseWith (showEither show) name (showText input) expected (parseMatrix input)

{-|
Uzdevuma Nr. 6 izpilde no galvenās izvēlnes: testi un interaktīvā daļa.
-}
runTask6 :: IO ()
runTask6 = do
  putStrLn "--- Task 6: Matrix Transposition ---"
  putStrLn ""
  _ <- runTests "Function tests" functionTests
  _ <- runTests "Input validation tests (edge cases)" inputTests
  interactivePart

-- Interaktīvā daļa: lietotājs ievada savu matricu.
interactivePart :: IO ()
interactivePart = do
  putStrLn "=== Try it yourself ==="
  putStrLn "Enter a matrix: rows separated by ';', numbers by spaces, e.g. 1 2 3; 4 5 6."
  putStrLn "Press Enter on an empty line to use the example matrix."
  loop
  where
    loop = do
      input <- promptUntilValid "Matrix: " (withDefault exampleMatrix parseMatrix)
      case input of
        Nothing     -> return ()
        Just matrix -> do
          putStrLn "Matrix:"
          mapM_ putStrLn (drawMatrix matrix)
          putStrLn "Transposed matrix:"
          mapM_ putStrLn (drawMatrix (transpose matrix))
          again <- askYesNo "Try another matrix? (y/n): "
          when again loop
