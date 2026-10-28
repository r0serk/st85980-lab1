{-|
Module      : Tasks.Task5
Description : Saraksta atjaunošana no garuma koda (run-length) kodējuma.

Uzdevums Nr. 5 (variants 7-8).

Garuma koda kodējumā saraksts ir pāru saraksts @(skaits, vērtība)@: pāra
pirmais elements norāda, cik reizes otrais elements atkārtojas pēc kārtas.
Modulis nodrošina funkciju, kas no šāda kodējuma atjauno sākotnējo sarakstu,
piemēram, @[(1,1),(2,2),(1,3),(1,2),(1,4)]@ pārvēršas par @[1,2,2,3,2,4]@.

Pārbaudei modulī ir arī pretējā funkcija 'encode': jebkuram sarakstam
@xs@ jāizpildās @decode (encode xs) == xs@.

Robežgadījumi, kas tiek pārbaudīti testos:

* tukšs kodējums, viens pāris, negatīvas vērtības;
* skaits 0 vai negatīvs skaits (nekorekts kodējums);
* pārāk liels skaits (aizsardzība pret milzīgu izvadi);
* burti vai decimāldaļskaitļi skaitļu vietā, nepareizs formāts, tukša ievade.
-}
module Tasks.Task5
  ( decode
  , decodeConcatMap
  , encode
  , maxRepeat
  , validateEncoding
  , parseEncoding
  , runTask5
  ) where

import Control.Monad (when)
import Data.Char (isAlpha)
import Text.Read (readMaybe)

import Utils.Input (askYesNo, promptUntilValid, trim, withDefault)
import Utils.Testing

{-|
Atjauno sākotnējo sarakstu no garuma koda kodējuma (rekursīvs risinājums).

Katrs pāris @(n, x)@ tiek izvērsts par @n@ elementa @x@ kopijām un
savienots ar pārējā kodējuma atkodēto daļu.

Parametri:

* @pairs@ - pāru saraksts @(skaits, vērtība)@.

Atgrieztā vērtība:

* atjaunotais saraksts. Pāris ar skaitu 0 vai negatīvu skaitu nedod
  nevienu elementu (tāpēc lietotāja ievadei izmanto 'validateEncoding').

Sarežģītība:

* Laika sarežģītība: \(O(m)\), kur \(m\) ir rezultāta garums.
-}
decode :: [(Int, a)] -> [a]
decode []              = []
decode ((n, x) : rest) = replicate n x ++ decode rest

{-|
Tas pats risinājums vienā rindā ar standarta funkcijām
(izmantots rezultātu salīdzināšanai).

Parametri:

* @pairs@ - pāru saraksts @(skaits, vērtība)@.

Atgrieztā vērtība:

* tāda pati kā funkcijai 'decode'.
-}
decodeConcatMap :: [(Int, a)] -> [a]
decodeConcatMap = concatMap (uncurry replicate)

{-|
Garuma koda kodēšana (pretējā darbība), vajadzīga tikai pārbaudei.

Parametri:

* @xs@ - kodējamais saraksts.

Atgrieztā vērtība:

* pāru saraksts @(skaits, vērtība)@, piem. @encode [1,2,2]@ ir @[(1,1),(2,2)]@.

Sarežģītība:

* Laika sarežģītība: \(O(n)\).
-}
encode :: Eq a => [a] -> [(Int, a)]
encode []       = []
encode (x : xs) = (1 + length same, x) : encode rest
  where
    (same, rest) = span (== x) xs

{-|
Lielākais atļautais atkārtojumu skaits vienā pārī lietotāja ievadē.
Ierobežojums aizsargā pret milzīgu izvadi, piemēram, @(1000000000, 1)@.
-}
maxRepeat :: Int
maxRepeat = 1000

{-|
Pārbauda, vai kodējums ir korekts: katram skaitam jābūt no 1 līdz 'maxRepeat'.

Parametri:

* @pairs@ - pārbaudāmais pāru saraksts.

Atgrieztā vērtība:

* @Right pairs@ - ja visi skaiti ir korekti;
* @Left err@ - paziņojums par pirmo nekorekto pāri.
-}
validateEncoding :: [(Int, a)] -> Either String [(Int, a)]
validateEncoding pairs =
  case [ (index, count) | (index, (count, _)) <- zip [1 :: Int ..] pairs
                        , count < 1 || count > maxRepeat ] of
    [] -> Right pairs
    ((index, count) : _)
      | count < 1 -> Left ("pair " ++ show index ++ ": count must be positive (got "
                           ++ show count ++ ")")
      | otherwise -> Left ("pair " ++ show index ++ ": count " ++ show count
                           ++ " is too large (maximum " ++ show maxRepeat ++ ")")

{-|
Pārveido lietotāja ievadi kodējumā un to pārbauda.

Atļautais formāts: pāri iekavās, atdalīti ar komatiem, piem.
@(1,1),(2,2),(1,3)@; ārējās kvadrātiekavas nav obligātas.

Parametri:

* @raw@ - lietotāja ievadītā virkne.

Atgrieztā vērtība:

* @Right pairs@ - korekts kodējums;
* @Left err@ - ja ievade ir tukša, satur burtus, ir nepareizā formātā,
  satur pārāk lielus skaitļus vai nekorektus skaitus.
-}
parseEncoding :: String -> Either String [(Int, Int)]
parseEncoding raw
  | null text = Left "empty input - pairs (count, value) were expected"
  | otherwise =
      case readMaybe listText :: Maybe [(Integer, Integer)] of
        Nothing
          | any isAlpha text -> Left lettersError
          | otherwise        -> Left formatError
        Just pairs -> do
          checked <- mapM checkRange (zip [1 :: Int ..] pairs)
          validateEncoding checked
  where
    text = trim raw

    -- Ja ārējo kvadrātiekavu nav, tās tiek pievienotas.
    listText = case text of
      ('[' : _) -> text
      _         -> "[" ++ text ++ "]"

    -- Pārbauda, vai abi pāra skaitļi ir tipa Int diapazonā.
    checkRange (index, (count, value))
      | outOfRange count || outOfRange value =
          Left ("pair " ++ show index ++ ": a number is outside the Int range")
      | otherwise = Right (fromInteger count, fromInteger value)

    outOfRange n = n < toInteger (minBound :: Int) || n > toInteger (maxBound :: Int)

-- | Kļūdu paziņojumi nekorektai ievadei.
lettersError, formatError :: String
lettersError = "letters are not allowed - counts and values must be integers"
formatError  = "invalid format - expected pairs like (1,1),(2,2),(1,3)"

-- | Uzdevuma piemēra kodējums.
exampleEncoding :: [(Int, Int)]
exampleEncoding = [(1, 1), (2, 2), (1, 3), (1, 2), (1, 4)]

-- | Visi saraksti ar garumu 0..maxLength no dotā alfabēta (izsmeļošai pārbaudei).
allLists :: [a] -> Int -> [[a]]
allLists alphabet maxLength = concatMap ofLength [0 .. maxLength]
  where
    ofLength 0 = [[]]
    ofLength k = [ c : rest | c <- alphabet, rest <- ofLength (k - 1) ]

-- | Funkcijas 'decode' testi, ieskaitot robežgadījumus.
functionTests :: [TestResult]
functionTests =
  [ testCase "Task example" (show exampleEncoding) [1, 2, 2, 3, 2, 4] (decode exampleEncoding)
  , testCase "Characters" "[(3,'a'),(1,'b'),(2,'c')]" "aaabcc"
      (decode [(3, 'a'), (1, 'b'), (2, 'c')])
  , testCase "Empty encoding" "[]" [] (decode ([] :: [(Int, Int)]))
  , testCase "Single pair" "[(4,True)]" [True, True, True, True] (decode [(4, True)])
  , testCase "Negative values" "[(2,-1),(1,0)]" [-1, -1, 0] (decode [(2, -1), (1, 0 :: Int)])
  , testCase "decode (encode xs) == xs" "all 3280 lists of length 0..7 over {1,2,3}" True
      (all (\xs -> decode (encode xs) == xs) (allLists [1, 2, 3 :: Int] 7))
  , testCase "Same result as concatMap version" "encodings of the same 3280 lists" True
      (all (\xs -> decode (encode xs) == decodeConcatMap (encode xs))
           (allLists [1, 2, 3 :: Int] 7))
  , testCase "encode gives the task example" "[1,2,2,3,2,4]" exampleEncoding
      (encode [1, 2, 2, 3, 2, 4])
  ]

-- | Lietotāja ievades pārbaudes testi (robežgadījumi).
inputTests :: [TestResult]
inputTests =
  [ test "Valid pairs" "(1,1),(2,2),(1,3)" (Right [(1, 1), (2, 2), (1, 3)])
  , test "Pairs in brackets" "[(2,5)]" (Right [(2, 5)])
  , test "Count is zero" "(2,1),(0,5)" (Left "pair 2: count must be positive (got 0)")
  , test "Negative count" "(-3,1)" (Left "pair 1: count must be positive (got -3)")
  , test "Count too large" "(5000,1)" (Left "pair 1: count 5000 is too large (maximum 1000)")
  , test "Letter instead of a number" "(2,a)" (Left lettersError)
  , test "Decimal count" "(1.5,2)" (Left formatError)
  , test "Missing parenthesis" "(1,1),(2,2" (Left formatError)
  , test "Empty input" "" (Left "empty input - pairs (count, value) were expected")
  ]
  where
    test name input expected =
      testCaseWith (showEither show) name (showText input) expected (parseEncoding input)

{-|
Uzdevuma Nr. 5 izpilde no galvenās izvēlnes: testi un interaktīvā daļa.
-}
runTask5 :: IO ()
runTask5 = do
  putStrLn "--- Task 5: Run-Length Decoding ---"
  putStrLn ""
  _ <- runTests "Function tests" functionTests
  _ <- runTests "Input validation tests (edge cases)" inputTests
  interactivePart

-- Interaktīvā daļa: lietotājs ievada savu kodējumu.
interactivePart :: IO ()
interactivePart = do
  putStrLn "=== Try it yourself ==="
  putStrLn "Enter pairs (count,value), e.g. (1,1),(2,2),(1,3)."
  putStrLn "Press Enter on an empty line to use the task example."
  loop
  where
    loop = do
      input <- promptUntilValid "Encoding: " (withDefault exampleEncoding parseEncoding)
      case input of
        Nothing    -> return ()
        Just pairs -> do
          let decoded = decode pairs
          putStrLn ("Encoding:       " ++ show pairs)
          putStrLn ("Decoded list:   " ++ show decoded)
          putStrLn ("Encoded again:  " ++ show (encode decoded))
          again <- askYesNo "Try another encoding? (y/n): "
          when again loop
