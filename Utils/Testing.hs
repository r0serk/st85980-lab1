{-|
Module      : Utils.Testing
Description : Testa gadījumu izpilde un rezultātu izdruka.

Modulis ļauj katram uzdevumam aprakstīt testa gadījumus, arī
robežgadījumus, un izdrukāt tos vienotā formātā:

@
Test: Normal list
Input: [5,10,11,15,3,8]
Expected: Just 15
Actual: Just 15
Result: PASS
@

Ārējas testēšanas bibliotēkas (piem. HUnit vai QuickCheck) netiek
izmantotas, lai programmu varētu palaist ar standarta GHC instalāciju.
-}
module Utils.Testing
  ( TestResult (..)
  , testCase
  , testCaseWith
  , showText
  , showEither
  , runTests
  ) where

{-|
Viena testa rezultāts.

Lauki:

* @testName@ - testa nosaukums (kas tiek pārbaudīts);
* @testInput@ - ievaddati teksta formā;
* @testExpected@ - gaidītais rezultāts teksta formā;
* @testActual@ - faktiskais rezultāts teksta formā;
* @testPassed@ - vai faktiskais rezultāts sakrīt ar gaidīto.
-}
data TestResult = TestResult
  { testName     :: String
  , testInput    :: String
  , testExpected :: String
  , testActual   :: String
  , testPassed   :: Bool
  }

{-|
Izveido testa rezultātu, salīdzinot gaidīto un faktisko vērtību.
Vērtības tiek attēlotas ar standarta funkciju 'show'.

Parametri:

* @name@ - testa nosaukums;
* @input@ - ievaddatu apraksts;
* @expected@ - gaidītā vērtība;
* @actual@ - funkcijas faktiski atgrieztā vērtība.

Atgrieztā vērtība:

* aizpildīts 'TestResult' ieraksts.
-}
testCase :: (Eq b, Show b) => String -> String -> b -> b -> TestResult
testCase = testCaseWith show

{-|
Tas pats, kas 'testCase', bet ar savu attēlošanas funkciju.

Vajadzīgs teksta vērtībām ar latviešu burtiem: 'show' tos attēlotu
kā kodus (piem. @\"\\257\"@ burta @ā@ vietā).

Parametri:

* @render@ - funkcija, kas vērtību pārvērš tekstā;
* pārējie parametri - kā funkcijai 'testCase'.

Atgrieztā vērtība:

* aizpildīts 'TestResult' ieraksts.
-}
testCaseWith :: Eq b => (b -> String) -> String -> String -> b -> b -> TestResult
testCaseWith render name input expected actual =
  TestResult
    { testName     = name
    , testInput    = input
    , testExpected = render expected
    , testActual   = render actual
    , testPassed   = expected == actual
    }

{-|
Attēlo virkni pēdiņās, nepārveidojot latviešu burtus kodos.

Parametri:

* @text@ - attēlojamā virkne.

Atgrieztā vērtība:

* virkne pēdiņās, piem. @showText "ābols"@ ir @"\\"ābols\\""@.
-}
showText :: String -> String
showText text = "\"" ++ text ++ "\""

{-|
Attēlo 'Either' vērtību, kur 'Left' satur kļūdas paziņojumu.

Parametri:

* @render@ - funkcija 'Right' vērtības attēlošanai;
* @value@ - attēlojamā vērtība.

Atgrieztā vērtība:

* piem. @Left \"empty input\"@ vai @Right [1,2,3]@.
-}
showEither :: (a -> String) -> Either String a -> String
showEither _      (Left err)    = "Left " ++ showText err
showEither render (Right value) = "Right " ++ render value

{-|
Izdrukā testu grupu un kopsavilkumu.

Parametri:

* @title@ - testu grupas nosaukums;
* @tests@ - testu rezultātu saraksts.

Atgrieztā vērtība:

* 'True', ja visi testi ir izturēti, citādi 'False'.
-}
runTests :: String -> [TestResult] -> IO Bool
runTests title tests = do
  putStrLn ("=== " ++ title ++ " ===")
  putStrLn ""
  mapM_ printTest tests
  let passed = length (filter testPassed tests)
  putStrLn ("Tests passed: " ++ show passed ++ "/" ++ show (length tests))
  putStrLn ""
  return (passed == length tests)

-- Izdrukā viena testa rezultātu.
printTest :: TestResult -> IO ()
printTest test = do
  putStrLn ("Test: " ++ testName test)
  putStrLn ("Input: " ++ testInput test)
  putStrLn ("Expected: " ++ testExpected test)
  putStrLn ("Actual: " ++ testActual test)
  putStrLn ("Result: " ++ (if testPassed test then "PASS" else "FAIL"))
  putStrLn ""
