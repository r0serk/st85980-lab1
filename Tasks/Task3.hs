{-|
Module      : Tasks.Task3
Description : Katra teikuma vārda pirmā burta pārveidošana par lielo burtu.

Uzdevums Nr. 3 (8. variants, 2. risinājums).

Modulis nodrošina reducēšanas (fold) funkciju, kas katram vārdam pievieno
teikumu ar atbilstošo pārveidoto vārdu. Teikums tiek sadalīts vārdos, un
funkcija 'foldr' tos apvieno atpakaļ, katra vārda pirmo burtu pārveidojot
par lielo burtu.

Kāpēc 'foldr', nevis 'foldl': ar 'foldl' teikums augtu kreisajā pusē,
un operācija @(++)@ katrā solī kopētu visu līdz šim izveidoto teikumu,
kopā \(O(n^2)\). Ar 'foldr' tiek kopēts tikai īsais vārds, kopā \(O(n)\).

Robežgadījumi, kas tiek pārbaudīti testos:

* latviešu burti, vairākas atstarpes un tabulācijas starp vārdiem;
* vārdi, kas jau sākas ar lielo burtu (piem. @NASA@);
* pieturzīmes pirms vārda (piem. @(hello)@, pēdiņas);
* vārds, kas sākas ar ciparu (piem. @3rd@), un ievade bez burtiem;
* tukša ievade un ievade tikai no atstarpēm.
-}
module Tasks.Task3
  ( capitalizeWord
  , addCapitalizedWord
  , capitalizeSentence
  , capitalizeSentenceFoldl
  , validateSentence
  , reductionSteps
  , runTask3
  ) where

import Control.Monad (when)
import Data.Char (isAlpha, isAlphaNum, toUpper)

import Utils.Input (askYesNo, promptUntilValid, trim)
import Utils.Testing

{-|
Pārveido vārda pirmo burtu par lielo burtu.

Pieturzīmes vārda sākumā (iekavas, pēdiņas) tiek izlaistas, tāpēc
@"(hello)"@ kļūst par @"(Hello)"@. Ja vārds sākas ar ciparu, tas netiek
mainīts (@"3rd"@ paliek @"3rd"@). Pārējie burti paliek nemainīti.

Parametri:

* @word@ - pārveidojamais vārds.

Atgrieztā vērtība:

* vārds ar lielo pirmo burtu.

Sarežģītība:

* Laika sarežģītība: \(O(k)\), kur \(k\) ir pieturzīmju skaits vārda sākumā.
-}
capitalizeWord :: String -> String
capitalizeWord word =
  case span (not . isAlphaNum) word of
    (prefix, c : rest) | isAlpha c -> prefix ++ (toUpper c : rest)
    _                              -> word

{-|
Reducēšanas solis: pievieno pārveidoto vārdu jau apstrādātajai teikuma
daļai (no labās puses).

Parametri:

* @word@ - kārtējais vārds;
* @sentence@ - jau apstrādātā teikuma daļa (pa labi no vārda).

Atgrieztā vērtība:

* teikums, kura sākumā ir pārveidotais vārds un viena atstarpe.
-}
addCapitalizedWord :: String -> String -> String
addCapitalizedWord word sentence
  | null sentence = capitalizeWord word
  | otherwise     = capitalizeWord word ++ (' ' : sentence)

{-|
Pārveido katra teikuma vārda pirmo burtu par lielo burtu (galvenā funkcija).

Teikums tiek sadalīts vārdos ar 'words' (liekās atstarpes un tabulācijas
tiek noņemtas), un vārdi tiek apvienoti ar reducēšanu 'foldr'.

Parametri:

* @sentence@ - apstrādājamais teikums.

Atgrieztā vērtība:

* teikums, kurā katrs vārds sākas ar lielo burtu un vārdus atdala
  viena atstarpe.

Sarežģītība:

* Laika sarežģītība: \(O(n)\), kur \(n\) ir teikuma garums.
-}
capitalizeSentence :: String -> String
capitalizeSentence sentence = foldr addCapitalizedWord "" (words sentence)

{-|
Tā pati funkcija ar 'foldl' - izmantota salīdzināšanai un pārbaudei.

Parametri:

* @sentence@ - apstrādājamais teikums.

Atgrieztā vērtība:

* tāds pats rezultāts kā 'capitalizeSentence'.

Sarežģītība:

* Laika sarežģītība: \(O(n^2)\) operācijas @(++)@ dēļ.
-}
capitalizeSentenceFoldl :: String -> String
capitalizeSentenceFoldl sentence = foldl step "" (words sentence)
  where
    step ""          word = capitalizeWord word
    step accumulator word = accumulator ++ " " ++ capitalizeWord word

{-|
Pārbauda lietotāja ievadi.

Parametri:

* @raw@ - lietotāja ievadītā virkne.

Atgrieztā vērtība:

* @Right sentence@ - teikums bez atstarpēm sākumā un beigās;
* @Left err@ - ja ievade ir tukša vai sastāv tikai no atstarpēm.
-}
validateSentence :: String -> Either String String
validateSentence raw
  | null sentence = Left "empty input - a sentence was expected"
  | otherwise     = Right sentence
  where
    sentence = trim raw

{-|
Atkļūdošanas funkcija: parāda, kā teikums aug reducēšanas laikā.
'foldr' apstrādā vārdus no labās puses uz kreiso.

Parametri:

* @sentence@ - teikums.

Atgrieztā vērtība:

* starprezultātu saraksts, sākot ar pēdējo vārdu.
-}
reductionSteps :: String -> [String]
reductionSteps sentence = drop 1 (reverse (scanr addCapitalizedWord "" (words sentence)))

-- | Funkcijas 'capitalizeSentence' testi, ieskaitot robežgadījumus.
functionTests :: [TestResult]
functionTests =
  [ test "Normal sentence" "convert the first letter of every word"
      "Convert The First Letter Of Every Word"
  , test "Latvian letters" "ēdam ābolus un ķiršus" "Ēdam Ābolus Un Ķiršus"
  , test "Extra spaces and tabs" "  hello \t  world  " "Hello World"
  , test "Words already in capitals" "NASA and haskell" "NASA And Haskell"
  , test "Punctuation before a word" "(hello) \"world\"" "(Hello) \"World\""
  , test "Word starting with a digit" "3rd place" "3rd Place"
  , test "Single word" "haskell" "Haskell"
  , test "No letters at all" "123 456" "123 456"
  , testCase "foldr and foldl versions give the same result" "all sentences above" True
      (and [ capitalizeSentence s == capitalizeSentenceFoldl s
           | s <- [ "convert the first letter", "  hello \t  world  "
                  , "(hello) \"world\"", "3rd place", "123 456", "" ] ])
  ]
  where
    test name input expected =
      testCaseWith showText name (showText input) expected (capitalizeSentence input)

-- | Lietotāja ievades pārbaudes testi (robežgadījumi).
inputTests :: [TestResult]
inputTests =
  [ test "Valid sentence" "hello world" (Right "hello world")
  , test "Empty input" "" (Left "empty input - a sentence was expected")
  , test "Only spaces" "    " (Left "empty input - a sentence was expected")
  ]
  where
    test name input expected =
      testCaseWith (showEither showText) name (showText input) expected (validateSentence input)

{-|
Uzdevuma Nr. 3 izpilde no galvenās izvēlnes: testi un interaktīvā daļa.
-}
runTask3 :: IO ()
runTask3 = do
  putStrLn "--- Task 3: Capitalize Every Word ---"
  putStrLn ""
  _ <- runTests "Function tests" functionTests
  _ <- runTests "Input validation tests (edge cases)" inputTests
  interactivePart

-- Interaktīvā daļa: lietotājs ievada teikumus.
interactivePart :: IO ()
interactivePart = do
  putStrLn "=== Try it yourself ==="
  loop
  where
    loop = do
      input <- promptUntilValid "Enter a sentence: " validateSentence
      case input of
        Nothing       -> return ()
        Just sentence -> do
          putStrLn "Reduction steps (the sentence grows from the right):"
          mapM_ (\step -> putStrLn ("  " ++ showText step)) (reductionSteps sentence)
          putStrLn ("Result: " ++ showText (capitalizeSentence sentence))
          again <- askYesNo "Try another sentence? (y/n): "
          when again loop
