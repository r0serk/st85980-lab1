{-|
Module      : Utils.Input
Description : Droša lietotāja ievades nolasīšana un pārbaude.

Modulis apvieno funkcijas, ar kurām galvenā izvēlne un visi uzdevumi
nolasa un pārbauda lietotāja ievadi.

Katra parsēšanas funkcija atgriež vērtību tipā @Either String a@:

* @Right x@ - ievade ir korekta, @x@ ir pārveidotā vērtība;
* @Left err@ - ievade ir nekorekta, @err@ ir paziņojums lietotājam.

Apstrādātie robežgadījumi:

* tukša ievade vai ievade tikai no atstarpēm;
* burti vai citi simboli skaitļa vietā (piem. @a@, @5x@, @#@);
* decimāldaļskaitlis vesela skaitļa vietā (piem. @3.5@);
* skaitlis ārpus tipa 'Int' diapazona (piem. @99999999999999999999@);
* tukšs saraksta elements (piem. @5,,3@) un neaizvērta iekava;
* ievades beigas (EOF, piem. Ctrl+D Linux vai Ctrl+Z Windows vidē).
-}
module Utils.Input
  ( trim
  , splitOn
  , parseInt
  , parseIntList
  , withDefault
  , readLineSafe
  , promptUntilValid
  , askYesNo
  , waitForEnter
  ) where

import Control.Exception (IOException, try)
import Data.Char (isAlpha, isDigit, isSpace, toLower)
import System.IO (hFlush, stdout)

{-|
Noņem atstarpes, tabulācijas un rindas beigu simbolus virknes sākumā
un beigās.

Parametri:

* @text@ - apstrādājamā virkne.

Atgrieztā vērtība:

* virkne bez atstarpēm sākumā un beigās (atstarpes vidū saglabājas).

Sarežģītība:

* Laika sarežģītība: \(O(n)\).
-}
trim :: String -> String
trim text = reverse (dropWhile isSpace (reverse (dropWhile isSpace text)))

{-|
Sadala virkni daļās pēc dotā atdalītāja simbola.

Atšķirībā no standarta funkcijas 'words' tukšās daļas tiek saglabātas,
tāpēc var atpazīt kļūdainu ievadi, piemēram, divus komatus pēc kārtas.

Parametri:

* @separator@ - atdalītāja simbols (piem. @','@ vai @';'@);
* @text@ - sadalāmā virkne.

Atgrieztā vērtība:

* daļu saraksts, piem. @splitOn ',' "1,,2"@ ir @["1","","2"]@.

Sarežģītība:

* Laika sarežģītība: \(O(n)\).
-}
splitOn :: Char -> String -> [String]
splitOn separator text =
  case break (== separator) text of
    (part, [])       -> [part]
    (part, _ : rest) -> part : splitOn separator rest

{-|
Pārveido teksta virkni veselā skaitlī ('Int'), pārbaudot robežgadījumus.

Atļautais formāts: neobligāta zīme @+@ vai @-@, aiz kuras seko viens
vai vairāki cipari. Atstarpes sākumā un beigās tiek ignorētas.

Parametri:

* @raw@ - lietotāja ievadītā virkne.

Atgrieztā vērtība:

* @Right n@ - ja virkne ir korekts vesels skaitlis tipa 'Int' diapazonā;
* @Left err@ - ja virkne ir tukša, satur burtus, ir decimāldaļskaitlis,
  satur citus simbolus vai skaitlis ir ārpus tipa 'Int' diapazona.

Sarežģītība:

* Laika sarežģītība: \(O(n)\), kur \(n\) ir virknes garums.
-}
parseInt :: String -> Either String Int
parseInt raw
  | null text              = Left "empty input - a number was expected"
  | any isAlpha text       = Left (quote text ++ " is not a number (letters are not allowed)")
  | looksDecimal           = Left (quote text ++ " is not a whole number")
  | not (validSyntax text) = Left (quote text ++ " is not a valid integer")
  | value < minInt || value > maxInt =
      Left (quote text ++ " is outside the Int range ("
            ++ show minInt ++ " .. " ++ show maxInt ++ ")")
  | otherwise              = Right (fromInteger value)
  where
    text   = trim raw
    minInt = toInteger (minBound :: Int)
    maxInt = toInteger (maxBound :: Int)

    -- Decimāldaļskaitlis: tikai cipari, zīme un punkts (piem. 3.5).
    looksDecimal = '.' `elem` text && all (\c -> isDigit c || c `elem` "+-.") text

    -- Korekta sintakse: neobligāta zīme un vismaz viens cipars.
    validSyntax ('-' : digits) = allDigits digits
    validSyntax ('+' : digits) = allDigits digits
    validSyntax digits         = allDigits digits
    allDigits digits = not (null digits) && all isDigit digits

    -- Vērtība tiek aprēķināta tipā Integer (bez pārpildes) un tikai pēc
    -- sintakses pārbaudes, jo Haskell novērtē izteiksmes slinki.
    value :: Integer
    value = case text of
      ('-' : digits) -> negate (read digits)
      ('+' : digits) -> read digits
      digits         -> read digits

{-|
Pārveido teksta virkni veselu skaitļu sarakstā.

Atļautie formāti:

* skaitļi, atdalīti ar komatiem: @5, 10, 11@;
* skaitļi, atdalīti ar atstarpēm: @5 10 11@;
* saraksts kvadrātiekavās: @[5,10,11]@, tukšs saraksts: @[]@.

Parametri:

* @raw@ - lietotāja ievadītā virkne.

Atgrieztā vērtība:

* @Right numbers@ - korekts skaitļu saraksts;
* @Left err@ - paziņojums, kurš elements ir nekorekts un kāpēc
  (tukšs elements, burts, decimāldaļskaitlis, pārāk liels skaitlis),
  vai arī paziņojums par trūkstošu iekavu vai tukšu ievadi.

Sarežģītība:

* Laika sarežģītība: \(O(n)\), kur \(n\) ir virknes garums.
-}
parseIntList :: String -> Either String [Int]
parseIntList raw
  | null text = Left "empty input - a list of numbers was expected"
  | otherwise = do
      inner <- stripBrackets text
      let items | ',' `elem` inner = map trim (splitOn ',' inner)
                | otherwise        = words inner
      mapM parseItem (zip [1 :: Int ..] items)
  where
    text = trim raw

    -- Noņem kvadrātiekavas un pārbauda, vai ir abas iekavas.
    stripBrackets s =
      case (s, reverse s) of
        ('[' : _, ']' : _) -> Right (trim (drop 1 (take (length s - 1) s)))
        ('[' : _, _)       -> Left "missing closing bracket ']'"
        (_, ']' : _)       -> Left "missing opening bracket '['"
        _                  -> Right s

    -- Pārbauda vienu elementu; kļūdas paziņojumā norāda tā numuru.
    parseItem (index, item)
      | null item = Left ("element " ++ show index ++ " is empty")
      | otherwise = case parseInt item of
          Right number -> Right number
          Left err     -> Left ("element " ++ show index ++ ": " ++ err)

{-|
Papildina parsēšanas funkciju ar noklusēto vērtību tukšai ievadei.

Izmanto interaktīvajā daļā: ja lietotājs nospiež Enter, neko neievadot,
tiek izmantots uzdevuma piemērs.

Parametri:

* @defaultValue@ - vērtība, ko atgriezt tukšas ievades gadījumā;
* @parser@ - parsēšanas funkcija pārējiem gadījumiem;
* @input@ - lietotāja ievadītā virkne.

Atgrieztā vērtība:

* @Right defaultValue@, ja ievade ir tukša, citādi @parser input@.
-}
withDefault :: a -> (String -> Either String a) -> String -> Either String a
withDefault defaultValue parser input
  | null (trim input) = Right defaultValue
  | otherwise         = parser input

{-|
Nolasa vienu rindu no standarta ievades, nepieļaujot programmas avāriju.

Ja ievade ir beigusies (EOF, piem. lietotājs nospiež Ctrl+D vai Ctrl+Z),
standarta 'getLine' izraisītu izņēmumu; šī funkcija to pārtver.

Atgrieztā vērtība:

* @Just line@ - nolasītā rinda;
* @Nothing@ - ievade ir beigusies.
-}
readLineSafe :: IO (Maybe String)
readLineSafe = do
  result <- try getLine :: IO (Either IOException String)
  return (either (const Nothing) Just result)

{-|
Izdrukā uzvedni un nolasa ievadi, līdz tā ir korekta.

Nekorektas ievades gadījumā tiek izdrukāts kļūdas paziņojums un ievade
tiek pieprasīta vēlreiz, tāpēc programma neapstājas kļūdas dēļ.

Parametri:

* @prompt@ - uzvednes teksts;
* @parser@ - funkcija, kas pārbauda un pārveido ievadi.

Atgrieztā vērtība:

* @Just value@ - pārbaudīta vērtība;
* @Nothing@ - ievade beidzās (EOF), pirms tika ievadīta korekta vērtība.
-}
promptUntilValid :: String -> (String -> Either String a) -> IO (Maybe a)
promptUntilValid prompt parser = loop
  where
    loop = do
      putStr prompt
      hFlush stdout
      line <- readLineSafe
      case line of
        Nothing    -> return Nothing
        Just input ->
          case parser input of
            Right value -> return (Just value)
            Left err    -> do
              putStrLn ("  Error: " ++ err ++ ". Please try again.")
              loop

{-|
Uzdod jautājumu ar atbildi \"jā\" vai \"nē\".

Pieņem atbildes @y@, @yes@, @n@, @no@ (lielie vai mazie burti).
Citas atbildes, arī tukša ievade, tiek noraidītas, un jautājums tiek
uzdots atkārtoti.

Parametri:

* @prompt@ - jautājuma teksts.

Atgrieztā vērtība:

* 'True', ja atbilde ir \"jā\";
* 'False', ja atbilde ir \"nē\" vai ievade ir beigusies (EOF).
-}
askYesNo :: String -> IO Bool
askYesNo prompt = do
  answer <- promptUntilValid prompt parseYesNo
  return (answer == Just True)
  where
    parseYesNo input =
      case map toLower (trim input) of
        ""    -> Left "empty answer (type y or n)"
        "y"   -> Right True
        "yes" -> Right True
        "n"   -> Right False
        "no"  -> Right False
        other -> Left (quote other ++ " is not a valid answer (type y or n)")

{-|
Gaida, kamēr lietotājs nospiež Enter, lai atgrieztos galvenajā izvēlnē.
-}
waitForEnter :: IO ()
waitForEnter = do
  putStr "\nPress Enter to return to the main menu..."
  hFlush stdout
  _ <- readLineSafe
  putStrLn ""

-- Ieliek tekstu vienpēdiņās kļūdu paziņojumiem, piem. 'abc'.
quote :: String -> String
quote text = "'" ++ text ++ "'"
