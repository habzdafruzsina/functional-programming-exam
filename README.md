# Funkcionális programozás

ELTE IK
_ beadandó 2023-24-1_
_tárgyfelelős: Horpácsi Dániel_

## 2024.01.17. vizsga: gyakorlati rész
### Nyilatkozat
A feladatsor beadásával az alábbi nyilatkozatot elfogadja a megoldó:
“Nyilatkozom, hogy a vizsgán beadott megoldásomat minden tekintetben önállóan készítettem. Nem megengedett eszközt és nem megengedett külső segítséget nem vettem igénybe, illetve másnak sem nyújtottam. A megoldás során nem használtam mesterséges intelligencia-alapú keresőprogramot, chatbotot, kódgeneráló programot (pl. chatGPT, Google Bard, Github Copilot).”
### Általános tudnivalók
- A feladatokat nem kell sorrendben megoldani! A feladatokhoz tartozó pontszám nehézség szerint különbözik.
- Érdemes lehet importálni a következő modulokat: Data.Char, Data.List, Data.Foldable.
- Érdemes a fájl elejére beilleszteni a {-# LANGUAGE InstanceSigs #-} utasítást, amely engedélyezi régebbi Haskell verziókban (a TMS-en futó verzió is ilyen) függvényszignatúra megadását típusosztály példányokban.
- A megoldásnak meg kell felelnie a feladathoz tartozó függvényszignatúráknak.
- A beküldött megoldásnak le kell fordulnia.
- A következő szabványkönyvtárbeli függvények segíthetnek a megoldás során (természetesen más függények is felhasználhatók a standard könyvtárból): (!!), (++), all, any, ceiling, chr, concatMap, cycle, div, drop, dropWhile, elem, even, filter, flip, floor, foldl, foldr, fromIntegral, head, isDigit, isAlpha, isLetter, isPrefixOf, length, lines, map, maximum, minimum, mod, null, odd, pi, repeat, replicate, reverse, show, splitAt, sqrt, tail, take, takeWhile (lásd Hoogle (Linkek egy külső oldalra)).
- Ha valamelyik feladatot nem tudjátok megoldani úgy, hogy leforduljon, akkor hagyjátok meg a fügvényszignatúrát és a definíció legyen undefined, továbbá a nem forduló megoldásodat hagyd ott kommentben, így lehet, hogy kapsz rá részpontot! Pl.:
```
fuggveny :: Int -> Int
fuggveny = undefined
{-
fuggveny n = "foo"
-}
```
Feladat beadási módja:
- A megoldást egy .hs kiterjesztésű fájlba írjátok
- Ezt a .hs fájlt csomagoljátok be egy .zip állományba
- Az így kapott .zip állományt töltsétek fel Canvasba, a feladat megoldásaként
### Feladatok
#### scalarProd (2 pont)
Definiáld két n-dimenziós vektor skaláris szorzatát (Linkek egy külső oldalra)! A vektorokat listákkal reprezentáljuk, és feltehetjük, hogy a hosszuk megegyezik.

```
scalarProd :: Num a => [a] -> [a] -> a

-- TESZTEK:
scalarProd [1..10] [1..10] == 385
scalarProd [] [] == 0
scalarProd [3] [4] == 12
scalarProd [1,2] [3,4] == 11
```
#### isPerfect (3 pont)
Döntsd el egy természetes számról, hogy tökéletes-e! Akkor nevezünk egy számot tökéletesnek, ha megegyezik a nála kisebb osztóinak összegével. Ekvivalens átfogalmazás: egy tökéletes szám kétszerese megegyezik az összes osztójának összegével.
```
isPerfect :: Integer -> Bool

-- TESZTEK:
isPerfect 6
isPerfect 28
isPerfect 496
isPerfect 8128
all (not . isPerfect) [497..1000]
```
#### initials (3 pont)
Írj függvényt, amely megadja egy név monogramját! A nevet egy lista ábrázolja, amely minden eleme egy kereszt- vagy vezetéknév.
```
initials :: [String] -> String

-- TESZTEK:
initials ["Kis", "Bela"] == "K.B."
initials ["Kis", "Bela", "Jozsef"] == "K.B.J."
initials ["Emanuel", "Jose", "Alejandro", "Somers"] == "E.J.A.S."
initials ["Horvath", "Anna", "Sara", "Katalin", "Eva", "Lukrecia", "Lilla"] == "H.A.S.K.E.L.L."
```
#### countLowerCase (3 pont)
Add meg, hogy egy szövegben hány darab angol ábécébeli kisbetű szerepel!
```
countLowerCase :: String -> Int

-- TESZTEK:
countLowerCase "almafa" == 6
countLowerCase "MaCSka" == 3
countLowerCase "" == 0
countLowerCase (take 100 (cycle "ÁrvíZtŰrő tÜkörFÚRÓgéP")) == 37
```
#### countDown (3 pont)
A Clock adattípus egy ketyegő óra állapotait valósítja meg. Írj függvényt amely ezt a típust felhasználva a függvény paramétertől elszámol 0-ig (felépít egy listát), úgy hogy a páros számokat a Tick, míg a páratlanokat a Tock tárolja, végül a 0 helyén Alarm legyen! Megjegyzés: Később meg kell adni egy Eq példányt az alábbi típushoz, ezért a tesztekhez másold be a megoldásodba a lent megadott identicalClock függvényt!
```
data Clock = Tick Int | Tock Int | Alarm deriving (Show)

identicalClock :: Clock -> Clock -> Bool
identicalClock Alarm Alarm = True
identicalClock (Tick x) (Tick y) = x == y
identicalClock (Tock x) (Tock y) = x == y
identicalClock _ _ = False

countDown :: Int -> [Clock]

-- TESZTEK:
and (zipWith identicalClock (countDown 0) [Alarm])
and (zipWith identicalClock (countDown 10) [Tick 10, Tock 9, Tick 8, Tock 7, Tick 6, Tock 5, Tick 4, Tock 3, Tick 2, Tock 1, Alarm])
and (zipWith identicalClock (countDown 2) [Tick 2, Tock 1, Alarm])
and (zipWith identicalClock (countDown 1) [Tock 1, Alarm])
```
#### Eq Clock (3 pont)
Az előző feladatbeli óra adattípushoz add meg az Eq típusosztály példányát! Két Clock típusú érték legyen akkor egyenlő, ha ugyanazzal a szám paraméterrel rendelkeznek, vagy mindkettő Alarm! (Tehát pl. Tick 1 == Tock 1.)
```
-- TESZTEK:
Tick 1 == Tock 1
Tick 1 == Tick 1
Tock 1 == Tock 1
Alarm == Alarm
Alarm /= Tick 1
Alarm /= Tock 1
Tick 1 /= Alarm
Tock 1 /= Alarm
map Tick [1..100] == map Tock [1..100]
zipWith ($) (cycle [Tick, Tock]) [100..1000] == zipWith ($) (cycle [Tick, Tock, Tick]) [100..1000]
```
#### sumEveryThird (4 pont)
Adj meg egy függvényt, amely összeadja egy lista minden harmadik elemét, ha az az elem páros (egyébként ezt az elemet kihagyja)!
```
sumEveryEvenThird :: [Int] -> Int

-- TESZTEK:
sumEveryEvenThird [1..100] == 816
sumEveryEvenThird (replicate 100 1) == 0
sumEveryEvenThird [2,4..100] == 816
sumEveryEvenThird [1,2,4,1,2,4,1,2,3] == 8
sumEveryEvenThird [] == 0
```
#### kLines (4 pont)
Add meg egy szöveg azon sorainak sorszámait, amelyek tartalmaznak 'k' betűt! Tipp: használd a lines függvényt!
```
kLines :: String -> [Int]

-- TESZTEK:
kLines "" == []
kLines "alma\n" == []
kLines "cica\n\nkutya\n\n\nalma" == [3]
kLines "macska\n\nkutya\n\n\nalmak" == [1,3,6]
kLines (take 200 $ cycle "k\n") == [1..100]
```
#### isomorph (4 pont)
Adj meg két függvényt, amelyek megmutatják, hogy a Maybe (Bool, a) és Either a (Maybe a) típusok izomorfak (azaz a két függvény kompozíciója identitás)!
```
isomorph1 :: Maybe (Bool, a) -> Either a (Maybe a)
isomorph2 :: Either a (Maybe a) -> Maybe (Bool, a)

-- TESZTEK:
(isomorph2 . isomorph1) Nothing == Nothing
map (isomorph2 . isomorph1) (map Just (zip (cycle [True, False]) [1..100])) == (map Just (zip (cycle [True, False]) [1..100]))
(isomorph1 . isomorph2) (Right Nothing) == Right Nothing
map (isomorph1 . isomorph2) (map Left [1..100]) == (map Left [1..100])
map (isomorph1 . isomorph2) (map (Right . Just) [1..100]) == (map (Right . Just) [1..100])
```
#### stern (5 pont)
Adj meg egy függvényt, amely megadja a Moritz Stern diatomikus sorozatot az n-edik eleméig (n a függvény paramétere). A sorozatot (S-sel jelölve) a következőképpen definiáljuk:
- S(0) := 0
- S(1) := 1
- Ha n páros, azaz n = 2 * k valamilyen k-ra: S(2 * k) := S(k)
- Ha n páratlan, azaz n = 2 * k + 1 valamilyen k-ra: S(2 * k + 1) := S(k) + S(k + 1)
Tippek:
- Használj egészosztást a sorozat páros és páratlan elemeinek meghatározására!
- A felépítendő lista i-edik eleme reprezentálja az i-edik “stern számot”! Ezt felhasználva az általános esetben indexeléssel le lehet kérdezni a már kiszámolt “stern számokat” a rekurzív eredményből.
- Az éppen kiszámolt “stern számot” a korábban kiszámoltak listájának végére fűzd!
- Az első 20 “stern számot” megtalálod a tesztek között.
```
stern :: Int -> [Int]

-- TESZTEK:
stern 20 == [0,1,1,2,1,3,2,3,1,4,3,5,2,5,3,4,1,5,4,7,3]
take 10 (drop 100 (stern 1000)) == [7,19,12,17,5,18,13,21,8,19]
take 10 (drop 900 (stern 1000)) == [16,45,29,42,13,49,36,59,23,56]
```
Érdekesség: Ha a fenti sorozatból elkészítjük a következő sorozatot: F(n) := S(n)/S(n + 1), akkor ez a sorozat legenerálja az összes nemnegatív (egyszerűsített) racionális számot úgy, hogy mindegyik csak egyszer szerepel a sorozatban. Ezzel rögtön az bizonyítható, hogy ugyanannyi nemnegatív racionális és természetes szám van, ugyanis ezzel a sorozattal tulajdonképpen “indexeltük” az összes nemnegatív racionális számot, ami egy bijekció a nemnegatív racionális számok, és a természetes számok között. Lásd az alábbi függvényt, amely egy limitig megadja ezeket a racionális számokat.
```
allRationalNumbers limit = [(xs !! n, xs !! (n + 1)) | n <- [0..(limit-1)], let xs = stern limit]
```
#### evs (6 pont)
Adott egy autókereskedés adatbázisa, amely (rendszám, autótípus, üzemanyag) hármasok formájában tárolja a gépjárművek adatait. Az üzemanyag típusát az alábbi felsorolási típussal adtuk meg:
```
data Fuel = MHEV | HEV | PHEV | EV | Gasoline | Diesel deriving (Eq, Show)
```
Add meg, hogy hány darab, érvényes rendszámmal rendelkező elektromos (üzemanyaga: EV) autót forgalmaz a kereskedés! Akkor tekinthető egy rendszám érvényesnek, ha megfelel a 2022-ben bevezetett új rendszernek, azaz 4 db angol ábécébeli nagybetűvel kezdődik, az 5. karakter egy kötőjel, és 3 számjeggyel végződik! Tipp: érdemes lehet a rendszám ellenőrzésére egy segédfüggvényt definiálni!
```
evs :: [(String, String,Fuel)] -> [String]

-- TESZTEK:
evs [("AABC-123","Volvo V90",PHEV),("AABC-999","KIA Rio",Gasoline),("AAAA-123","Dacia Spring",EV)] == ["Dacia Spring"]
evs [("AABC-123","Volvo V90",PHEV),("AABC-999","KIA Rio",Diesel),("AAAA123","Dacia Spring",EV)] == []
evs [("AABC-123","Volvo V90",PHEV),("AABC-999","KIA C'EED",HEV),("AAA-123","Dacia Spring",EV)] == []
evs [("AABC-123","Volvo V90",PHEV),("AABC-999","Mazda 3",MHEV),("AAAŐ-123","Dacia Spring",EV)] == []
evs [("AABC-123","Volvo V90",PHEV),("AABC-999","KIA Rio",Gasoline),("AAAA-1234","Dacia Spring",EV)] == []
evs [("TSLA-123","Volvo EX90",EV),("AAAA-001","BMW iX",EV),("AAAA-00","Renault Zoe",EV)] == ["Volvo EX90","BMW iX"]
evs [("TSLA-A12","Volvo EX90",EV),("AAAA-0011","BMW iX",EV),("AAAA-00","Renault Zoe",EV)] == []
```
