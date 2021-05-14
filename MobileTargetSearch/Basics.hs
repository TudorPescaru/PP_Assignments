{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

module Basics where
{-
    Expune funcțiile necesare reprezentării jocului.
-}

import ProblemState
import Data.List
import Data.Maybe

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc.
    Colțul stânga-sus este (0, 0).
-}
type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea Target-urilor.
    Acestea conțin informații atât despre poziția curentă a
    Target-ului cât și despre comportamentul acestuia.
    Tipul Behavior este definit mai jos.
-}
data Target = Target {
    position :: Position,
    behavior :: Behavior
}

instance Eq Target where
    Target p1 _ == Target p2 _ = p1 == p2

instance Ord Target where
    Target p1 _ <= Target p2 _ = p1 <= p2

{-
    Tip de date pentru reprezentarea comportamentului unui Target.
    Tipul Behavior este utilizat pentru a modela tranziția Target-urilor
    din starea curentă în starea următoare. Primul parametru este poziția
    actuală a target-ului, iar al doilea, starea curentă a jocului.
    Tipul Game este definit mai jos.
    
    Observați că, din moment ce un Behavior produce un Target nou,
    acesta din urmă ar putea fi caracterizat de un alt Behavior
    decât cel anterior.
-}
type Behavior = Position -> Game -> Target

{-
    Direcțiile de deplasare pe tablă
-}
data Direction = North | South | West | East
    deriving (Eq, Show)

{-
    *** TODO ***
    
    Tip de date pentru reprezentarea stării jocului, la un anumit
    moment. Completați-l cu orice informație aveți nevoie pentru
    stocarea stării jocului (hunter, target, obstacole, gateways).
-}

data Cell = H | T | G | O | Blank
    deriving (Eq, Ord)

instance Show Cell
    where
        show Blank = " "
        show H = "!"
        show T = "*"
        show G = "#"
        show O = "@"

data Game = Game {
    lns :: Int,
    cols :: Int,
    board :: [Cell],
    hunter :: Position,
    targets :: [Target],
    obstacles :: [Position],
    gateways :: [(Position, Position)]
} deriving (Eq, Ord)
{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Game.
    În cazul acesta, eliminați deriving (Eq, Ord) din Game.
-}

{-
    *** TODO ***

    Reprezentați starea jocului ca șir de caractere, pentru afișarea
    la consolă.
    
    Atenție! Fiecare linie, mai puțin ultima, este urmată de \n.
    Celule goale vor fi reprezentate ca ' '.
    Hunter-ul va fi reprezentat ca '!'.
    Target-urile vor fi reprezentate ca '*'
    Gateways-urile vor fi reprezentate ca '#'.
    Obstacolele vor fi reprezentate de '@'.

    Hint: S-ar putea să vă fie utile list comprehensions,
    precum și funcțiile elem, any și intercalate din Data.List.
-}
gameAsString :: Game -> String
gameAsString (Game l c b _ _ _ _) = init $ unlines [concatMap show $ take c $ drop ((i - 1) * c) b | i <- [1 .. l]]

instance Show Game where
    show = gameAsString

{-
    *** TODO ***
    
    Primește numărul de linii și numărul de coloane ale tablei de joc.
    Intoarce un obiect de tip Game în care tabla conține spații goale în interior, fiind
    împrejmuită de obstacole pe toate laturile. Implicit, colțul din stânga sus este (0,0),
    iar Hunterul se găsește pe poziția (1, 1).
-}
emptyGame :: Int -> Int -> Game
emptyGame l c = Game l c finalBoard (1, 1) [] obstacleList []
    where
        obstacleList = [(i, j) | i <- [0 .. l - 1],
                                 j <- [0 .. c - 1],
                                 i == 0 || j == 0 || i == l - 1 || j == c - 1]
        elemCreate i j
            | i == 1 && j == 1 = H
            | (i, j) `elem` obstacleList = O
            | otherwise = Blank
        finalBoard = [elemCreate i j | i <- [0 .. l - 1], j <- [0 .. c - 1]]

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, cu Hunter-ul pus
    pe poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugat Hunter-ul
    Daca poziția este invalidă (ocupată sau în afara tablei de joc) se va întoarce
    același joc.
-}
addHunter :: Position -> Game -> Game
addHunter newHun@(i, j) game@(Game l c brd hun@(old_i, old_j) tgts obs gtws)
    | valid = Game l c updatedBoard (i, j) tgts obs gtws
    | otherwise = game
    where
        valid
            | i < 0 || i >= l = False 
            | j < 0 || j >= c = False
            | newHun == hun = False 
            | any (\ t -> newHun == position t) tgts = False 
            | newHun `elem` obs = False
            | otherwise = True
        old_idx = old_i * c + old_j
        noHunter
            | old_i == -1 && old_j == -1 = brd
            | otherwise = take old_idx brd ++ [Blank] ++ drop (old_idx + 1) brd
        idx = i * c + j
        updatedBoard = take idx noHunter ++ [H] ++ drop (idx + 1) noHunter

{-
    *** TODO ***

    Primește un comportament, o poziție și un joc și întoarce un nou joc, în care a fost
    adăugat Target-ul descris de comportament și poziție.
    Parametrul Behavior reprezintă comportamentul Hunter-ului care va fi adăugat.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat Target-ul.
-}
addTarget :: Behavior -> Position -> Game -> Game
addTarget bhv pos@(i, j) game@(Game l c brd hun tgts obs gtws)
    | valid = Game l c updatedBoard newHun updatedTargets obs gtws
    | otherwise = game
    where
        newTarget = Target pos bhv
        valid
            | i < 0 || i >= l = False 
            | j < 0 || j >= c = False
            | pos `elem` obs = False
            | otherwise = True
        newHun
            | pos == hun = (-1, -1)
            | otherwise = hun
        idx = i * c + j
        updatedBoard = take idx brd ++ [T] ++ drop (idx + 1) brd
        updatedTargets = insert newTarget tgts


{-
    *** TODO ***

    Primește o pereche de poziții și un joc și întoarce un nou joc, în care au fost adăugate
    cele două gateway-uri interconectate.
    Parametrul (Position, Position) reprezintă pozițiile de pe hartă la care vor fi adăugate 
    cele două gateway-uri interconectate printr-un canal bidirecțional.
-}
addGateway :: (Position, Position) -> Game -> Game
addGateway newGtw@(p1@(x, y), p2@(z, w)) game@(Game l c brd hun tgts obs gtws)
    | valid = Game l c updatedBoard newHun tgts obs updatedGateways
    | otherwise = game
    where
        valid
            | x < 0 || x >= l = False 
            | y < 0 || y >= c = False
            | z < 0 || z >= l = False 
            | w < 0 || w >= c = False
            | any (\ t -> p1 == position t) tgts = False
            | any (\ t -> p2 == position t) tgts = False
            | p1 `elem` obs = False
            | p2 `elem` obs = False
            | any (\ g -> p1 == fst g || p1 == snd g) gtws = False
            | any (\ g -> p2 == fst g || p2 == snd g) gtws = False
            | otherwise = True
        newHun
            | p1 == hun || p2 == hun = (-1, -1)
            | otherwise = hun
        idx1 = x * c + y
        idx2 = z * c + w
        tmpBoard = take idx1 brd ++ [G] ++ drop (idx1 + 1) brd
        updatedBoard = take idx2 tmpBoard ++ [G] ++ drop (idx2 + 1) tmpBoard
        updatedGateways = insert newGtw gtws

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, în care a fost adăugat un obstacol
    la poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat obstacolul.
-}
addObstacle :: Position -> Game -> Game
addObstacle newObs@(i, j) game@(Game l c brd hun tgts obs gtws)
    | valid = Game l c updatedBoard newHun tgts updatedObstacles gtws
    | otherwise = game
    where
        valid
            | i < 0 || i >= l = False 
            | j < 0 || j >= c = False
            | any (\ t -> newObs == position t) tgts = False 
            | newObs `elem` obs = False
            | any (\ g -> newObs == fst g || newObs == snd g) gtws = False
            | otherwise = True
        newHun
            | newObs == hun = (-1, -1)
            | otherwise = hun
        idx = i * c + j
        updatedBoard = take idx brd ++ [O] ++ drop (idx + 1) brd
        updatedObstacles = insert newObs obs

{-
    *** TODO ***
    
    Primește o poziție destinație înspre care vrea să se deplaseze o entitate (Hunter sau Target)
    și verifică daca deplasarea este posibilă, întorcând noua poziție, luând în considerare
    și Gateway-urile.
    Avem următoarele cazuri:
    - dacă poziția corespunde unui spațiu gol, se întoarce acea poziție;
    - dacă poziția corespunde unui gateway, se întoarce poziția gateway-ului pereche;
    - dacă poziția corespunde unui obstacol, se întoarce Nothing.
    Parametrul Position reprezintă poziția destinație.
-}
attemptMove :: Position -> Game -> Maybe Position
attemptMove pos@(i, j) (Game l c _ _ _ obs gtws)
    | i < 0 || i >= l = Nothing
    | j < 0 || j >= c = Nothing
    | any (\ g -> pos == fst g || pos == snd g) gtws = Just getPair
    | pos `elem` obs = Nothing
    | otherwise = Just pos
    where
        getGtw = head $ filter (\ g -> pos == fst g || pos == snd g) gtws
        getPair
            | pos == fst getGtw = snd getGtw
            | otherwise = fst getGtw

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre est. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
    
    Conform definiției, tipul Behavior corespunde tipului funcție
    Position -> Game -> Target.
    
    Având în vedere că cele patru funcții definite în continuare (goEast, goWest,
    goNorth, goSouth) sunt foarte similare, încercați să implementați o funcție
    mai generală, pe baza căreia să le definiți apoi pe acestea patru.
-}

go :: Direction -> Position -> Behavior
go dir newPos pos game = case attemptMove newPos game of
    Just updatedPos -> Target updatedPos (getBhv dir)
    Nothing -> Target (fromMaybe pos (attemptMove pos game)) (getBhv dir)
    where
        getBhv North = goNorth
        getBhv South = goSouth
        getBhv West = goWest
        getBhv East = goEast

goEast :: Behavior
goEast pos@(i, j) = go East (i, j + 1) pos

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre vest. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goWest :: Behavior
goWest pos@(i, j) = go West (i, j - 1) pos

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre nord. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goNorth :: Behavior
goNorth pos@(i, j) = go North (i - 1, j) pos

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre sud. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goSouth :: Behavior
goSouth pos@(i, j) = go South (i + 1, j) pos

{-
    *** TODO ***

    Comportamentul unui Target de a-și oscila mișcarea, când înspre nord, când înspre sud. 
    Mișcarea se poate face doar dacă poziția este validă (se află pe tablă de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul iși va schimba
    direcția de mers astfel:
    - daca mergea inspre nord, își va modifica direcția miscării înspre sud;
    - daca mergea inspre sud, își va continua mișcarea înspre nord.
    Daca Target-ul întâlneste un Gateway pe traseul său, va trece prin acesta,
    către Gateway-ul pereche conectat și își va continua mișcarea în același sens la ieșire
    din acesta.
    Puteți folosit parametrul Int pentru a surprinde deplasamentul Target-ului (de exemplu,
    1 pentru sud, -1 pentru nord).
-}
bounce :: Int -> Behavior
bounce dir pos@(i, j) game = newTarget
    where
        newBhv d
            | d > 0 = (i + 1, j)
            | otherwise = (i - 1, j)
        newPos d
            | isNothing $ attemptMove (newBhv d) game = Target pos (bounce d)
            | d > 0 = go South (newBhv d) pos game
            | otherwise = go North (newBhv d) pos game
        newTarget
            | position (newPos dir) == pos = Target (position $ newPos (-dir)) (bounce (-dir))
            | otherwise = Target (position $ newPos dir) (bounce dir)

{-
    *** TODO ***
    Funcție care mută toate Target-urile din Game-ul dat o poziție, în functie
    de behavior-ul fiecăreia și întoarce noul Game în care pozițiile Target-urilor
    sunt actualizate.
    
-}

recreateGame :: (Int, Int, Position, [(Behavior, Position)], [Position], [(Position, Position)]) -> Game
recreateGame (nRows, nColumns, hntr, targs, obstcls, gates) = addedHunter
  where 
    emptyG = emptyGame nRows nColumns
    addedObstacles = foldl (flip addObstacle) emptyG obstcls
    addedGateways = foldl (flip addGateway) addedObstacles gates
    addedTargets = foldl (flip (uncurry addTarget)) addedGateways targs
    addedHunter = addHunter hntr addedTargets

moveTargets :: Game -> Game
moveTargets game@(Game l c _ hun tgts obs gtws) = recreateGame (l, c, hun,
                                                                [(behavior t, position t) | t <- updatedTargets],
                                                                obs, gtws)
    where
        updatedTargets = map (\ t -> behavior t (position t) game) tgts

{-
    *** TODO ***

    Verifică dacă Targetul va fi eliminat de Hunter.
    Un Target este eliminat de Hunter daca se află pe o poziție adiacentă
    cu acesta.
    Parametrul Position reprezintă poziția Hunterului pe tabla
    de joc.
    Parametrul Target reprezintă Targetul pentru care se face verificarea.
-}
isTargetKilled :: Position -> Target -> Bool
isTargetKilled (i, j) (Target (x, y) _)
    | i == x && (j == y + 1 || j == y - 1) = True 
    | (i == x + 1 || i == x - 1) && j == y = True 
    | otherwise = False


{-
    *** TODO ***

    Avansează starea jocului curent, rezultând starea următoare a jocului.
    Parametrul Direction reprezintă direcția în care se va deplasa Hunter-ul.
    Parametrul Bool specifică dacă, după mutarea Hunter-ului, vor fi
    mutate și Target-urile sau nu, și dacă vor fi eliminate din joc sau nu.
    Este folosit pentru a distinge între desfășurarea reală a jocului (True)
    și planificarea „imaginată” de hunter (False) în partea a doua a temei.

    Avansarea stării jocului respectă următoarea ordine:
    1. Se deplasează Hunter-ul.
    2. În funcție de parametrul Bool, se elimină Target-urile omorâte de către Hunter.
    3. In funcție de parametrul Bool, se deplasează Target-urile rămase pe tablă.
    4. Se elimină Targeturile omorâte de către Hunter și după deplasarea acestora.
    
    Dubla verificare a anihilării Target-urilor, în pașii 2 și 4, îi oferă Hunter-ului
    un avantaj în prinderea lor.
-}
advanceGameState :: Direction -> Bool -> Game -> Game
advanceGameState dir real game = updateGame
    where
        newHun oldHun g = fromMaybe oldHun (attemptMove (newPos oldHun dir) g)
        newPos (i, j) North = (i - 1, j)
        newPos (i, j) South = (i + 1, j)
        newPos (i, j) East = (i, j + 1)
        newPos (i, j) West = (i, j - 1)
        moveHunter g@(Game l c _ hun tgts obs gtws) = recreateGame (l, c, newHun hun g,
                                                                    [(behavior t, position t) | t <- tgts], obs, gtws)
        tgtsAlive hunPos tgtList = filter (not . isTargetKilled hunPos) tgtList
        elimTgts (Game l c _ hun tgts obs gtws) = recreateGame (l, c, hun,
                                                                [(behavior t, position t) | t <- tgtsAlive hun tgts],
                                                                obs, gtws)
        updateGame
            | real = elimTgts $ moveTargets $ elimTgts $ moveHunter game
            | otherwise = moveHunter game


{-
    ***  TODO ***

    Verifică dacă mai există Target-uri pe table de joc.
-}
areTargetsLeft :: Game -> Bool
areTargetsLeft game = not $ null $ targets game

{-
    *** BONUS TODO ***

    Comportamentul unui Target de a se deplasa în cerc, în jurul unui Position, având
    o rază fixată.
    Primul parametru, Position, reprezintă centrul cercului.
    Parametrul Int reprezintă raza cercului.
    Puteți testa utilizând terenul circle.txt din directorul terrains, în conjuncție
    cu funcția interactive.
-}
circle :: Position -> Int -> Behavior
circle center@(x, y) r pos _ = Target getNext (circle center r)
    where
        circleGenerate = up ++ right ++ down ++ left
        s1 = x - r - 1
        s2 = y - r - 1
        e1 = x + r + 1
        e2 = y + r + 1
        up = [(i, j) | i <- [s1], j <- [s2 .. e2]]
        right = [(i, j) | i <- [s1 + 1 .. e1], j <- [e2]]
        down = reverse [(i, j) | i <- [e1], j <- [s2 .. e2 - 1]]
        left = reverse [(i, j) | i <- [s1 .. e1 - 1], j <- [s2]]
        idx = fromMaybe 0 (elemIndex pos circleGenerate)
        getNext
            | idx == length circleGenerate - 1 = head circleGenerate
            | otherwise = circleGenerate !! (idx + 1)

instance ProblemState Game Direction where
    {-
        *** TODO ***
        
        Generează succesorii stării curente a jocului.
        Utilizați advanceGameState, cu parametrul Bool ales corespunzător.
    -}
    successors game = [succNorth, succSouth, succEast, succWest]
        where
            succNorth = (North, advanceGameState North False game)
            succSouth = (South, advanceGameState South False game)
            succEast = (East, advanceGameState East False game)
            succWest = (West, advanceGameState West False game)

    {-
        *** TODO ***
        
        Verifică dacă starea curentă este un în care Hunter-ul poate anihila
        un Target. Puteți alege Target-ul cum doriți, în prezența mai multora.
    -}
    isGoal game
        | any (isTargetKilled $ hunter game) (targets game) = True 
        | otherwise = False 

    {-
        *** TODO ***
        
        Euristica euclidiană (vezi hEuclidian mai jos) până la Target-ul ales
        de isGoal.
    -}
    h game
        | isGoal game = hEuclidean (hunter game) (position tgtKilled)
        | otherwise = hEuclidean (hunter game) (position tgtNearest)
        where
            tgtKilled = head $ filter (isTargetKilled $ hunter game) (targets game)
            tgtNearest = minimumBy (\ x y -> compare (hEuclidean (hunter game) (position x))
                                                     (hEuclidean (hunter game) (position y))) (targets game)

{-
     ** NU MODIFICATI **
-}
hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ ((x1 - x2) ^ pow) + ((y1 - y2) ^ pow)
  where
    pow = 2 :: Int

{-
    *** BONUS ***

    Acesta reprezintă un artificiu necesar pentru testarea bonusului,
    deoarece nu pot exista două instanțe diferite ale aceleiași clase
    pentru același tip.

    OBSERVAȚIE: Testarea bonusului pentru Seach este făcută separat.
-}

newtype BonusGame = BonusGame Game
    deriving (Eq, Ord, Show)

{-
    *** BONUS TODO ***

    Folosind wrapper-ul peste tipul Game de mai sus instanțiați
    ProblemState astfel încât să fie folosită noua euristică. 
-}
instance ProblemState BonusGame Direction where
    {-
        *** BONUS TODO ***

        Pentru a ne asigura că toțî succesorii unei stări sunt de tipul
        BonusGame și folosesc noua euristică trebuie să aplicăm wrapper-ul
        definit mai sus peste toți succesorii unei stări.

        Hint: Puteți să folosiți funcția fmap pe perechi pentru acest lucru.
        https://wiki.haskell.org/Functor
    -}
    successors (BonusGame game) = [succNorth, succSouth, succEast, succWest]
        where
            succNorth = fmap BonusGame (North, advanceGameState North False game)
            succSouth = fmap BonusGame (South, advanceGameState South False game)
            succEast = fmap BonusGame (East, advanceGameState East False game)
            succWest = fmap BonusGame (West, advanceGameState West False game)

    {-
        *** BONUS TODO ***

        Definiți funcția isGoal pentru BonusGame.

        Hint: Folosiți funcția isGoal deja implementată pentru tipul Game.
    -}
    isGoal (BonusGame game)
        | any (isTargetKilled $ hunter game) (targets game) = True 
        | otherwise = False 

    {-
        *** BONUS TODO ***

        Definiți o funcție euristică care este capabilă să găsească un drum mai scurt
        comparativ cu cel găsit de euristica implementată pentru Game.

        ATENȚIE: Noua euristică NU trebuie să fie una trivială.

        OBSERVAȚIE: Pentru testare se va folosi fișierul terrains/game-6.txt.
    -}
    h (BonusGame game)
        | isGoal game = hManhattan (hunter game) (position tgtKilled)
        | otherwise = uncurry distThroughGateway tgtNearestThroughGateway
        where
            tgtKilled = head $ filter (isTargetKilled $ hunter game) (targets game)
            tgtNearestThroughGateway = minimum [(minimumBy (\ x y -> compare (distThroughGateway x tgt)
                                                                             (distThroughGateway y tgt)) 
                                                            (gateways game), tgt) | tgt <- targets game]
            distThroughGateway gtw tgt = min (hManhattan (hunter game) (fst gtw) + hManhattan (snd gtw) (position tgt))
                                             (hManhattan (hunter game) (snd gtw) + hManhattan (fst gtw) (position tgt))

hManhattan :: Position -> Position -> Float
hManhattan (x1, y1) (x2, y2) = fromIntegral $ abs (x1 - x2) + abs (y1 - y2)
