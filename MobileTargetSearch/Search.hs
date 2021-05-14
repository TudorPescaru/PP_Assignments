{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
import qualified Data.PSQueue as PQ
import Data.Maybe
import Prelude
import qualified Data.Set as S

{-
    *** TODO ***
    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:
    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime;
    * estimarea costului până la starea finală;
    * copiii, ce vor desemna stările învecinate;
-}

data Node s a = Node {
    state :: s,
    action :: Maybe a,
    parent :: Maybe (Node s a),
    depth :: Int,
    heuristic :: Float,
    children :: [Node s a]
}

{-
    *** TODO ***
    Instanțiați Eq și Ord pe baza stării.
-}

instance Eq s => Eq (Node s a) where
    s1 == s2 = state s1 == state s2

instance Ord s => Ord (Node s a) where
    s1 <= s2 = state s1 <= state s2

{-
    *** TODO ***
    Gettere folosite pentru accesul la câmpurile nodului
-}

nodeState :: Node s a -> s
nodeState = state

nodeParent :: Node s a -> Maybe (Node s a)
nodeParent = parent

nodeDepth :: Node s a -> Int
nodeDepth = depth

nodeChildren :: Node s a -> [Node s a]
nodeChildren = children

nodeHeuristic :: Node s a -> Float
nodeHeuristic = heuristic

nodeAction :: Node s a -> Maybe a
nodeAction = action

{-
    *** TODO ***
    Generarea întregului spațiu al stărilor.
    Primește starea inițială și creează nodul corespunzător acestei stări,
    având drept copii nodurile succesorilor stării curente, și așa mai
    departe, recursiv.
-}

createStateSpace :: (ProblemState s a, Eq s) => s -> Node s a
createStateSpace initialState = initialNode
    where
        initialNode = createStateSpaceHelper Nothing 0 (Nothing, initialState)

createStateSpaceHelper :: (ProblemState s a, Eq s) => Maybe (Node s a) -> Int -> (Maybe a, s) -> Node s a
createStateSpaceHelper pr dp (ac, st) = node
    where
        node = Node st ac pr dp (h st) c
        c = map (\ successor -> createStateSpaceHelper (Just node) (dp + 1) (Just $ fst successor, snd successor)) 
                (successors st)

{-
    Funcție ce primește o coadă de priorități și întoarce o pereche
    formată din cheia cu prioritatea minimă și coada din care a fost ștearsă
    aceasta.
    Hint: O puteți folosi pentru a extrage și a șterge un nod din frontieră.
-}

deleteFindMin :: (Ord k, Ord p) => PQ.PSQ k p -> (k, PQ.PSQ k p)
deleteFindMin pq = (minK, pq')
    where minK = PQ.key $ fromJust $ PQ.findMin pq
          pq' = PQ.deleteMin pq

{-
    *** TODO ***
    Primește nodul curent și mulțimea stărilor vizitate și întoarce
    o listă cu nodurile succesor nevizitate, care ar putea fi introduse
    în frontieră.
-}

suitableSuccs :: (ProblemState s a, Ord s) => Node s a -> S.Set s -> [Node s a]
suitableSuccs node visited = filter (\ n -> nodeState n `notElem` visited) $ nodeChildren node

{-
    *** TODO ***
    Primește o frontieră (o coadă de priorități) și un nod ce trebuie inserat în aceasta,
    întorcând o nouă frontieră.
    ATENȚIE: Dacă la introducerea unui nod există deja în frontieră un alt nod cu aceeași
    stare, dar cu cost mai mare, nodul nou, cu cost mai mic îl va înlocui pe cel vechi.
    
    Hints:
    1. Vedeți funcția insertWith din pachetul PSQueue.
        (https://hackage.haskell.org/package/PSQueue-1.1.0.1/docs/Data-PSQueue.html#v:insertWith)
    2. Costul se calculează ca suma dintre adâncime și euristică.
-}

insertSucc :: (ProblemState s a, Ord s) => PQ.PSQ (Node s a) Float-> Node s a -> PQ.PSQ (Node s a) Float
insertSucc frontier node = newFrontier
    where
        newFrontier = PQ.insertWith min node nodeCost frontier
        nodeCost = fromIntegral (nodeDepth node) + nodeHeuristic node

{-
    *** TODO ***
    Primește nodul curent, frontiera și mulțimea stărilor vizitate, întorcând noua
    frontieră (coadă de priorități) în care au fost adăugate nodurile succesor validate
    de suitableSuccs.
-}

insertSuccs :: (ProblemState s a, Ord s) => Node s a -> PQ.PSQ (Node s a) Float -> S.Set s -> PQ.PSQ (Node s a) Float
insertSuccs node frontier visited = newFrontier
    where
        newFrontier = foldr (flip insertSucc) frontier $ suitableSuccs node visited

{-
    *** TODO ***
    Funcție helper care implementează A-star.
    Primește o mulțime de noduri vizitate și o coadă de priorități (aka frontiera) și
    întoarce starea finală.
    Se procedează astfel până la întâlnirea unei stări scop:
        - se extrage un nod adecvat din frontireră
        - se marchează starea acestuia ca fiind vizitată
        - se introduc succesorii în frontieră
-}

astar' :: (ProblemState s a, Ord s) => S.Set s -> PQ.PSQ (Node s a) Float -> Node s a
astar' visited frontier = goalNode
    where
        goalNode
            | isGoal $ nodeState firstNode = firstNode
            | otherwise = astar' updatedVisited updatedFrontier
        firstNode = fst $ deleteFindMin frontier
        updatedVisited = S.insert (nodeState firstNode) visited
        updatedFrontier = insertSuccs firstNode (snd $ deleteFindMin frontier) updatedVisited

{-
    *** TODO ***
  
    Primește starea inițială și întoarce starea finală pentru o singură aplicare
    a algoritmului.
    Asigură parametrii inițiali corecți pentru aplicarea funcției astar'.
-}

astar :: (ProblemState s a, Ord s) => Node s a -> Node s a
astar initialNode = goalNode
    where
        goalNode = astar' initialVisited initialFrontier
        initialVisited = S.insert (nodeState initialNode) S.empty
        initialFrontier = insertSucc (insertSuccs initialNode PQ.empty initialVisited) initialNode

{-
    *** TODO ***
    Pornind de la un nod, reface parțial calea către nodul inițial, urmând legăturile
    către părinți.
    Întoarce o listă de perechi (acțiune, stare), care pornește de la starea următoare
    stării inițiale și se încheie la starea finală.
    ATENȚIE: Nodul inițial este singurul exclus!
-}

extractPath :: Node s a -> [(a, s)]
extractPath goalNode = path
    where
        funList = replicate (nodeDepth goalNode) nodeParent
        parentList = tail $ foldr (\ f acc -> fromJust (f $ head acc) : acc) [goalNode] funList
        path = map (\ n -> (fromJust $ nodeAction n, nodeState n)) parentList
