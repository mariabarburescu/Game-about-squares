{-# OPTIONS_GHC -Wall #-}

module Search where

import ProblemState
import Data.Maybe

import qualified Data.Set as S

{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime.
-}
data Node s a = Node s (Maybe a) (Node s a) Int | EmptyNode
    deriving (Eq, Show)

{-
    *** TODO ***

    Întoarce starea stocată într-un nod.
-}
nodeState :: Node s a -> s
nodeState (Node s _ _ _) = s

{-
    *** TODO ***

    Întoarce lista nodurilor rezultate prin parcurgerea limitată în adâncime
    a spațiului stărilor, pornind de la starea dată ca parametru.

    Pentru reținerea stărilor vizitate, recomandăm Data.Set. Constrângerea
    `Ord s` permite utilizarea tipului `Set`.

    În afara BONUS-ului, puteți ignora parametrul boolean. Pentru BONUS, puteți
    sorta lista succesorilor folosind `sortBy` din Data.List.
-}
dfsD :: (ProblemState s a, Ord s)
           => [s] 
           -> Node s a
           -> [(a, s)]
           -> S.Set s
           -> [Node s a]
           -> Int
           -> Int
           -> [Node s a]
dfsD queue nod@(Node _ _ p _) suc visitedS visited d depth =
    if d == depth && (length queue) == 1 then visited
    else
        if null suc then 
            dfsD queue p (successors (nodeState p)) visitedS visited (d-1) depth
        else
            let ac = Just (fst (head suc));
                st = snd (head suc);
                succ = tail suc;
                q = tail queue;
            in
                
                if d == depth then
                    dfsD q p (successors (nodeState p)) visitedS visited (d-1) depth
                else 
                    if S.member st visitedS then
                        dfsD queue nod succ visitedS visited d depth
                    else
                        dfsD ((map snd suc) ++ q) (Node st ac nod (d+1)) (successors st) (S.insert st visitedS) (visited ++ [(Node st ac nod (d+1))]) (d+1) depth


limitedDfs :: (ProblemState s a, Ord s)
           => s           -- Starea inițială
           -> Bool        -- Pentru BONUS, `True` dacă utilizăm euristica
           -> Int         -- Adâncimea maximă de explorare
           -> [Node s a]  -- Lista de noduri
limitedDfs st _ depth = 
    dfsD [st] (Node st Nothing EmptyNode 0) (successors st) (S.insert st visitedS) (visited ++ [(Node st Nothing EmptyNode 0)]) 0 depth
    where visitedS = S.empty
          visited = []

{-
    *** TODO ***

    Explorează în adâncime spațiul stărilor, utilizând adâncire iterativă,
    pentru determinarea primei stări finale întâlnite.

    Întoarce o perche între nodul cu prima stare finală întâlnită și numărul
    de stări nefinale vizitate până în acel moment.

    În afara BONUS-ului, puteți ignora parametrul boolean.
-}

checkNode :: (ProblemState s a, Ord s) =>  [Node s a] -> Bool
checkNode d =
    if null d then False
    else
        if isGoal (nodeState (head d)) then True
        else checkNode (tail d)

findDepth :: (ProblemState s a, Ord s) =>  s -> Int -> Int
findDepth st depth = 
    if checkNode (limitedDfs st False depth) then depth
    else findDepth st (depth+1)

findNode :: (ProblemState s a, Ord s) =>  [Node s a] -> (Node s a)
findNode d = 
    if isGoal (nodeState (head d)) then (head d)
    else findNode (tail d)

findNumber1 :: (ProblemState s a, Ord s) => s -> Int -> Int -> Int -> Int
findNumber1 st findNr nr s =
    if findNr == nr then s 
    else findNumber1 st (findNr+1) nr (s+(length (limitedDfs st False findNr)))

findNumber2 :: (ProblemState s a, Ord s) => [Node s a] -> Int -> Int
findNumber2 d nr = 
    if isGoal (nodeState (head d)) then nr
    else findNumber2 (tail d) (nr+1)

iterativeDeepening :: (ProblemState s a, Ord s)
    => s                -- Starea inițială
    -> Bool             -- Pentru BONUS, `True` dacă utilizăm euristica
    -> (Node s a, Int)  -- (Nod cu prima stare finală,
                        --  număr de stări nefinale vizitate)
iterativeDeepening st _ = 
    (findNode (limitedDfs st False (findDepth st 0)), (findNumber1 st 0 (findDepth st 0) 0) + (findNumber2 (limitedDfs st False (findDepth st 0)) 0))



{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}

extractAcSt :: Node s a -> (a, s)
extractAcSt (Node st ac _ _) = ((fromJust ac), st)

extract :: Node s a -> [(a, s)] -> [(a, s)]
extract (Node st ac p _) result =
    if isNothing ac then reverse result
    else extract p (result ++ [((fromJust ac), st)])

extractPath :: Node s a -> [(a, s)]
extractPath (Node st ac p _) = 
    if isNothing ac then []
    else
        extract p [((fromJust ac), st)]

{-
    Poate fi utilizată pentru afișarea fiecărui element al unei liste
    pe o linie separată.
-}
printSpacedList :: Show a => [a] -> IO ()
printSpacedList = mapM_ (\a -> print a >> putStrLn (replicate 20 '*'))