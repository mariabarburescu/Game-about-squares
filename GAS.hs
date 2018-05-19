{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances #-}

module GAS where

import ProblemState

import qualified Data.Char as C
import Data.Maybe

import qualified Data.Map.Strict as M

{-
    Pozițiile tablei de joc, în formă (linie, coloană), unde ambele coordonate
    pot fi negative.
-}
type Position = (Int, Int)

{-
    Culorile pătratelor și cercurilor.
-}
data Color = Red | Blue | Gray
    deriving (Eq, Ord, Show)

{-
    Orientările pătratelor și săgeților.
-}
data Heading = North | South | East | West
    deriving (Eq, Ord)

instance Show Heading where
    show North = "^"
    show South = "v"
    show East  = ">"
    show West  = "<"

{-
    *** TODO ***

    Un obiect de pe tabla de joc: pătrat/ cerc/ săgeată.
-}
data Object = Square Color Heading | Circle Color | Arrow Heading | Empty
    deriving (Eq, Ord)

{-
    *** TODO ***

    Reprezetarea textuală a unui obiect.
-}
instance Show Object where
    show (Square color heading) = [(show color) !! 0] ++ show heading
    show (Circle color) = [C.toLower ((show color) !! 0)]
    show (Arrow heading) = show heading
    show Empty = ""

{-
    *** TODO ***

    Un nivel al jocului.

    Recomandăm Data.Map.Strict.
-}
data Level = Level (M.Map Position (Object, Object))
    deriving (Eq, Ord)

{-
    *** TODO ***

    Reprezetarea textuală a unui nivel.
-}

findBounds :: Level -> [Position]
findBounds (Level myMap) = [(minimum [x | x <- map fst pos],
							maximum [x | x <- map fst pos]),
							(minimum [y | y <- map snd pos],
							maximum [y | y <- map snd pos])]
							where pos = M.keys myMap

allPositions :: Level -> [Position]
allPositions level = [(x, y) | x <- [fst (head (findBounds level))..snd (head (findBounds level))],
							   y <- [fst (head (tail (findBounds level)))..snd (head (tail (findBounds level)))]]

printObject :: Position -> Level -> String
printObject p l@(Level myMap) = 
	let object = M.lookup p myMap;
		xmax = snd (head (findBounds l));
		ymax = snd (head (tail (findBounds l)));
		(x, y) = p;
	in 
		if isNothing object then if y == ymax && x == xmax then "   "
		else
			if y == ymax then "   \n"
			else "   |"
		else
			let obj1 = fst (fromJust (M.lookup p myMap)); 
				obj2 = snd (fromJust (M.lookup p myMap)); 
			in
				case (obj1, obj2) of 
					((Square _ _), Empty) ->
						if y == ymax && x == xmax then show obj1 ++ " "
						else if y == ymax then show obj1 ++ " \n"
							else show obj1 ++ " |"
					((Square _ _), _) ->
						if y == ymax && x == xmax then show obj1 ++ show obj2
						else if y == ymax then show obj1 ++ show obj2 ++ "\n"
							else show obj1 ++ show obj2 ++ "|"
					(Empty, _) ->
						if y == ymax && x == xmax then "  " ++ show obj2
						else if y == ymax then "  " ++ show obj2 ++ "\n"
							else "  " ++ show obj2 ++ "|"
		

instance Show Level where
    show level = let positions = allPositions level
    			  in concat (map (\pos -> (printObject pos level)) positions)

{-
    *** TODO ***

    Nivelul vid, fără obiecte.
-}
emptyLevel :: Level
emptyLevel = (Level M.empty)

{-
    *** TODO ***

    Adaugă un pătrat cu caracteristicile date la poziția precizată din nivel.
-}
addSquare :: Color -> Heading -> Position -> Level -> Level
addSquare c h p (Level myMap) = 
	let object = M.lookup p myMap;
	in 
		if isNothing object then
			(Level (M.insert p ((Square c h), Empty) myMap))
		else
			let obj2 = snd (fromJust (M.lookup p myMap));
			in
				(Level (M.insert p ((Square c h), obj2) myMap))
{-
    *** TODO ***

    Adaugă un cerc cu caracteristicile date la poziția precizată din nivel.
-}
addCircle :: Color -> Position -> Level -> Level
addCircle c p (Level myMap) =
	let object = M.lookup p myMap;
	in 
		if isNothing object then
			(Level (M.insert p (Empty, (Circle c)) myMap))
		else
			let obj1 = fst (fromJust (M.lookup p myMap));
			in
				(Level (M.insert p (obj1, (Circle c)) myMap))

{-
    *** TODO ***

    Adaugă o săgeată cu caracteristicile date la poziția precizată din nivel.
-}
addArrow :: Heading -> Position -> Level -> Level
addArrow h p (Level myMap) = 
	let object = M.lookup p myMap;
	in 
		if isNothing object then
			(Level (M.insert p (Empty, (Arrow h)) myMap))
		else
			let obj1 = fst (fromJust (M.lookup p myMap));
			in
				(Level (M.insert p (obj1, (Arrow h)) myMap))

{-
    *** TODO ***

    Mută pătratul de la poziția precizată din nivel. Dacă la poziția respectivă
    nu se găsește un pătrat, întoarce direct parametrul.
-}
getDirSquare :: Object -> Heading
getDirSquare (Square _ h) = h

getDirArrow :: Object -> Heading
getDirArrow (Arrow h) = h

moveAux :: Position -> Level -> Object -> Heading -> Level
moveAux p l@(Level myMap) (Square c hd) h =
	let newP = (((fst p)+cx), ((snd p)+cy));
	in
		let object = M.lookup newP myMap;
		in
			if isNothing object then
				addSquare c hd newP l
			else
				let	maybeSquare = fst (fromJust (M.lookup newP myMap));
					secondObj = snd (fromJust (M.lookup newP myMap));
				in
					case (maybeSquare, secondObj) of
						(Empty, (Arrow _)) ->
							addSquare c (getDirArrow secondObj) newP l
						(Empty, _) ->
							addSquare c hd newP l
						((Square _ _), (Arrow _)) ->
							moveAux newP (addSquare c (getDirArrow secondObj) newP l) maybeSquare h
						((Square _ _), _) ->
							moveAux newP (addSquare c hd newP l) maybeSquare h
		where
			cx = if h == North then -1 
					else if h == South then 1 else 0
			cy = if h == West then -1 
					else if h == East then 1 else 0

move :: Position  -- Poziția
     -> Level     -- Nivelul inițial
     -> Level     -- Nivelul final
move p l@(Level myMap) = 
	let
		dir = getDirSquare (fst (fromJust (M.lookup p myMap)));
		squareToMove = fst (fromJust (M.lookup p myMap));
		secondObj = snd (fromJust (M.lookup p myMap));
	in
		if not (M.member p myMap) then l
		else
			case (squareToMove, secondObj) of
				((Square _ _), Empty) ->
					moveAux p (Level (M.delete p myMap)) squareToMove dir
				((Square _ _), _) ->
					moveAux p (Level (M.insert p (Empty, secondObj) myMap)) squareToMove dir
				_ -> l
{-
    *** TODO ***

    Instanțiați clasa `ProblemState` pentru jocul nostru.
-}

checkSquare :: Position -> Level -> Bool
checkSquare p (Level myMap) = 
	let obj1 = fst (fromJust (M.lookup p myMap));
	in
		case obj1 of
			(Square _ _) -> True
			_ -> False

checkGoal :: Position -> Level -> Bool
checkGoal p (Level myMap) = 
	let obj1 = fst (fromJust (M.lookup p myMap));
		obj2 = snd (fromJust (M.lookup p myMap));
	in
		case (obj1, obj2) of
			((Square cS _), (Circle cC)) ->
				if cS == cC then True else False
			((Square _ _), _) -> False
			(Empty, _) -> True


instance ProblemState Level Position where
    successors level@(Level myMap) = 
    	let positions = M.keys myMap;
    		squareLevels = filter (\pos -> (checkSquare pos level)) positions;
    	in map (\pos -> (pos, move pos level)) squareLevels

    isGoal level@(Level myMap) = 
    	let positions = M.keys myMap;
    	in all (\pos -> checkGoal pos level) positions

    -- Doar petru BONUS
    -- heuristic =
