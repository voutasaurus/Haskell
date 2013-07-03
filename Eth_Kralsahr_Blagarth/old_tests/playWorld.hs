{-# LANGUAGE TemplateHaskell #-}

import Control.Lens
import System.IO   
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

-------------------------
-- Objects in the game --
-------------------------

type Items 					= [String]
type EnvironmentObjects 	= [String]
type PlaceName = String

-- Location state
data Place = Place 	{ _placeName :: PlaceName
					, _visibles :: EnvironmentObjects
					, _interactables :: EnvironmentObjects 
					, _connections :: [PlaceName]
					} deriving (Show) 

-- World State
data World = World 	{ _playerName :: String
					, _inventory :: Items
					, _currentLocation :: Place
					, _knownLocations :: [Place]
					, _unknownLocations :: [Place]
					} deriving (Show) 

-- Lenses for the above
$( makeLenses ''Place )
$( makeLenses ''World )

---------------------
----- GAME MAP ------
-- Initial states for the locations
---------------------

-- Starting point (outside Eth Kralsahr Blagarth)
initDoorOfKral :: Place
initDoorOfKral = Place
	{ _placeName = "The Door of Kral"
	, _visibles = ["the Door of Kral", "the left torch", "the right torch"]
	, _interactables = ["the right torch"]
	, _connections = []
	}

-- Just inside the Door of Kral
initAntechamber :: Place
initAntechamber = Place
	{ _placeName = "The Antechamber of Eth Kralsahr Blagath"
	, _visibles = ["the Door of Kral", "star globe", "skeleton"]
	, _interactables = ["the Door of Kral"]
	, _connections = ["The Door of Kral"]
	}

---------------------
------ WORLD  -------
-- Initial state for the world
---------------------
	
-- Initial game state
initialState :: World
initialState = World
	{ _playerName = "Grogdan the immolator"
	, _inventory = ["test tube 1", "test tube 2"]
	, _currentLocation = initDoorOfKral
	, _knownLocations = [initDoorOfKral]
	, _unkownLocations = [initAntechamber]
	}

move :: StateT World IO ()
move = do
	lift $ putStrLn 
	
-- Main function dummy
main = do   
	putStrLn "Success"

-- Main function (interact version)
-- main = interact respondByLine

-- Line by line game, no memory
respondByLine = unlines . map (action ["test"]) . lines  

-- Single action in game
action :: Items -> String -> String
action inventory = acting inventory . words

-- Act, but now we've broken the action into a list of words
acting :: Items -> [String] -> String
acting _ [] = "You do nothing. You feel uneasy for a moment. You are eaten by a Yodrathi Mouseman."
acting inventory (x:xs)
	| x == "look" 	= look inventory xs
	| x == "use"	= useObject inventory xs
	| x == "get"	= get xs
	| x == "pick"	= pick xs
	| x == "throw"	= throw xs
	| otherwise = "You try to " ++ unwords (x:xs) ++ ", but you fail, because you're terrible at life."

------------------------------
-- Specific actions
------------------------------

-------------
-- LOOKING --
-------------

look :: Items -> [String] -> String
look _ [] = "You look. Not at anything, but whatever, who's judging?"
look inventory (x:xs)
	| x == "at" 	= lookat inventory xs -- "You look at " ++ unwords xs ++ "."
	| x == "up" 	= "You look up."
	| x == "down" 	= "You look down. You see your feet. They are stuck."
	| otherwise 	= "You look. Not at anything, but whatever, who's judging?"

-- Look at something in particular
lookat :: Items -> [String] -> String
lookat _ [] = "You look at... nothing? You see the nothingness swirl before you, before it swallows you whole. You're fucked now."
lookat inventory xs
	| object `elem` inventory = "You look at " ++ object ++ ". " ++ (properties object)
	| otherwise = "You can't look at " ++ object ++ " because you don't have it on you, and it's too misty to see anything more that a foot in front of you."
	where 
	object = unwords xs

-- The properties of objects: This will probably be stored in an external database or something.
-- That might be tricky with reading etc.
properties :: String -> String
properties "" = "It holds within it nothing. It is nothing. You see the nothingness swirl before you, before it swallows you whole. You're fucked now."
properties "test" = "It is a test to see whether this works."
properties _ = "It is supremely uninteresting."


-----------
-- USING --
-----------
	
useObject :: Items -> [String] -> String
useObject [] xs = "You do not have that."	
useObject inventory [] = "Use what? Twat."	
useObject inventory xs
	| item `elem` inventory	= "You use " ++ item ++ (targetPhrase targetInput)
	| otherwise = "You don't have that."	
	where 
	item = unwords (takeWhile (/= "on") xs)
	targetInput = dropWhile (/= "on") xs
	targetPhrase [] = " on nothing in particular." 	-- Here we need to diff into self usable
	targetPhrase [x] = " on what? Nonce!" 
	targetPhrase (x:xs) = " on " ++ unwords xs 		-- Here we must change the state of the target

-------------
-- GETTING --
-------------
	
get :: [String] -> String
get (x:xs) = "You cannot reach that."
--	| x == "at" 	= "You look at " ++ unwords xs ++ "."
--	| x == "up" 	= "You look up."
--	| x == "down" 	= "You look down. You see your feet. They are stuck."
--	| otherwise 	= "You look. Not at anything, but whatever, who's judging?"

pick :: [String] -> String
pick [] = "You pick your nose."
pick (x:xs) 
	| x == "up" 	= "You pick up " ++ unwords xs ++ "."
	| otherwise 	= "You pick your nose instead."

-------------------
-- OTHER ACTIONS --
-------------------

throw :: [String] -> String
throw (x:xs) = "You do not have that."


------------------------------
-- Helper functions
------------------------------

splitPhrase :: Eq a => a -> [a] -> ([a], [a])
splitPhrase x xs = (takeWhile (/= x) xs, tail (dropWhile (/= x) xs))

-----------------------------------------------------------------
-- MISCELLANEOUS FUNCTIONS
-----------------------------------------------------------------

echo :: String -> String
echo xs = xs

respondPassword = unlines . map (\xs -> if isPassword xs then "access granted" else "access denied") . lines  
    where   isPassword xs = xs == "test"  

respondPalindromes = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") . lines  
    where   isPalindrome xs = xs == reverse xs  

	
-----------------------------------------------------------------
-- Graveyard
-----------------------------------------------------------------


--use_on inventory xs = "something " ++ what xs
--	what [] = 
--
--	| targetPhrase == [] 	= "This shouldn't happen"
--	| target == ["on"] 		= "This shouldn't happen"
--	| item == "" 			= "Use " ++ item ++ " on what? Twat."
--	item `elem` inventory 	
--		| target == "" 	= "You use " ++ item "."
--		| otherwise 	= "You use " ++ item " on " ++ target ++ "."
--	| otherwise					= "You do not have that."
--		where 	item = unwords (takeWhile (/= "on") xs)
--				targetPhrase = dropWhile (/= "on") xs

--use :: Items -> [String] -> String
--use [] xs = "You do not have that."
--use inventory [] = "Use what? Twat."
--use inventory xs
--	| "on" `elem` xs 	= use_on inventory xs
--	| otherwise 		= just_use inventory xs

--just_use :: Items -> [String] -> String
--just_use [] xs = "You do not have that. How did that even happen?"	-- These should not occur
--just_use inventory [] = "Use what? Twat. How did that even happen?"	-- These should not occur
--just_use inventory xs
--	| item `elem` inventory = "You use " ++ item 	
--	| otherwise = "You don't have that."	
--	where item = unwords xs
	
--verb :: String -> String
--verb = fst . span (/=' ')

--preposition :: String -> String
--preposition = fst . span (/=' ') . tail . snd . span (/=' ')


--observation :: String -> String
--observation xs =
--	| (verb xs) == "look"		= "You looked"
--	| (preposition xs) == "up" 	= "Upwards is an odd nonsense"
--	| otherwise 				= "You failed to see Malrath Gakish Tor, the defender of the wicked. He cuts your head clean off."
