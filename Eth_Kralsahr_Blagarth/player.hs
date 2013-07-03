module Main (main) where

import Gamedata
import LensString

import Data.List
import Control.Lens
import System.IO   
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

-- REMEMBER useful lens operations
-- "lens %= function" 	mutation by function
-- "lens .= value" 		assignment
	
-- Main function dummy
main = do   
	putStrLn "Success"

------------------
-- LOOK 		--
-- Descriptions --
------------------
	
-- Create a string listing the elements in a list
listElem :: [String] -> String -> String
listElem [] noCase  	=  noCase
listElem [w] noCase     = "and " ++ w ++ "."
listElem (w:ws) noCase  = w ++ ", " ++ listElem ws noCase

-- Describe the visible objects	at a location
visDesc :: Location -> String
visDesc location = "You see " ++ ( listElem objects nothingCase ) 
	where 
	objects = fmap (^.objectName) (location^.visibles)  -- [] -- void test -- 
	nothingCase = "nothing in particular. This place is empty."

-- Gives the player's current location
curLoc :: Player -> Location
curLoc player = head $ fst $ player^.location

-- Describes the player's current location
curLocDesc :: Player -> String
curLocDesc player = (curLoc player)^.placeDescription ++ "\n" ++ (visDesc (curLoc player))

-- Describes the afforances of an obect
-- TO DO: add affordance structure to Gamedata
-- TO DO: add affordance probes to this section
-- TO DO: add affordance modifiers to action/reaction
objAffordDesc :: Object -> String
objAffordDesc object = "It appears to be "  ++ ( listElem affordanceAdjectives nothingCase ) 
	where 	
	affordanceAdjectives = [] -- void test -- 
	nothingCase = "normal. Don't bother with it."

-- Describes an object, including its affordances
objDesc :: Object -> String
objDesc object = object^.objectDescription ++ "\n" ++ (objAffordDesc object)

-- Test expressions:
-- putStrLn $ objDesc $ initObjRightTorch 	-- Prints the description of the right torch
-- putStrLn $ curLocDesc $ initialState 	-- Prints the desciption of the initial location

--------------
-- END LOOK --
--------------


--move :: StateT Player IO ()
--move = do
--	lift $ putStrLn "test"

	
{- Commenting out the below FOR NOW 

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

	THIS CONCLUDES THE COMMENTS -}
	
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
