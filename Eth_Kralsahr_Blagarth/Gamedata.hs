{-# LANGUAGE TemplateHaskell #-}

module Gamedata where

import Control.Lens
import System.IO   
import Control.Monad.Trans.Class
import Control.Monad.Trans.State


-------------------------
-- Objects in the game --
-------------------------

-- Object actions
--data Action = Action { _actionType :: 
--					 , _selfEffect ::
--					 , _onEffect ::
--					 }
					 
-- Object/Item/Interactable
data Object = Object { _objectName :: String
				 , _objectDescription :: String
--				 , _objectAffordances :: [Action]
--				 , _objectSecretActs :: [Action]
				 } deriving (Show) 

data Door = Door { _objectName :: String
				 , _objectDescription :: String
				 , _locked :: Bool
--				 , _objectAffordances :: [Action]
--				 , _objectSecretActs :: [Action]
				 } deriving (Show) 
				 
----------------------------------------
-- Location Navigation By List Zipper --
----------------------------------------

type ListZipper a = ([a],[a])  

goForward :: ListZipper a -> Maybe (ListZipper a)
goForward (x:xs, bs) = Just (xs, x:bs)  
goForward ([], _) = Nothing
  
goBack :: ListZipper a -> Maybe (ListZipper a)
goBack (xs, b:bs) = Just (b:xs, bs)  
goBack (_, []) = Nothing

----------------
-- Game State --
----------------

-- Location state
data Location = Location 	{ _placeName :: String
							, _placeDescription :: String
							, _visibles :: [Object]
							, _invisibles :: [Object]
							} deriving (Show) 

-- Player State
data Player = Player 	{ _playerName :: String
					, _inventory :: [Object]
					, _location :: ListZipper Location
					} deriving (Show) 

------------
-- Lenses --
------------

-- Lenses for the objects
-- $( makeLenses ''Action )
$( makeLenses ''Object )
$( makeLenses ''Door )

-- Lenses for the game state
$( makeLenses ''Location )
$( makeLenses ''Player )

---------------------
----- GAME MAP ------
-- Initial states for the locations
---------------------

-- Starting point (outside Eth Kralsahr Blagarth)
-- The Door of Kral

-- Objects at this location
initObjDoorOfKral :: Door
initObjDoorOfKral = Door
	{ _objectName = "the Door of Kral"
	, _objectDescription = "This door is large and very old"
	, _locked = True
--				 , _objectAffordances :: [Action]
--				 , _objectSecretActs :: [Action]
	}

initObjLeftTorch :: Object
initObjLeftTorch = Object
	{ _objectName = "a torch to the left of the door"
	, _objectDescription = "This torch shines as bright as the day it was lit."
--				 , _objectAffordances :: [Action]
--				 , _objectSecretActs :: [Action]
	}

initObjRightTorch :: Object
initObjRightTorch = Object
	{ _objectName = "a torch to the right of the door"
	, _objectDescription = "This torch shines as bright as the day it was lit. It seems to be off centre though."
--				 , _objectAffordances :: [Action]
--				 , _objectSecretActs :: [Action]
	}

-- Location	
initLocDoorOfKral :: Location
initLocDoorOfKral = Location
	{ _placeName = "The Door of Kral"
	, _placeDescription = "You stand at the great Door of Kral: the door to Eth Kralsahr Blagarth."
	, _visibles = [initObjDoorOfKral, initObjLeftTorch, initObjRightTorch]
	, _invisibles = []
	}

-- Just inside the Door of Kral
-- Antechamber

-- Objects at this location
initObjStarGlobe :: Object
initObjStarGlobe = Object
	{ _objectName = "the star globe"
	, _objectDescription = "A grand globe floats high in the antechamber, radiating like the sun."
--				 , _objectAffordances :: [Action]
--				 , _objectSecretActs :: [Action]
	}

initObjSkeleton :: Object
initObjSkeleton = Object
	{ _objectName = "a skeleton"
	, _objectDescription = "A skeleton lays on the ground before you."
--				 , _objectAffordances :: [Action]
--				 , _objectSecretActs :: [Action]
	}

	
-- Location	
initLocAntechamber :: Location
initLocAntechamber = Location
	{ _placeName = "The Antechamber of Eth Kralsahr Blagarth"
	, _placeDescription = "You stand in the grand antechamber of Eth Kralsahr Blagarth."
	, _visibles = [initObjDoorOfKral, initObjStarGlobe, initObjSkeleton]
	, _invisibles = []
	}

---------------------
------ Player  -------
-- Initial state for the Player
---------------------

-- items in the inventory
initTestTube1 :: Object
initTestTube1 = Object
	{ _objectName = "test tube 1"
	, _objectDescription = "It's just a boring test tube."
--				 , _objectAffordances :: [Action]
--				 , _objectSecretActs :: [Action]
	}

initTestTube2 :: Object
initTestTube2 = Object
	{ _objectName = "test tube 2"
	, _objectDescription = "It's another boring test tube."
--				 , _objectAffordances :: [Action]
--				 , _objectSecretActs :: [Action]
	}

	
-- Initial game state
initialState :: Player
initialState = Player
	{ _playerName = "Grogdan the immolator"
	, _inventory = [initTestTube1, initTestTube2]
	, _location = ([initLocDoorOfKral, initLocAntechamber], [])
	}