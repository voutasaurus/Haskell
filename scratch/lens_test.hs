{-# LANGUAGE TemplateHaskell #-}

module LensTest (testUp, testRep, initObj, execStateT, (<==)) where

import Control.Lens
import System.IO   
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

import LensAssign

data Object = Object { _objectID :: Int 
				 , _objectName :: String
				 , _objectDescription :: String
				 } deriving (Show)
	
$( makeLenses ''Object )

initObj :: Object
initObj = Object
	{ _objectID = 2
	, _objectName = "Object Test Name"
	, _objectDescription = "Object Test Description"
	}

-- Test statement 1, replace object name with object description
-- 	execStateT (objectName <== (initObj^.objectDescription)) initObj

-- Test statement 2, add things to objectID, then replace objectName
-- 		and add prefix and suffix to objectName
-- 	execStateT (testUp 2) initObj
	
-- redundant - equivalent to: objectName <== testString
testRep :: String -> StateT Object IO ()
testRep testString = do
	objectName <== testString
	
testUp :: Int -> StateT Object IO ()
testUp 0 = do 
	lift $ putStrLn "done"
	objectName <== "replaced middle successfully"
--	objectName %= (replaceWith "replaced middle successfully") 
	objectName ++= ": Suffix test 1" 
	objectName =++ "Prefix test 1: " 
testUp n = do
	lift $ putStrLn "Start"	
	up <- lift $ getLine
	lift $ putStrLn up
	objectID += (read up)
	lift $ putStrLn "once more"
	upmore <- lift $ getLine
	lift $ putStrLn upmore
	objectID += (read upmore)
	lift $ putStrLn "done"
--	lift $ putStrLn (show (objectID))
	testUp (n-1)


---------------
-- GRAVEYARD --
---------------

-- main = execStateT testUp initObj


-- (++=) ::  Control.Monad.State.Class.MonadState s m => 
-- Setting (->) s s [a] [a] -> [a] -> m ()
-- stringLens ++= suffix = stringLens %= (++ suffix)

-- (=++) ::  Control.Monad.State.Class.MonadState s m => 
-- Setting (->) s s [a] [a] -> [a] -> m ()
-- stringLens =++ prefix = stringLens %= (prefix ++)

-- replaceWith :: a -> a -> a
-- replaceWith x y = x

-- (<==) :: Control.Monad.State.Class.MonadState s m =>
-- Setting (->) s s b b -> b -> m ()
-- xLens <== x = xLens %= (replaceWith x)
