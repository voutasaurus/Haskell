{-# LANGUAGE TemplateHaskell #-}

module LensString 
(
-- * String operators
(=++),(++=)
) where

import Control.Lens
import System.IO   
import Control.Monad.Trans.Class
import Control.Monad.Trans.State

-- (++=) ::  Control.Monad.State.Class.MonadState s m => 
-- Setting (->) s s [a] [a] -> [a] -> m ()
stringLens ++= suffix = stringLens %= (++ suffix)

-- (=++) ::  Control.Monad.State.Class.MonadState s m => 
-- Setting (->) s s [a] [a] -> [a] -> m ()
stringLens =++ prefix = stringLens %= (prefix ++)

-- Assignment: THE OPERATOR (.=) already does this
-- (<==) :: Control.Monad.State.Class.MonadState s m =>
-- Setting (->) s s b b -> b -> m ()
-- xLens <== x = xLens %= ((\a b -> a) x)