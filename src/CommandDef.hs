module CommandDef
( CommandStateMask
, anyStateCommand
, notAuthenticatedStateCommand
, authenticatedStateCommand
, selectedStateCommand
, CommandDef
, commandName
, process
, isCommandAvaible
) where

import ConnectionState

import Data.Bits
import Data.Int (Int8)

import qualified Data.ByteString as B

newtype CommandStateMask = CommandStateMask { unwrap :: Int8 }


-- AnyState < *
-- NotAuthenticated
-- Authenticated < Selected

stateMask :: ConnectionState -> Int8
stateMask NotAuthenticated = 1 -- 001
stateMask Authenticated    = 2 -- 010
stateMask Selected         = 6 -- 110      When in Selected state we are also in Authenticated state

anyStateCommand = CommandStateMask maxBound       -- 111
notAuthenticatedStateCommand = CommandStateMask 1 -- 001
authenticatedStateCommand    = CommandStateMask 2 -- 010
selectedStateCommand         = CommandStateMask 4 -- 100



isCommandAvaible' :: ConnectionState -> CommandStateMask -> Bool
isCommandAvaible' state commandMask = (stateMask state .&. unwrap commandMask) /= 0


-- TODO: Add a meaningful process type
data CommandDef = CommandDef
             { commandStateMask :: CommandStateMask
             , commandName      :: B.ByteString
             , process          :: IO ()
             }

isCommandAvaible :: ConnectionState -> CommandDef -> Bool
isCommandAvaible state cmd = isCommandAvaible' state (commandStateMask cmd)

makeCommand :: CommandStateMask -> B.ByteString -> IO () -> CommandDef
makeCommand = CommandDef

makeAnyStateCommand              = makeCommand anyStateCommand
makeNotAuthenticatedStateCommand = makeCommand notAuthenticatedStateCommand
makeAuthenticatedStateCommand    = makeCommand authenticatedStateCommand
makeSelectedStateCommand         = makeCommand selectedStateCommand
