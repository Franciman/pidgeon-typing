module CommandRegistry
( CommandRegistry
, makeRegistry
, lookupCommand
) where

import qualified Data.Map.Strict as M

import qualified Data.ByteString as B

import CommandDef

newtype CommandRegistry = CommandRegistry { unwrap :: M.Map B.ByteString CommandDef }

makeRegistry :: [CommandDef] -> CommandRegistry
makeRegistry = CommandRegistry . M.fromList . map (\cmd -> (commandName cmd, cmd))


lookupCommand :: CommandRegistry -> B.ByteString -> Maybe CommandDef
lookupCommand (CommandRegistry reg) name = M.lookup name reg
