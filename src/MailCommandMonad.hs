{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module MailCommandMonad where
    
import qualified Data.ByteString as B

import Control.Monad.Reader
    
data CommandContext = CommandContext
                    { commandTag   :: B.ByteString
                    , unparsedArgs :: B.ByteString
                    }

newtype MailCommand a = MailCommand { unwrap :: ReaderT CommandContext IO a }
                      deriving(Functor, Applicative, Monad)
