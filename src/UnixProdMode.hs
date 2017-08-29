{-# LANGUAGE CPP #-}

module UnixProdMode where

import           System.Exit (die)
import qualified Web.Scotty as Scotty;


import qualified Data.Default.Class as Default
import qualified Web.Scotty as Scotty
import qualified System.Posix.User as User
import           Network.Wai.Handler.Warp

prodModeScotty :: IO ( Scotty.ScottyM () -> IO () )

#ifdef OS_Windows
prodModeScotty = die "Production Mode not available on Windows"
#else

prodModeScotty = return $ Scotty.scottyOpts opts 
    where
        defaultWarpSettings = Scotty.settings Default.def 
        setUser = do
            User.groupID `fmap` User.getGroupEntryForName "nomad-chat" >>= User.setGroupID
            User.userID `fmap` User.getUserEntryForName "nomad-chat" >>= User.setUserID
        prodSettings = (setFdCacheDuration 10) . (setPort 80) . (setBeforeMainLoop setUser) $ defaultWarpSettings
        opts = Default.def {Scotty.settings = prodSettings}

#endif
