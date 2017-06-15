{-# LANGUAGE CPP #-}

module UnixProdMode where

#ifdef OS_Windows
import           System.Exit (die)
prodModeScotty = die "Production Mode not available on Windows"
#else

import qualified Scotty as Scotty
import qualified System.Posix.User as User

prodModeScotty :: ScottyM () -> IO ()
prodModeScotty = Scotty.scottyOpts opts 
    where
        defaultWarpSettings = Scotty.settings Default.def 
        setUser = do
            User.userId `fmap` User.getUserEntryForName "nomad-chat" >>= User.setUserId
            User.groupId `fmap` User.getGroupEntryForName "nomad-chat" >>= User.setGroupId
        prodSettings = (setFdCacheDuration 10) . (setPort 80) . (setBeforeMainLoop setUser) $ defaultWarpSettings
        opts = Default.def {settings = prodSettings}

#endif