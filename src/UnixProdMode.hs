{-# LANGUAGE CPP #-}

module UnixProdMode where

#ifdef OS_Windows
import           System.Exit (die)
prodModeScotty = die "Production Mode not available on Windows"
#else

import qualified Data.Default.Class as Default
import qualified Web.Scotty as Scotty
import qualified System.Posix.User as User
import           Network.Wai.Handler.Warp

prodModeScotty :: Scotty.ScottyM () -> IO ()
prodModeScotty = Scotty.scottyOpts opts 
    where
        defaultWarpSettings = Scotty.settings Default.def 
        setUser = do
            User.groupID `fmap` User.getGroupEntryForName "nomad-chat" >>= User.setGroupID
            User.userID `fmap` User.getUserEntryForName "nomad-chat" >>= User.setUserID
        prodSettings = (setFdCacheDuration 10) . (setPort 80) . (setBeforeMainLoop setUser) $ defaultWarpSettings
        opts = Default.def {Scotty.settings = prodSettings}

#endif
