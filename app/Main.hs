{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Web.Scotty (scotty, param, get, html, middleware)
import Network.Wai.Middleware.Static (staticPolicy, hasPrefix)
import qualified Network.WebSockets as WS
import qualified Network.Wai.Handler.WebSockets as WSWai
import Data.Monoid (mconcat)
import qualified Data.Text as T
import qualified Data.Text.IO as T.IO

main :: IO ()
main = scotty 3000 $ do
    middleware $ staticPolicy $ hasPrefix "public/"
    middleware $ WSWai.websocketsOr WS.defaultConnectionOptions handleWs

handleWs :: WS.PendingConnection -> IO ()
handleWs p = do
    conn <- WS.acceptRequest p
    WS.sendTextData conn $ T.pack "PingPong!"
    msg <- recieveText conn
    T.IO.putStrLn msg
    WS.sendClose conn $ T.pack "Bye!"
    return ()
    where
        recieveText :: WS.Connection -> IO T.Text
        recieveText = WS.receiveData
