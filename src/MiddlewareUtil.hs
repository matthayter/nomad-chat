{-# LANGUAGE OverloadedStrings #-}

-- Various utility functions for integrating WAI, Websockets and Scotty
module MiddlewareUtil where

import qualified Web.Scotty as Scotty;
    import       Web.Scotty (scotty, param, get, html, middleware, setHeader, file)

import qualified Network.WebSockets as WS
import qualified Network.Wai.Handler.WebSockets as WSWai
import qualified Network.Wai as WAI;
    import       Network.Wai (Application, Middleware)
import Control.Monad

requires :: (a -> Middleware) -> (WAI.Request -> Maybe a) -> Middleware
requires dependentMiddleware condition nextApp req res =
    let
        resultMware = maybeMiddleware $ liftM dependentMiddleware (condition req)
    in
        resultMware nextApp req res

requireWithIO :: (WAI.Request -> IO (Maybe a)) -> (a -> Middleware) -> Middleware
requireWithIO condition dependentMiddleware nextApp req res =
    let
        resultMware = \cond -> maybeMiddleware $ liftM dependentMiddleware cond
    in do
        mCondition <- condition req
        (resultMware mCondition) nextApp req res

require2 :: (WAI.Request -> Maybe a) -> (WAI.Request -> Maybe b) -> (a -> b -> Middleware) -> Middleware
require2 condition1 condition2 dependentMiddleware nextApp req res =
    let
        resultMware = maybeMiddleware $ liftM2 dependentMiddleware (condition1 req) (condition2 req)
    in
        resultMware nextApp req res

httpget :: Scotty.RoutePattern -> Scotty.ActionM () -> Scotty.ScottyM ()
httpget route action =
    -- Ignore ws requests. Ideally, scotty should allow specification of 'http-only-get'
    get route $ do
        req <- Scotty.request
        when (WSWai.isWebSocketsReq req) (void Scotty.next)
        action

maybeMiddleware :: Maybe Middleware -> Middleware
maybeMiddleware (Just m) app = m app
maybeMiddleware Nothing app = app

wsMiddleware :: Middleware -> Scotty.ScottyM ()
wsMiddleware = middleware . (WAI.ifRequest WSWai.isWebSocketsReq)

logHostMiddleware :: Application -> Application
logHostMiddleware nextApp req res = do
    putStrLn $ "New connection from host: " ++ (show $ WAI.remoteHost req)
    nextApp req res

