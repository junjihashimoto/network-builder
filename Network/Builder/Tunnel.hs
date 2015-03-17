{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}
{-#LANGUAGE ExtendedDefaultRules #-}
{-#LANGUAGE RecordWildCards #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Network.Builder.Tunnel where

import Prelude hiding (FilePath)
import Shelly hiding (command)
import qualified Data.Text as T

import Control.Monad
import Control.Exception
import Data.Char
import qualified Data.Aeson.TH as A
import Network.Builder.Namespace

default (T.Text)

data Tunnel =
  GreTunnel {
    gName :: T.Text
  , gRemoteIp :: T.Text
  , gLocalIp :: T.Text
  , gRemoteNetwork :: T.Text
  , gGreDeviceIp :: T.Text
  } | 
  GreTapTunnel {
    gName :: T.Text
  , gRemoteIp :: T.Text
  , gLocalIp :: T.Text
  , gGreTapBridge :: T.Text
  }
  deriving (Show,Eq)
$(A.deriveJSON A.defaultOptions{A.fieldLabelModifier = drop 1, A.constructorTagModifier = map toLower} ''Tunnel)

-- sampleTunnel :: (NetworkNameSpace,Tunnel)
-- sampleTunnel =(
--   NetworkNameSpace {
--     nname = "server2"
--   , nnss = []
--   },
--   GreTunnel {
--     gName = "gre2"
--   , gRemoteIp = "192.168.10.3/24"
--   , gLocalIp = "192.168.10.2/24"
--   , gRemoteNetwork ="192.168.12.0/24"
--   , gGreDeviceIp ="192.168.11.254/24"
--   })

createTunnel ::  VirtualServer a => a -> Tunnel -> Sh ()
createTunnel ns GreTunnel{..} = do
  void $ runNs ns "ip" ["tunnel", "add", gName, "mode", "gre", "remote", gRemoteIp, "local", gLocalIp]
  void $ runNs ns "ip" ["link", "set", gName, "up"]
  void $ runNs ns "ip" ["link", "set", gName, "mtu", "1462"]
  void $ runNs ns "ip" ["addr", "add", gGreDeviceIp, "dev", gName]
  void $ runNs ns "ip" ["route", "add", gRemoteNetwork, "dev", gName]

createTunnel ns GreTapTunnel{..} = do
  void $ runNs ns "ip" ["link", "add", gName, "type", "gretap", "remote", gRemoteIp, "local", gLocalIp]
  void $ runNs ns "ip" ["link", "set", gName, "up"]
  void $ runNs ns "ip" ["link", "set", gName, "mtu", "1462"]
  void $ runNs ns "ip" ["link", "set", gName, "master", gGreTapBridge]

deleteTunnel ::  VirtualServer a => a -> Tunnel -> Sh ()
deleteTunnel ns GreTunnel{..} = do
  ignore $ runNs ns "ip" ["route", "del", gRemoteNetwork, "dev", gName]
  ignore $ runNs ns "ip" ["addr", "del", gGreDeviceIp, "dev", gName]
  ignore $ runNs ns "ip" ["link", "set", gName, "down"]
  ignore $ runNs ns "ip" ["tunnel", "del", gName]
  where
    ignore act = (void $ act) `catch_sh` \(err :: SomeException) -> liftIO $ putStrLn $ show err    

deleteTunnel ns GreTapTunnel{..} = do
  ignore $ runNs ns "ip" ["link", "set", gName, "down"]
  ignore $ runNs ns "ip" ["link", "del", gName]
  where
    ignore act = (void $ act) `catch_sh` \(err :: SomeException) -> liftIO $ putStrLn $ show err    

