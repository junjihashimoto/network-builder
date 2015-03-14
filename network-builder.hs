{-#LANGUAGE OverloadedStrings#-}
{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}
{-#LANGUAGE ExtendedDefaultRules #-}
{-#LANGUAGE RecordWildCards #-}
{-#LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
import Prelude hiding (FilePath)
import Shelly hiding (command)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Monoid
import Data.Maybe
import Control.Monad
import Control.Exception
import Options.Applicative
import Data.Char
import qualified Data.Yaml as Y
import qualified Data.Aeson.TH as A
import qualified Data.ByteString.Char8 as B

default (T.Text)

data Bridge = Bridge {
    bname :: T.Text
  , bip :: T.Text
  }
  deriving (Show,Eq)
$(A.deriveJSON A.defaultOptions{A.fieldLabelModifier = drop 1,A.constructorTagModifier = map toLower} ''Bridge)

data Veth = Veth {
    vname :: T.Text
  , vip :: T.Text
  }
  deriving (Show,Eq)
$(A.deriveJSON A.defaultOptions{A.fieldLabelModifier = drop 1, A.constructorTagModifier = map toLower} ''Veth)


data NetworkNameSpace =
  NetworkNameSpace {
    nname :: T.Text
  , nnss :: Maybe [(Bridge,[(Veth,NetworkNameSpace)])]
  }
  deriving (Show,Eq)
$(A.deriveJSON A.defaultOptions{A.fieldLabelModifier = drop 1, A.constructorTagModifier = map toLower} ''NetworkNameSpace)


data HostServer =
  HostServer {
    hnss :: Maybe [(Bridge,[(Veth,NetworkNameSpace)])]
  }
  deriving (Show,Eq)
$(A.deriveJSON A.defaultOptions{A.fieldLabelModifier = drop 1, A.constructorTagModifier = map toLower} ''HostServer)

data Tunnel =
  GreTunnel {
    gName :: T.Text
  , gRemoteIp :: T.Text
  , gLocalIp :: T.Text
  , gRemoteNetwork :: T.Text
  , gGreDeviceIp :: T.Text
  }
  deriving (Show,Eq)
$(A.deriveJSON A.defaultOptions{A.fieldLabelModifier = drop 1, A.constructorTagModifier = map toLower} ''Tunnel)


class VirtualServer a where
  name :: a -> T.Text 
  nss :: a -> [(Bridge,[(Veth,NetworkNameSpace)])]
  isHost :: a -> Bool
  runNs :: a -> T.Text -> [T.Text] -> Sh T.Text

instance VirtualServer HostServer where
  name _ = "localhost"
  nss host = maybe [] id $ hnss host
  isHost _ = True
  runNs (HostServer _) cmd' args = do
    run "sudo" $ [cmd'] ++ args

instance VirtualServer NetworkNameSpace where
  name = nname
  nss ns = maybe [] id $ nnss ns
  isHost _ = False
  runNs (NetworkNameSpace name' _) cmd' args = do
    run "sudo" $ ["ip", "netns", "exec", name', cmd'] ++ args


delMask :: T.Text -> T.Text
delMask = T.takeWhile (/= '/') 

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
  
-- sampleNS :: HostServer
-- sampleNS =
--   HostServer{
--     hnss = [
--     (Bridge {
--        bname = "br1"
--      , bip = "192.168.10.1/24"
--      } ,
--      [
--        (Veth {
--           vname = "veth-2"
--         , vip = "192.168.10.2/24"
--         } ,
--         NetworkNameSpace {
--           nname = "server2"
--         , nnss = [
--           (Bridge {
--              bname = "br1"
--            , bip = "192.168.11.1/24"
--            } ,
--            [
--              (Veth {
--                 vname = "veth-3"
--               , vip = "192.168.11.4/24"
--               } ,
--               NetworkNameSpace {
--                 nname = "server4"
--               , nnss = []
--               })
--            ])
--           ]
--         }),
--        (Veth {
--           vname = "veth-3"
--         , vip = "192.168.10.3/24"
--         } ,
--         NetworkNameSpace {
--           nname = "server3"
--         , nnss = []
--         })
--      ])
--     ]
--   }

createNetworkNameSpaces :: HostServer -> Sh ()
createNetworkNameSpaces host = createNetworkNameSpaces' host host

createNetworkNameSpaces' :: VirtualServer a => HostServer -> a -> Sh ()
createNetworkNameSpaces' host ns = do
  forM_ (nss ns) $ \(br,vs) -> do
    -- create bridge
    _<- runNs ns "ip" ["link", "add", "name", bname br, "type", "bridge"]
    _<- runNs ns "ip" ["addr", "add" , bip br, "dev", bname br]
    _<- runNs ns "ip" ["link", "set", "dev", bname br, "up"]
    
    -- setup ip masquerade
    _<- runNs ns "iptables" ["-t","nat", "-A", "POSTROUTING", "-s", bip br, "!", "-o", bname br, "-j", "MASQUERADE"]
    
    forM_ vs $ \(veth,nss') -> do
      -- create namespace 
      _<- runNs host "ip" ["netns", "add", nname nss']
      
      -- create veth pair
      _<- runNs host "ip" ["link", "add", vname veth <> "-0", "type", "veth", "peer", "name", vname veth <> "-1"]
      
      -- assign veth pair for namespace
      when (isHost ns == False) $ do
        void $ runNs host "ip" ["link", "set", vname veth <> "-0", "netns", name ns]
      void $ runNs host "ip" ["link", "set", vname veth <> "-1", "netns", name nss']
      
      -- up veth of upper side sserver
      _<- runNs ns "ip" ["link", "set", "dev", vname veth <> "-0", "up"]
      
      -- connect veth to bridge
      _<- runNs ns "ip" ["link", "set", "dev", vname veth <> "-0", "master", bname br]
      
      -- setup lo
      _<- runNs nss' "ip" ["addr", "add" , "127.0.0.1", "dev", "lo"]
      _<- runNs nss' "ip" ["link", "set" , "dev", "lo", "up"]
      
      -- up veth of lower side server
      _<- runNs nss' "ip" ["addr", "add" , vip veth, "dev", vname veth <> "-1"]
      _<- runNs nss' "ip" ["link", "set" , "dev", vname veth <> "-1", "up"]

      -- setup gateway
      _<- runNs nss' "ip" ["route", "add", "default", "via",delMask (bip br), "dev", vname veth <> "-1"]

      -- create lower network
      _<- createNetworkNameSpaces' host nss'
      return ()


deleteNetworkNameSpaces :: HostServer -> Sh ()
deleteNetworkNameSpaces host = deleteNetworkNameSpaces' host host

ignore act = (void $ act) `catch_sh` \(err :: SomeException) -> liftIO $ putStrLn $ show err

deleteNetworkNameSpaces' :: VirtualServer a => HostServer -> a -> Sh ()
deleteNetworkNameSpaces' host ns = do
  forM_ (nss ns) $ \(br,vs) -> do
    forM_ vs $ \(veth,nss') -> do
      _<- deleteNetworkNameSpaces' host nss'
      ignore $ runNs ns "ip" ["link", "set", "dev", vname veth <> "-0", "down"]
      ignore $ runNs ns "ip" ["link", "set", "dev", vname veth <> "-0", "nomaster"]
      ignore $ runNs host "ip" ["netns", "del", nname nss']
      return ()
    ignore $ runNs ns "iptables" ["-t","nat", "-D", "POSTROUTING", "-s", bip br, "!", "-o", bname br, "-j", "MASQUERADE"]
    ignore $ runNs ns "ip" ["link", "delete", bname br, "type", "bridge"]
    return ()


createTunnel ::  VirtualServer a => a -> Tunnel -> Sh ()
createTunnel ns GreTunnel{..} = do
  runNs ns "ip" ["tunnel", "add", gName, "mode", "gre", "remote", gRemoteIp, "local", gLocalIp]
  runNs ns "ip" ["link", "set", gName, "up"]
  runNs ns "ip" ["addr", "add", gGreDeviceIp, "dev", gName]
  runNs ns "ip" ["route", "add", gRemoteNetwork, "dev", gName]
  return ()

deleteTunnel ::  VirtualServer a => a -> Tunnel -> Sh ()
deleteTunnel ns GreTunnel{..} = do
  ignore $ runNs ns "ip" ["route", "del", gRemoteNetwork, "dev", gName]
  ignore $ runNs ns "ip" ["addr", "del", gGreDeviceIp, "dev", gName]
  ignore $ runNs ns "ip" ["link", "set", gName, "down"]
  ignore $ runNs ns "ip" ["tunnel", "del", gName]
  return ()

data Command
  = Create String
  | Destroy String
  | Show String
  | CreateTunnel String
  | DestroyTunnel String
  | ShowTunnel String

create :: Parser Command
create = Create
         <$> option auto (long "conf" <> value "network-builder.yml" <> metavar "CONFILE")

destroy :: Parser Command
destroy = Destroy
          <$> option auto (long "conf" <> value "network-builder.yml" <> metavar "CONFILE")

createTun :: Parser Command
createTun = CreateTunnel
         <$> argument str (metavar "TUNNELFILE")

destroyTun :: Parser Command
destroyTun = DestroyTunnel
         <$> argument str (metavar "TUNNELFILE")

showTun :: Parser Command
showTun = ShowTunnel
         <$> argument str (metavar "TUNNELFILE")

showYaml :: Parser Command
showYaml = Show
           <$> option auto (long "conf" <> value "network-builder.yml" <> metavar "CONFILE")

parse :: Parser Command
parse = subparser $ 
          command "create"  (info create (progDesc "create linux-network-namespaces")) <>
          command "destroy" (info destroy (progDesc "destroy linux-network-namespaces")) <>
          command "show"  (info showYaml (progDesc "show decoded yaml-conf")) <>
          command "create-tunnel"  (info createTun (progDesc "create tunnel")) <>
          command "destroy-tunnel" (info destroyTun (progDesc "destroy tunnel")) <>
          command "show-tunnel" (info showTun (progDesc "show tunnel"))


runCmd :: Command -> IO ()
runCmd (Create file) = shelly $ do
  eyaml <- liftIO $ Y.decodeFileEither file
  case eyaml of
    Right conf -> createNetworkNameSpaces conf
    Left err -> echo $ T.pack $ show err
    
runCmd (Destroy file) = shelly $ do
  eyaml <- liftIO $ Y.decodeFileEither file
  case eyaml of
    Right conf -> deleteNetworkNameSpaces conf
    Left err -> echo $ T.pack $ show err

runCmd (CreateTunnel file) = shelly $ do
  eyaml <- liftIO $ Y.decodeFileEither file :: Sh (Either Y.ParseException (NetworkNameSpace,Tunnel))
  case eyaml of
    Right (netconf,tunnelconf) -> createTunnel netconf tunnelconf
    Left err -> do
      eyaml <- liftIO $ Y.decodeFileEither file :: Sh (Either Y.ParseException (HostServer,Tunnel))
      case eyaml of
        Right (netconf,tunnelconf) -> createTunnel netconf tunnelconf
        Left err -> echo $ T.pack $ show err
    
runCmd (DestroyTunnel file) = shelly $ do
  eyaml <- liftIO $ Y.decodeFileEither file :: Sh (Either Y.ParseException (NetworkNameSpace,Tunnel))
  case eyaml of
    Right (netconf,tunnelconf) -> deleteTunnel netconf tunnelconf
    Left err -> do
      eyaml <- liftIO $ Y.decodeFileEither file :: Sh (Either Y.ParseException (HostServer,Tunnel))
      case eyaml of
        Right (netconf,tunnelconf) -> deleteTunnel netconf tunnelconf
        Left err -> echo $ T.pack $ show err

runCmd (ShowTunnel file) = shelly $ do
  eyaml <- liftIO $ Y.decodeFileEither file :: Sh (Either Y.ParseException (NetworkNameSpace,Tunnel))
  case eyaml of
    Right a@(netconf,tunnelconf) -> echo $ T.decodeUtf8 $ Y.encode a
    Left err -> do
      eyaml <- liftIO $ Y.decodeFileEither file :: Sh (Either Y.ParseException (HostServer,Tunnel))
      case eyaml of
        Right a@(netconf,tunnelconf) -> echo $ T.decodeUtf8 $ Y.encode a
        Left err -> echo $ T.pack $ show err

runCmd (Show file) = shelly $ do
  eyaml <- liftIO $ (Y.decodeFileEither file :: IO (Either Y.ParseException HostServer))
  case eyaml of
    Right conf -> echo $ T.pack $ show conf
    Left err -> echo $ T.pack $ show err

opts :: ParserInfo Command
opts = info (parse <**> helper) idm

main :: IO ()
main = execParser opts >>= runCmd

