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
import Options.Applicative
import qualified Data.Yaml as Y
import Network.Builder

default (T.Text)

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
    Left _err -> do
      eyaml' <- liftIO $ Y.decodeFileEither file :: Sh (Either Y.ParseException (HostServer,Tunnel))
      case eyaml' of
        Right (netconf,tunnelconf) -> createTunnel netconf tunnelconf
        Left err -> echo $ T.pack $ show err
    
runCmd (DestroyTunnel file) = shelly $ do
  eyaml <- liftIO $ Y.decodeFileEither file :: Sh (Either Y.ParseException (NetworkNameSpace,Tunnel))
  case eyaml of
    Right (netconf,tunnelconf) -> deleteTunnel netconf tunnelconf
    Left _err -> do
      eyaml' <- liftIO $ Y.decodeFileEither file :: Sh (Either Y.ParseException (HostServer,Tunnel))
      case eyaml' of
        Right (netconf,tunnelconf) -> deleteTunnel netconf tunnelconf
        Left err -> echo $ T.pack $ show err

runCmd (ShowTunnel file) = shelly $ do
  eyaml <- liftIO $ Y.decodeFileEither file :: Sh (Either Y.ParseException (NetworkNameSpace,Tunnel))
  case eyaml of
    Right a -> echo $ T.decodeUtf8 $ Y.encode a
    Left _err -> do
      eyaml' <- liftIO $ Y.decodeFileEither file :: Sh (Either Y.ParseException (HostServer,Tunnel))
      case eyaml' of
        Right a -> echo $ T.decodeUtf8 $ Y.encode a
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

