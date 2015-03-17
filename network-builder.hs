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
         <$> option str (long "conf" <> value "network-builder.yml" <> metavar "CONFILE")

destroy :: Parser Command
destroy = Destroy
          <$> option str (long "conf" <> value "network-builder.yml" <> metavar "CONFILE")

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

runCmd' :: String -> (HostServer -> Sh a) -> IO a
runCmd' file action' = shelly $ do
  echo $ T.pack file
  eyaml <- liftIO $ Y.decodeFileEither file
  case eyaml of
    Right conf -> action' conf
    Left err -> do
      echo $ T.pack $ show err
      exit 1

runCmd'' :: String
         -> (NetworkNameSpace -> Tunnel -> Sh a)
         -> (HostServer -> Tunnel -> Sh a)
         -> IO a
runCmd'' file act0 act1 = shelly $ do
  eyaml <- liftIO $ Y.decodeFileEither file :: Sh (Either Y.ParseException (NetworkNameSpace,Tunnel))
  case eyaml of
    Right (netconf,tunnelconf) -> act0 netconf tunnelconf
    Left _err -> do
      eyaml' <- liftIO $ Y.decodeFileEither file :: Sh (Either Y.ParseException (HostServer,Tunnel))
      case eyaml' of
        Right (netconf,tunnelconf) -> act1 netconf tunnelconf
        Left err -> do
          echo $ T.pack $ show err
          exit 1

runCmd :: Command -> IO ()
runCmd (Create file) = runCmd' file createNetworkNameSpaces
runCmd (Destroy file) = runCmd' file deleteNetworkNameSpaces
runCmd (Show file) = runCmd' file $ echo . T.pack . show
runCmd (CreateTunnel file) = runCmd'' file createTunnel createTunnel
runCmd (DestroyTunnel file) = runCmd'' file deleteTunnel deleteTunnel
runCmd (ShowTunnel file) = runCmd'' file 
                           (\n t -> echo $ T.decodeUtf8 $ Y.encode (n,t))
                           (\n t -> echo $ T.decodeUtf8 $ Y.encode (n,t))

opts :: ParserInfo Command
opts = info (parse <**> helper) idm

main :: IO ()
main = execParser opts >>= runCmd

