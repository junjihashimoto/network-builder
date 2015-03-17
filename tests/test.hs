{-#LANGUAGE TemplateHaskell#-}
{-#LANGUAGE QuasiQuotes#-}
{-#LANGUAGE OverloadedStrings#-}

import Test.Hspec
import Test.Hspec.Server

import Test.Cabal.Path
import System.Process

data NameSpace = NameSpace{
  nName :: String
, nOS :: !(Maybe ServerOS)
} deriving (Show,Eq)

namespace :: String -> NameSpace
namespace name = NameSpace name Nothing

instance ServerType NameSpace where
  stSetup a = do
    os' <- detectOS a
    return $ a {nOS = os'}
  stOS = nOS
  stName = nName
  stCmd d c arg i = do
    readProcessWithExitCode "sudo" ( ["ip", "netns", "exec", nName d, c] ++ arg ) i

main :: IO ()
main = do
  bin <-  getExePath "." "network-builder"
  hspec $ do
    describe "Create Network" $ with localhost $ do
      it "create network" $ do
        command bin ["create","--conf","sample/sample-gre/network-builder.yml"] [] @>= exit 0
    describe "Test Network from server2" $ with (namespace "server2") $ do
      it "ping" $ do
        host "127.0.0.1" @>= reachable
        host "192.168.10.1" @>= reachable
        host "192.168.10.2" @>= reachable
        host "192.168.10.3" @>= reachable
        host "192.168.11.1" @>= reachable
        host "192.168.11.4" @>= reachable
        host "192.168.12.1" @== none
        host "192.168.12.4" @== none
    describe "Test Network from server3" $ with (namespace "server3") $ do
      it "ping" $ do
        host "127.0.0.1" @>= reachable
        host "192.168.10.1" @>= reachable
        host "192.168.10.2" @>= reachable
        host "192.168.10.3" @>= reachable
        host "192.168.11.1" @>= reachable
        host "192.168.11.4" @>= reachable
        host "192.168.12.1" @== none
        host "192.168.12.4" @== none
    describe "Create Gre Tunnel" $ with localhost $ do
      it "create tunnel from server2 to server4" $ do
        command bin ["create-tunnel","sample/sample-gre/tunnel0.yml"] [] @>= exit 0
      it "create tunnel from server4 to server2" $ do
        command bin ["create-tunnel","sample/sample-gre/tunnel1.yml"] [] @>= exit 0
    describe "Test Network from server2" $ with (namespace "server2") $ do
      it "ping" $ do
        host "127.0.0.1" @>= reachable
        host "192.168.10.1" @>= reachable
        host "192.168.10.2" @>= reachable
        host "192.168.10.3" @>= reachable
        host "192.168.11.1" @>= reachable
        host "192.168.11.4" @>= reachable
        host "192.168.12.1" @>= reachable
        host "192.168.12.4" @>= reachable
    describe "Test Network from server3" $ with (namespace "server3") $ do
      it "ping" $ do
        host "127.0.0.1" @>= reachable
        host "192.168.10.1" @>= reachable
        host "192.168.10.2" @>= reachable
        host "192.168.10.3" @>= reachable
        host "192.168.11.1" @>= reachable
        host "192.168.11.4" @>= reachable
        host "192.168.12.1" @== reachable
        host "192.168.12.4" @== reachable
    describe "Destroy Network with Gre Tunnel" $ with localhost $ do
      it "destroy network" $ do
        command bin ["destroy","--conf","sample/sample-gre/network-builder.yml"] [] @>= exit 0
    describe "Create Network with GreTap Tunnel" $ with localhost $ do
      it "create" $ do
        command bin ["create","--conf","sample/sample-gretap/network-builder.yml"] [] @>= exit 0
    describe "Test Network from server2" $ with (namespace "server2") $ do
      it "ping" $ do
        host "127.0.0.1" @>= reachable
        host "192.168.10.1" @>= reachable
        host "192.168.10.2" @>= reachable
        host "192.168.10.3" @>= reachable
        host "192.168.11.1" @>= reachable
        host "192.168.11.2" @>= reachable
        host "192.168.11.3" @== none
        host "192.168.11.4" @== none
    describe "Test Network from server3" $ with (namespace "server3") $ do
      it "ping" $ do
        host "127.0.0.1" @>= reachable
        host "192.168.10.1" @>= reachable
        host "192.168.10.2" @>= reachable
        host "192.168.10.3" @>= reachable
        host "192.168.11.1" @>= reachable
        host "192.168.11.2" @>= reachable
        host "192.168.11.3" @== none
        host "192.168.11.4" @== none
    describe "Create GreTap Tunnel" $ with localhost $ do
      it "create tunnel from server2 to server4" $ do
        command bin ["create-tunnel","sample/sample-gretap/tunnel0.yml"] [] @>= exit 0
      it "create tunnel from server4 to server2" $ do
        command bin ["create-tunnel","sample/sample-gretap/tunnel1.yml"] [] @>= exit 0
    describe "Test Network from server2" $ with (namespace "server2") $ do
      it "ping" $ do
        host "127.0.0.1" @>= reachable
        host "192.168.10.1" @>= reachable
        host "192.168.10.2" @>= reachable
        host "192.168.10.3" @>= reachable
        host "192.168.11.1" @>= reachable
        host "192.168.11.2" @>= reachable
        host "192.168.11.3" @== reachable
        host "192.168.11.4" @== reachable
    describe "Test Network from server3" $ with (namespace "server3") $ do
      it "ping" $ do
        host "127.0.0.1" @>= reachable
        host "192.168.10.1" @>= reachable
        host "192.168.10.2" @>= reachable
        host "192.168.10.3" @>= reachable
        host "192.168.11.1" @>= reachable
        host "192.168.11.2" @>= reachable
        host "192.168.11.3" @== reachable
        host "192.168.11.4" @== reachable
    describe "Destroy Network with GreTap Tunnel" $ with localhost $ do
      it "destroy" $ do
        command bin ["destroy","--conf","sample/sample-gretap/network-builder.yml"] [] @>= exit 0
