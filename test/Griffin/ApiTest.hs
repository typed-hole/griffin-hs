module Griffin.ApiTest (tests) where

import           Griffin.Client          (ping, runGriffinClientT)
import           Network.HTTP.Client.TLS (mkManagerSettings, newTlsManager)
import           Test.Hspec              (Spec, example, hspec, it, parallel)

tests :: Spec
tests =
  parallel $ do
    it "can ping Griffin's API" . example $ do
      manager <- newTlsManager
      runGriffinClientT manager ping
