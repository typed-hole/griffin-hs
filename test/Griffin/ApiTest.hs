{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
module Griffin.ApiTest (tests) where

import qualified Data.Text               as T
import qualified Griffin.Api.V0          as Api
import           Griffin.Client          (ApiKeyInfo (..), Index (..),
                                          Organization (..), getApiKey,
                                          getOrganization, index, ping,
                                          runGriffinClientT)
import           Network.HTTP.Client.TLS (newTlsManager)
import           System.Environment      (getEnv)
import           Test.Hspec              (Spec, describe, example, it, parallel,
                                          runIO)

tests :: Spec
tests = do
  apiKey <- fmap (Api.ApiKey . T.pack) . runIO $ getEnv "GRIFFIN_API_KEY"
  manager <- runIO newTlsManager
  let
    runClient = runGriffinClientT manager apiKey
  parallel $ do
    it "can ping Griffin's API" . example $ do
      runGriffinClientT manager apiKey ping

    describe "/v0/index" $ do
      it "can call the index endpoint" . example $ do
        Index {} <- runClient index
        pure ()

    describe "/v0/api-keys" $ do
      it "can fetch API key info" . example $ do
        runClient $ do
          Index {apiKeyId} <- index
          ApiKeyInfo {} <- getApiKey apiKeyId
          pure ()

    describe "/v0/organizations" $ do
      it "can GET an organization for the API key" . example $ do
        runClient $ do
          Index {apiKeyId} <- index
          ApiKeyInfo {organizationId} <- getApiKey apiKeyId
          Organization {} <- getOrganization organizationId
          pure ()
