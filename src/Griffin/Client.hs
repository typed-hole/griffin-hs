{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE OverloadedStrings     #-}
module Griffin.Client
  ( GriffinClientT
  , Index (..)
  , ApiKeyInfo (..)
  , Organization (..)
  , runGriffinClientT
  , index
  , ping
  , getApiKey
  , getOrganization
  ) where

import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Trans.Reader (ReaderT (ReaderT))
import           Data.Aeson.TH              (SumEncoding (UntaggedValue),
                                             defaultOptions, deriveJSON)
import           Data.Aeson.Types           (Options (..))
import           Data.Data                  (Proxy (..))
import           Data.Function              ((&))
import           Data.Kind                  (Type)
import           Data.Maybe                 (fromMaybe)
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Griffin.Api.V0             (ApiKeyId)
import qualified Griffin.Api.V0             as Api
import qualified Griffin.Api.V0             as API
import           Network.HTTP.Client        (Manager)
import           Servant.API                (Capture, Get, GetNoContent, Header,
                                             Header', JSON,
                                             NoContent (NoContent),
                                             ToHttpApiData (toUrlPiece),
                                             type (:<|>) ((:<|>)), type (:>))
import           Servant.API.Modifiers      (Required, Strict)
import           Servant.Client             (BaseUrl (BaseUrl), ClientEnv,
                                             ClientError, ClientM,
                                             Scheme (Https), client,
                                             mkClientEnv, runClientM)
import           Servant.Client.Streaming   (BaseUrl (..))
import           Text.Casing                (fromHumps, toKebab)

data GriffinEnv = GriffinEnv
  { apiKey    :: Api.ApiKey
  , clientEnv :: ClientEnv
  }

newtype GriffinClientT (m :: Type -> Type) (a :: Type) where
  GriffinClientT :: {unGriffinClientT :: GriffinEnv -> m a} -> GriffinClientT m a
  deriving (Functor, Applicative, Monad) via (ReaderT GriffinEnv m)

runGriffinClientT :: Manager -> Api.ApiKey -> GriffinClientT m a -> m a
runGriffinClientT manager apiKey m = unGriffinClientT m GriffinEnv
  { apiKey
  , clientEnv
  }
  where
    clientEnv = mkClientEnv
      manager
      BaseUrl
        { baseUrlScheme = Https
        , baseUrlHost = "api.griffin.sh"
        , baseUrlPath = ""
        , baseUrlPort = 443
        }

runClient :: MonadIO m => ClientEnv -> ClientM a -> m (Either ClientError a)
runClient env = liftIO . flip runClientM env

ping :: MonadIO m => GriffinClientT m ()
ping = GriffinClientT $ \GriffinEnv {clientEnv} -> do
  runClient clientEnv Api.getPing >>= \case
    Right NoContent -> pure ()

newtype Index = Index
  { apiKeyId :: ApiKeyId
  }

index :: MonadIO m => GriffinClientT m Index
index = GriffinClientT $ \GriffinEnv {apiKey, clientEnv} -> do
  runClient clientEnv (Api.getIndex apiKey) >>= \case
    Right Api.GetIndex {apiKeyUrl} -> do
      let
        apiKeyId =
          Api.ApiKeyId
          . fromMaybe ""
          . T.stripPrefix "/v0/api-keys/"
          $ apiKeyUrl
      pure Index
        { apiKeyId = apiKeyId
        }
    Left err  -> do
      error . show $ err

data ApiKeyInfo = ApiKeyInfo
  { isLive         :: Bool
  , name           :: Text
  , organizationId :: Api.OrganizationId
  , userId         :: Text
  }

getApiKey :: MonadIO m => Api.ApiKeyId -> GriffinClientT m ApiKeyInfo
getApiKey apiKeyId = GriffinClientT $ \GriffinEnv {apiKey, clientEnv} -> do
  runClient clientEnv (Api.getApiKey apiKey apiKeyId) >>= \case
    Right Api.GetApiKey {apiKeyLive, apiKeyName, organizationUrl, userUrl} -> do
      let
        organizationId =
          Api.OrganizationId
          . fromMaybe ""
          . T.stripPrefix "/v0/organizations/"
          $ organizationUrl
      pure ApiKeyInfo
        { isLive = apiKeyLive
        , name = apiKeyName
        , organizationId
        , userId = undefined userUrl
        }
    Left err  -> error . show $ err

data Organization = Organization
  { name :: Text
  , mode :: Text
  }

getOrganization :: MonadIO m => Api.OrganizationId -> GriffinClientT m Organization
getOrganization orgId = GriffinClientT $ \GriffinEnv {apiKey, clientEnv} -> do
  runClient clientEnv (Api.getOrganization apiKey orgId) >>= \case
    Right Api.GetOrganization {displayName, organizationMode} -> do
      pure Organization
        { name = displayName
        , mode = organizationMode
        }
    Left err -> error . show $ err
