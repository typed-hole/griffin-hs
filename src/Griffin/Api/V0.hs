{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitNamespaces    #-}
{-# LANGUAGE OverloadedStrings     #-}
module Griffin.Api.V0
  ( ApiKey (..)
  , ApiKeyId (..)
  , OrganizationId (..)
  , GetIndex (..)
  , GetApiKey (..)
  , GetOrganization (..)
  , getPing
  , getIndex
  , getApiKey
  , getOrganization
  ) where
import           Data.Aeson            (FromJSON, Value, withObject, (.:))
import           Data.Aeson.Types      (FromJSON (parseJSON), Parser)
import           Data.Data             (Proxy (..))
import           Data.Text             (Text)
import           Servant.API           (Capture, Get, GetNoContent, Header',
                                        JSON, NoContent,
                                        ToHttpApiData (toUrlPiece),
                                        type (:<|>) ((:<|>)), type (:>))
import           Servant.API.Modifiers (Required, Strict)
import           Servant.Client        (ClientM, client)

newtype ApiKey = ApiKey Text

instance ToHttpApiData ApiKey where
  toUrlPiece :: ApiKey -> Text
  toUrlPiece (ApiKey key) = "Authorization: GriffinAPIKey " <> key

data GetIndex = GetIndex
  { organizationsUrl :: Text
  , usersUrl         :: Text
  , rolesUrl         :: Text
  , apiKeyUrl        :: Text
  }
  deriving stock (Show, Eq)

instance FromJSON GetIndex where
  parseJSON :: Value -> Parser GetIndex
  parseJSON = withObject "GetApiKey" $ \p -> GetIndex
    <$> p .: "organizations-url"
    <*> p .: "users-url"
    <*> p .: "roles-url"
    <*> p .: "api-key-url"

data GetApiKey = GetApiKey
  { apiKeyUrl       :: Text
  , apiKeyName      :: Text
  , apiKeyLive      :: Bool
  , organizationUrl :: Text
  , userUrl         :: Text
  }

instance FromJSON GetApiKey where
  parseJSON :: Value -> Parser GetApiKey
  parseJSON = withObject "GetApiKey" $ \p -> GetApiKey
    <$> p .: "api-key-url"
    <*> p .: "api-key-name"
    <*> p .: "api-key-live?"
    <*> p .: "organization-url"
    <*> p .: "user-url"

data GetOrganization = GetOrganization
  { displayName                 :: Text
  , organizationBankAccountsUrl :: Text
  , organizationMode            :: Text
  }

instance FromJSON GetOrganization where
  parseJSON :: Value -> Parser GetOrganization
  parseJSON = withObject "GetOrganization" $ \o -> GetOrganization
    <$> o .: "display-name"
    <*> o .: "organization-bank-accounts-url"
    <*> o .: "organization-mode"

type Authd = Header' '[Required, Strict] "Authorization" ApiKey

newtype ApiKeyId = ApiKeyId Text
  deriving (Show, Eq, ToHttpApiData) via Text

newtype OrganizationId = OrganizationId Text
  deriving (Show, Eq, ToHttpApiData) via Text

type V0 =
                "ping" :> GetNoContent
  :<|> Authd :> "index" :> Get '[JSON] GetIndex
  :<|> Authd :> "api-keys" :> Capture "api-key-id" ApiKeyId :> Get '[JSON] GetApiKey
  :<|> Authd :> "organizations" :> Capture "organization-id" OrganizationId :> Get '[JSON] GetOrganization

type GriffinApi =
  "v0" :> V0

getPing :: ClientM NoContent
getIndex :: ApiKey -> ClientM GetIndex
getApiKey :: ApiKey -> ApiKeyId -> ClientM GetApiKey
getOrganization :: ApiKey -> OrganizationId -> ClientM GetOrganization
getPing :<|> getIndex :<|> getApiKey :<|> getOrganization = client $ Proxy @GriffinApi
