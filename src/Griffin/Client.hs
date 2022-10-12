{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingVia        #-}
{-# LANGUAGE ExplicitNamespaces #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE LambdaCase         #-}
module Griffin.Client
  ( GriffinClientT
  , runGriffinClientT
  , ping
  ) where

import           Control.Monad.IO.Class     (MonadIO (..))
import           Control.Monad.Trans.Reader (ReaderT (ReaderT))
import           Data.Data                  (Proxy (..))
import           Data.Function              ((&))
import           Data.Kind                  (Type)
import           Network.HTTP.Client        (Manager)
import           Servant.API                (GetNoContent,
                                             NoContent (NoContent), type (:>))
import           Servant.Client             (BaseUrl (BaseUrl), ClientEnv,
                                             Scheme (Https), client,
                                             mkClientEnv, runClientM)
import           Servant.Client.Streaming   (BaseUrl (..))

newtype GriffinClientT (m :: Type -> Type) (a :: Type) where
  GriffinClientT :: {unGriffinClientT :: ClientEnv -> m a} -> GriffinClientT m a
  deriving (Functor, Applicative, Monad) via (ReaderT ClientEnv m)

runGriffinClientT :: Manager -> GriffinClientT m a -> m a
runGriffinClientT manager m = unGriffinClientT m clientEnv
  where
    clientEnv = mkClientEnv
      manager
      BaseUrl
        { baseUrlScheme = Https
        , baseUrlHost = "api.griffin.sh"
        , baseUrlPath = ""
        , baseUrlPort = 443
        }

type Ping = "v0" :> "ping" :> GetNoContent

ping :: MonadIO m => GriffinClientT m ()
ping = GriffinClientT $ \clientEnv -> do
  runClientM (client (Proxy @Ping)) clientEnv & liftIO >>= \case
    Right NoContent -> pure ()
