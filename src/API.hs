module API (app) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Proxy (Proxy (..))
import Servant (Handler, Strict, throwError)
import Servant.API (Header', JSON, Post, ReqBody, Required, type (:>))
import Servant.Server (Application, Server, err401, serve)
import System.Environment (getEnv)
import Telegram.Bot.API.Types.Update (Update)

app :: Application
app = serve (Proxy @UpdatesAPI) server

type UpdatesAPI =
  "updates"
    :> Header' [Required, Strict] "X-Telegram-Bot-Api-Secret-Token" String
    :> ReqBody '[JSON] Update
    :> Post '[JSON] Update

server :: Server UpdatesAPI
server = handleUpdate

handleUpdate :: String -> Update -> Handler Update
handleUpdate secretToken update = do
  configuredSecretToken <- liftIO $ getEnv "SECRET_TOKEN"

  if secretToken == configuredSecretToken
    then return update
    else throwError err401