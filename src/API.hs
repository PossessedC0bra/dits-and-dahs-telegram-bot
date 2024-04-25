module API (app) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Proxy (Proxy (..))
import Servant (Handler, Strict, throwError)
import Servant.API (Header', JSON, Post, ReqBody, Required, type (:>))
import Servant.Server (Application, Server, err401, serve)
import System.Environment (getEnv)
import Telegram.Bot.API.SendMessageWebhookResponse (SendMessageWebhookResponse, sendMessageWebhookResponse)
import qualified Telegram.Bot.API.Types.Chat as Chat
import qualified Telegram.Bot.API.Types.Message as Message
import Telegram.Bot.API.Types.Update (Update)
import qualified Telegram.Bot.API.Types.Update as Update

app :: Application
app = serve (Proxy @UpdatesAPI) server

type UpdatesAPI =
  "updates"
    :> Header' [Required, Strict] "X-Telegram-Bot-Api-Secret-Token" String
    :> ReqBody '[JSON] Update
    :> Post '[JSON] SendMessageWebhookResponse

server :: Server UpdatesAPI
server = handleUpdate

handleUpdate :: String -> Update -> Handler SendMessageWebhookResponse
handleUpdate secretToken update = do
  configuredSecretToken <- liftIO $ getEnv "SECRET_TOKEN"

  if secretToken == configuredSecretToken
    then return (sendMessageWebhookResponse (getChatId update) "Hello, world!")
    else throwError err401

getChatId :: Update -> Int
getChatId update
  | Just message <- Update.message update = Chat.id (Message.chat message)
  | otherwise = 0