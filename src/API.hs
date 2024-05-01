module API (app) where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Data.Proxy (Proxy (..))
import Servant (Handler, Strict, throwError)
import Servant.API (Header', JSON, Post, ReqBody, Required, type (:>))
import Servant.Server (Application, Server, err401, serve)
import System.Environment (getEnv)
import qualified Telegram.Bot.API.Types.Chat as Chat
import qualified Telegram.Bot.API.Types.Message as Message
import Telegram.Bot.API.Types.SendMessagePayload (SendMessagePayload (..))
import Telegram.Bot.API.Types.Update (Update)
import qualified Telegram.Bot.API.Types.Update as Update
import Telegram.Bot.API.WebhookResponse (WebhookResponse (..))
import qualified Telegram.Bot.API.WebhookResponse as Method (Method (..))

app :: Application
app = serve (Proxy @UpdatesAPI) server

type UpdatesAPI =
  "updates"
    :> Header' [Required, Strict] "X-Telegram-Bot-Api-Secret-Token" String
    :> ReqBody '[JSON] Update
    :> Post '[JSON] (WebhookResponse SendMessagePayload)

server :: Server UpdatesAPI
server = handleUpdate
  
handleUpdate :: String -> Update -> Handler (WebhookResponse SendMessagePayload)
handleUpdate secretToken update = do
  configuredSecretToken <- liftIO $ getEnv "SECRET_TOKEN"

  if secretToken == configuredSecretToken
    then return (WebhookResponse Method.SendMessage SendMessagePayload {chat_id = getChatId update, text = "Sali du!"})
    else throwError err401

getChatId :: Update -> Int
getChatId update
  | Just message <- Update.message update = Chat.id (Message.chat message)
  | otherwise = 0