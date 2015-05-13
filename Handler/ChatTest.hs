module Handler.ChatTest where

import Import
import Yesod.WebSockets
import Control.Monad (forever)
import Control.Monad.Trans.Reader
import Control.Concurrent (threadDelay)
import Control.Concurrent.STM.TChan
import Control.Concurrent.STM
import Data.Time
import Data.Monoid ((<>))
import qualified Data.Text as T

import Data.Time
import Data.Conduit.Combinators
import Data.Conduit
import Conduit

chatApp :: WebSocketsT Handler ()
chatApp = do
  sendTextData ("Welcome to the chat server, please enter your name." :: T.Text)
  name <- receiveData 
  sendTextData $ "Welcome, " <> name
  app <- getYesod
  let writeChan = chatLine app
  readChan <- (liftIO . atomically) $ do
    writeTChan writeChan $ name <> " has joined the chat"
    dupTChan writeChan
  race_
    (forever $ (liftIO . atomically) (readTChan readChan) >>= sendTextData)
    (forever $ 
      do
        dt <- receiveData
        (liftIO . atomically) $ 
          writeTChan writeChan $ T.append name (T.append ": " dt))

--    (sourceWS $$ mapM_C (\msg ->
--      (liftIO . atomically) $ writeTChan writeChan $ name <> ": " <> msg))

getChatTestR :: Handler Html
getChatTestR = do
  webSockets chatApp
  defaultLayout $ do
      [whamlet|
          <div #output>
          <form #form>
              <input #input autofocus>
      |]
      toWidget [lucius|
          \#output {
              width: 600px;
              height: 400px;
              border: 1px solid black;
              margin-bottom: 1em;
              p {
                  margin: 0 0 0.5em 0;
                  padding: 0 0 0.5em 0;
                  border-bottom: 1px dashed #99aa99;
              }
          }
          \#input {
              width: 600px;
              display: block;
          }
      |]
      toWidget [julius|
          var url = document.URL,
              output = document.getElementById("output"),
              form = document.getElementById("form"),
              input = document.getElementById("input"),
              conn;

          url = url.replace("http:", "ws:").replace("https:", "wss:");
          conn = new WebSocket(url);

          conn.onmessage = function(e) {
              var p = document.createElement("p");
              p.appendChild(document.createTextNode(e.data));
              output.appendChild(p);
          };

          form.addEventListener("submit", function(e){
              conn.send(input.value);
              input.value = "";
              e.preventDefault();
          });
      |]


postChatTestR :: Handler Html
postChatTestR = error "Not yet implemented: postChatTestR"
