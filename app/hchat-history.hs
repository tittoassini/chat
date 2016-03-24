{-# LANGUAGE ScopedTypeVariables  #-}
module Main where

import           Model
import           Control.Monad.Trans.State
import           Network.Router.API

{-
 A simple bot that stores all messages it sees and returns them on request.

 TODOs:
 Return also messages relative to sub subjects.
 Limit number of messages stored.
 Store only relevant messages.
 Persist messages
 ..
-}

t = main

type HistoryM = StateT [Message] IO

main = do
  -- Display messages sent and received
  logLevel DEBUG

  -- Use local server
  --let def2 = def {ip="127.0.0.1",port=8080}

  -- Protect against crashes, restart on failure
  forever $ do
    -- Receive all values of type Message
    Left (ex :: SomeException) <- try $ runClient def (byType (Proxy::Proxy Message)) $ \conn -> do

      liftIO $ dbgS "connected"
      execStateT (runEffect $ pipeIn conn >-> historyAgent >-> pipeOut conn) []

    -- Something went wrong, wait a few seconds and restart
    dbg ["Exited loop with error",concat ["'",show ex,"'"],"retrying in a bit."]
    threadDelay $ seconds 5

   where
     seconds = (* 1000000)

     historyAgent = do
       msg <- await
       -- Store all messages
       lift $ addMsg msg
       case content msg of
          -- On request, send a list of recent messages
          AskHistory -> do
            msgs <- lift $ getMsgsBySubject (subject msg)
            yield $ Message userName (subject msg) (History msgs)
          _ -> return ()
       historyAgent

     userName = "hchat-history"

     addMsg msg = modify (msg:)
     getMsgsBySubject subj = gets (take 50 . filter (\msg -> subject msg == subj))

