{-# LANGUAGE ScopedTypeVariables  #-}
module Main where

import           Model
import           Control.Monad.Trans.State
import           Network.Router.API

{-
 Store all messages and return them on request

 TODOs:
 Return also messages relative to sub subjects.
 Limit number of messages stored.
 Persist messages
 ..
-}

t = main

type HistoryM = StateT [Message] IO

main = do
   logLevel DEBUG
   --let def2 = def {ip="127.0.0.1",port=8080}
   forever $ do
    Left (ex :: SomeException) <- try $ runClient def (byType (Proxy::Proxy Message)) $ \conn -> do
       liftIO $ dbgS "connected"
       execStateT (runEffect $ pipeIn conn >-> historyAgent >-> pipeOut conn) []

    dbg ["Exited loop with error",concat ["'",show ex,"'"],"retrying in a bit."]
    threadDelay $ 5 * 1000000

   where

     historyAgent = do
       msg <- await
       lift $ addMsg msg
       case content msg of
          AskHistory -> do
            msgs <- lift $ getMsgsBySubject (subject msg)
            yield $ Message userName (subject msg) (History msgs)
          _ -> return ()
       historyAgent

     userName = "hchat-history"

     addMsg msg = modify (msg:)
     getMsgsBySubject subj = gets (take 100 . filter (\msg -> subject msg == subj))

