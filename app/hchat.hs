{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-
TODOs:
-- .. too many to list
-}
-- |A very basic chat client
module Main where

import           Control.Concurrent.Async
import           Control.Monad.Trans.State
import           Data.List
import           Data.Maybe
import           Model
import           Network.Router.API
import           Pipes                     (Consumer')
import           Pipes.Prelude             (stdinLn)

type ChatM = StateT ChatState IO

data ChatState = ChatState {
  shownHistory::Bool -- Have we already shown history (previous messages)? This is set to false after the initial display to avoid redoing it again.
  }

t = main

main = do
  -- Uncomment this to see messages being sent and received
  -- logLevel DEBUG

  userName <- getName

  -- The subject of the discussion
  let subjL = ["Haskell","Meeting","Firenze","26 Marzo 2016","Sessione: Applicazioni Distribuite"]
  let subj = Subject subjL
  let msg = Message userName subj

  putStrLn $ unlines [""
                     ,"Help:"
                     ,"To send a message: just enter it and press return."
                     ,"To exit: Ctrl-D." --,"To exit: enter . (a single full stop)"
                     ]

  putStrLn $ unwords ["Current Subject:",prettySubject (Subject []) subj,"\n"]


  -- In a real chat system, it would be wasteful to receive messages about every subject
  -- To filter them at the source, to get only messages about
  -- the subject of our interest and its sub subjects:
  -- bySubject :: Pattern .. -> Pattern ..
  -- let bySubject = $(filterPatternQ [p|Message _ (Subject subj) _|])
  -- runClient def (byPattern (Proxy::Proxy Message) (bySubject (prefixPattern subjL))) $ \conn -> do
  -- NOTE: this is not fully implemented yet so we won't be using it during the meeting

  -- Receive/Send values of type Message
  runClient def (byType (Proxy::Proxy Message)) $ \conn -> do

     -- We can use two different systems to exchange messages:
     -- Either we use send/receive, as in:
    let sendOne = send conn . msg
    -- Or we use pipes with pipeIn/pipeOut (see the beautiful Pipes package for more info: http://hackage.haskell.org/package/pipes) as in:
    -- let sendOne c = runEffect $ yield (msg c) >-> pipeOut conn

    -- Let everybody know that we joined the discussion
    sendOne Join

    -- Then ask for recent messages
    sendOne AskHistory

    -- Asynchronously, receive messages and display them
    -- We use a simple pipe, to get a message from the connection (pipeIn) and print it
    printMessagesTask <- async $ execStateT (runEffect $ pipeIn conn >-> printMessage subj) (ChatState False)

    -- Another pipe, to read lines from the user (stdinLn) and send them out (pipeOut)
    runEffect $ for stdinLn (\txt -> unless (null txt) (yield . msg . TextMessage $ txt)) >-> pipeOut conn

    -- The user has had enough, time to say goodbye
    sendOne Leave

    -- And to cancel the asynchronous receive-and-display messages pipe
    cancel printMessagesTask

  where

     getName = do
       putStrLn "Enter your name:"
       userName <- getLine
       if (length userName < 1)
         then getName
         else return userName

-- Print all messages, relative to the current subject or one of its sub-subjects
printMessage :: Subject -> Consumer' Message ChatM ()
printMessage subj = loop
  where
    loop = do
        msg <- await
        when (inSubject subj $ subject msg) $ do
          mmsg <- lift $ prettyMsg subj msg
             --case prettyMsg subj msg of
          case mmsg of
             Just pr -> liftIO $ putStrLn pr -- yield pr
             Nothing -> return ()
        loop

prettyMsg :: Subject -> Message -> ChatM (Maybe [Prelude.Char])
prettyMsg topSubj msg =
  let header = return . Just . (concat [prettySubject topSubj (subject msg),fromUser msg++": "] ++)
  in case content msg of
    TextMessage txt -> header txt
    Join ->  header "I just joined the discussion."
    Leave -> header "I just left the discussion."
    History msgs -> do
      shown <- state (\s -> (shownHistory s,ChatState True))
      if shown
        then return Nothing
        else Just . unlines . catMaybes <$> (mapM (prettyMsg topSubj) . reverse $ msgs)
    _ -> return Nothing

prettySubject :: Subject -> Subject -> [Prelude.Char]
prettySubject (Subject topSubj) (Subject subj) =
  let s = fromMaybe subj $ stripPrefix topSubj subj
  in if null s
     then ""
     else concat ["(",intercalate "/" s,") "]

inSubject :: Subject -> Subject -> Bool
inSubject (Subject topSubj) (Subject subj) = topSubj `isPrefixOf` subj

