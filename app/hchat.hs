{-# LANGUAGE TemplateHaskell #-}
{-
TODOs:
.. add
-}
module Main where

import           Model
import           Control.Monad.Trans.State
import           Network.Router.API
import           Pipes.Prelude (stdinLn)--Quid2                (await, for, lift, runEffect,
--                                             ``d, wsIn, wsOut, yield, (>->))
import           Data.List
import           Data.Maybe
import Control.Concurrent.Async 

type ChatM = StateT ChatState IO

data ChatState = ChatState {shownHistory::Bool}

t = main

main = do
  -- logLevel DEBUG

  userName <- getName

  let subjL = ["Haskell","Meeting","Firenze","26 Marzo 2016","Sessione: Applicazioni Distribuite"]
  let subj = Subject subjL 
  let msg = Message userName subj

  putStrLn $ unlines [""
                     ,"Help:"
                     ,"To send a message: just enter it and press return."
                     ,"To exit: Ctrl-D." --,"To exit: enter . (a single full stop)"
                     ]

  putStrLn $ unwords ["Current Subject:",prettySubject (Subject []) subj,"\n"]


  -- In a real chat system, it would wasteful to receive messages about every subject
  -- To filter them at the source so that we get only messages about
  -- the subject of our interest, and its sub subjects:
  -- bySubject :: Pattern .. -> Pattern ..
  -- let bySubject = $(filterPatternQ [p|Message _ (Subject subj) _|])
  -- runClient def (byPattern (Proxy::Proxy Message) (bySubject (prefixPattern subjL))) $ \conn -> do
  -- NOTE: this is not fully implemented yet so we won't be using it during the meeting

  -- Receive/Send all values of type Message
  runClient def (byType (Proxy::Proxy Message)) $ \conn -> do

    let sendOne c = runEffect $ yield (msg c) >-> pipeOut conn
    sendOne Join
    sendOne AskHistory

    printMessages <- async $ execStateT (runEffect $ pipeIn conn >-> printMessage subj) (ChatState False)
    runEffect $ for stdinLn (\txt -> unless (null txt) (yield . msg . TextMessage $ txt)) >-> pipeOut conn

    sendOne Leave
    cancel printMessages

  where

     getName = do
       putStrLn "Enter your name:"
       userName <- getLine
       if (length userName < 1)
         then getName
         else return userName

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

prettyMsg
  :: Monad m =>
     Subject -> Message -> StateT ChatState m (Maybe [Prelude.Char])
prettyMsg topSubj msg =
  let header = return . Just . (concat [prettySubject topSubj (subject msg),fromUser msg++": "] ++)
  in case content msg of
    TextMessage txt -> header txt
    Join ->  header "I just joined the discussion."
    Leave -> header "I just left he discussion."
    History msgs -> do
      shown <- state (\s -> (shownHistory s,ChatState True))
      if shown
        then return Nothing
        else Just . unlines . catMaybes <$> (mapM (prettyMsg topSubj) . reverse $ msgs)
    _ -> return Nothing

prettySubject (Subject topSubj) (Subject subj) =
  let s = fromMaybe subj $ stripPrefix topSubj subj
  in if null s
     then ""
     else concat ["(",intercalate "/" s,") "]

inSubject (Subject topSubj) (Subject subj) = topSubj `isPrefixOf` subj

