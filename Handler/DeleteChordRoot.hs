module Handler.DeleteChordRoot where

import Import

getDeleteChordRootR :: ChordRootId -> Handler Html
getDeleteChordRootR cid = do 
  runDB $ delete cid
  redirect ChordRootsR
