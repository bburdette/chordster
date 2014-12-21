module Handler.DeleteOscDest where

import Import

getDeleteOscDestR :: OSCDestId -> Handler Html
getDeleteOscDestR odid= do
  _ <- runDB $ delete odid
  redirect OscDestsR 
