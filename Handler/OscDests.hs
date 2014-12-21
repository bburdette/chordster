module Handler.OscDests where

import Import
import qualified Data.Text as T

oscDestForm :: Maybe OSCDest -> Form OSCDest
oscDestForm mbod = renderTable $ OSCDest
  <$> areq textField "Name" (oSCDestName <$> mbod)
  <*> areq textField "IP" (oSCDestIp <$> mbod)
  <*> areq intField "Port" (oSCDestPort <$> mbod)
  <*> areq (selectFieldList [(T.pack "chords", T.pack "chords"),(T.pack "lights",T.pack "lights")]) "Type" (oSCDestType <$> mbod)


getOscDestsR :: Handler Html
getOscDestsR = do 
  dests <- runDB $ selectList [] []
  (widg,etype) <- generateFormPost $ oscDestForm Nothing
  defaultLayout $ [whamlet|
    <h1> OSC destinations
    <table> 
      <tr>
        <th>Name
        <th>IP
        <th>Port
        <th>Type
      $forall Entity odid od <- dests
        <tr> 
         <td>
           #{oSCDestName od}
         <td>
           #{show $ oSCDestIp od}
         <td>
           #{oSCDestPort od}
         <td>
           #{oSCDestType od}
         <td>
           <a href=@{DeleteOscDestR odid}> delete
    <form method=post entype=#{etype}>
      ^{widg}
      <input type=submit value="add dest">
  |]

postOscDestsR :: Handler Html
postOscDestsR = do
  ((res,_),_) <- runFormPost $ oscDestForm Nothing
  case res of 
    FormSuccess oscdest -> do
      _ <- runDB $ insert oscdest
      redirect OscDestsR 
    _ -> error "insert fail"

