import Network.Wai.Handler.Warp
import Network.Wai.Frontend.MonadCGI

import Network.XmlRpc.Server
import Network.CGI

main = Network.Wai.Handler.Warp.runSettings 
       ( defaultSettings { settingsTimeout = 1, settingsPort = 9999 } )
       application

application = Network.Wai.Frontend.MonadCGI.cgiToApp cgi

cgi = do
         input <- getBody
         result <- liftIO $ handleCall server input
         outputFPS result

server = methods [ ("leak", fun leak) ]

leak :: Int -> Int -> IO Int
leak x y = return $ x + y

