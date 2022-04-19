   -- https://ro-che.info/articles/2016-04-14-scotty-http-basic-auth
{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Network.Wai.Middleware.HttpAuth
import Data.SecureMem -- for constant-time comparison
import Lucid -- for HTML generation

password :: SecureMem
password = secureMemFromByteString "An7aLasi" -- https://xkcd.com/221/

main :: IO ()
main = scotty 8000 $ do
  middleware $ basicAuth (\u p -> return $ u == "user" && secureMemFromByteString p == password)
    "Bananas and lenses recording"

  get "/" . html . renderText $ do
    doctype_
    html_ $ do
      head_ $ do
        title_ "Bananas and lenses recording"

      body_ $ h1_ "Hello world!"
