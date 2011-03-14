{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Controller
    ( withClog
    ) where

import Clog
import Settings
import Yesod.Helpers.Static
import Data.ByteString (ByteString)

-- Import all relevant handler modules here.
import Handler.Root

-- This line actually creates our YesodSite instance. It is the second half
-- of the call to mkYesodData which occurs in Clog.hs. Please see
-- the comments there for more details.
mkYesodDispatch "Clog" resourcesClog

-- Some default handlers that ship with the Yesod site template. You will
-- very rarely need to modify this.
getFaviconR :: Handler ()
getFaviconR = sendFile "image/x-icon" "favicon.ico"

getRobotsR :: Handler RepPlain
getRobotsR = return $ RepPlain $ toContent ("User-agent: *" :: ByteString)

-- This function allocates resources (such as a database connection pool),
-- performs initialization and creates a WAI application. This is also the
-- place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
withClog :: (Application -> IO a) -> IO a
withClog f = Settings.withMongoPool $ \p -> do
    let h = Clog s p
    toWaiApp h >>= f
  where
    s = static Settings.staticdir
