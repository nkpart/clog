{-# LANGUAGE QuasiQuotes, TemplateHaskell, TypeFamilies, OverloadedStrings, PackageImports, RankNTypes, LiberalTypeSynonyms #-}
module Clog
    ( Clog (..)
    , ClogRoute (..)
    , resourcesClog
    , Handler
    , Widget
    , module Yesod
    , module Settings
    , module Model
    , StaticRoute (..)
    , addGoogleWebFont
    , runMo
    ) where

import Yesod
import Yesod.Helpers.Static
import qualified Settings
import System.Directory
import qualified Data.ByteString.Lazy as L
import Settings (hamletFile, cassiusFile, juliusFile, widgetFile)
import Model
import Data.Maybe (isJust)
import Control.Monad (join, unless)
import Network.Mail.Mime
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Encoding
import Text.Jasmine (minifym)
import "mtl" Control.Monad.Reader
import Database.MongoDB

data Clog = Clog
    { getStatic :: Static -- ^ Settings for static file serving.
    , connPool :: Settings.ConnPool Settings.Host -- ^ Database connection pool.
    }

type Handler = GHandler Clog Clog
type Widget = GWidget Clog Clog

-- http://docs.yesodweb.com/book/web-routes-quasi/
mkYesodData "Clog" [parseRoutes|
/static StaticR Static getStatic

/favicon.ico FaviconR GET
/robots.txt RobotsR GET

/                         RootR GET
/posts/#Integer/#Int/#Int PostsDayR GET
/posts/#Integer/#Int      PostsMonthR GET
/posts/new                NewPostR POST
/calendar                 CalendarR GET
/admin                    AdminR GET
/admin/posts/#String/delete DeleteR GET POST
|]

addGoogleWebFont :: String -> GWidget sub master ()
addGoogleWebFont = addStylesheetRemote . ("http://fonts.googleapis.com/css?family="++)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod Clog where
    approot _ = Settings.approot

    defaultLayout widget = do
        mmsg <- getMessage
        pc <- widgetToPageContent $ do
            mapM_ addGoogleWebFont ["Bevan", "Inconsolata"]
            widget
            addCassius $(Settings.cassiusFile "default-layout")
        hamletToRepHtml $(Settings.hamletFile "default-layout")

    -- This is done to provide an optimization for serving static files from
    -- a separate domain. Please see the staticroot setting in Settings.hs
    urlRenderOverride a (StaticR s) = Just $ uncurry (joinPath a Settings.staticroot) $ renderRoute s
    urlRenderOverride _ _ = Nothing

    -- The page to be redirected to when authentication is required.
    authRoute _ = Nothing

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent ext' _ content = do
        let fn = base64md5 content ++ '.' : ext'
        let content' =
                if ext' == "js"
                    then case minifym content of
                            Left _ -> content
                            Right y -> y
                    else content
        let statictmp = Settings.staticdir ++ "/tmp/"
        liftIO $ createDirectoryIfMissing True statictmp
        let fn' = statictmp ++ fn
        exists <- liftIO $ doesFileExist fn'
        unless exists $ liftIO $ L.writeFile fn' content'
        return $ Just $ Right (StaticR $ StaticRoute ["tmp", fn] [], [])

newtype A m a = A { unA :: (ReaderT Database (Action m) a) }
runMo action = runDB $ A action

instance YesodPersist Clog where
    type YesodDB Clog = A 
    runDB db = do
      yesod <- getYesod
      let pool = connPool yesod
      liftIOHandler $ Settings.runMongoDB (use (Database "clog") (unA db)) pool
