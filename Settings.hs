{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell, OverloadedStrings, PackageImports #-}
-- | Settings are centralized, as much as possible, into this file. This
-- includes database connection settings, static file locations, etc.
-- In addition, you can configure a number of different aspects of Yesod
-- by overriding methods in the Yesod typeclass. That instance is
-- declared in the Clog.hs file.
module Settings
    ( hamletFile
    , cassiusFile
    , juliusFile
    , widgetFile
    , Host
    , ConnPool
    , withMongoPool
    , runMongoDB
    , approot
    , staticroot
    , staticdir
    , timeZone
    ) where

import qualified Text.Hamlet as H
import qualified Text.Cassius as H
import qualified Text.Julius as H
import Language.Haskell.TH.Syntax
import Yesod (MonadPeelIO, addWidget, addCassius, addJulius)
import Data.Monoid (mempty)
import System.Directory (doesFileExist)
import Database.MongoDB
import "mtl" Control.Monad.Reader
import Data.Time.LocalTime

timeZone = TimeZone (10*60) False "AU"

-- | The base URL for your application. This will usually be different for
-- development and production. Yesod automatically constructs URLs for you,
-- so this value must be accurate to create valid links.
approot :: String
#ifdef PRODUCTION
-- You probably want to change this. If your domain name was "yesod.com",
-- you would probably want it to be:
-- > approot = "http://www.yesod.com"
-- Please note that there is no trailing slash.
approot = "http://localhost:3000"
#else
approot = "http://localhost:3000"
#endif

-- | The location of static files on your system. This is a file system
-- path. The default value works properly with your scaffolded site.
staticdir :: FilePath
staticdir = "static"

-- | The base URL for your static files. As you can see by the default
-- value, this can simply be "static" appended to your application root.
-- A powerful optimization can be serving static files from a separate
-- domain name. This allows you to use a web server optimized for static
-- files, more easily set expires and cache values, and avoid possibly
-- costly transference of cookies on static files. For more information,
-- please see:
--   http://code.google.com/speed/page-speed/docs/request.html#ServeFromCookielessDomain
--
-- If you change the resource pattern for StaticR in Clog.hs, you will
-- have to make a corresponding change here.
--
-- To see how this value is used, see urlRenderOverride in Clog.hs
staticroot :: String
staticroot = approot ++ "/static"

-- MongoDb database
mongoDatabase :: String
#ifdef PRODUCTION
mongoDatabase = "production"
#else
mongoDatabase = "development"
#endif

connectionCount :: Int
connectionCount = 10

-- The following three functions are used for calling HTML, CSS and
-- Javascript templates from your Haskell code. During development,
-- the "Debug" versions of these functions are used so that changes to
-- the templates are immediately reflected in an already running
-- application. When making a production compile, the non-debug version
-- is used for increased performance.
--
-- You can see an example of how to call these functions in Handler/Root.hs
--
-- Note: due to polymorphic Hamlet templates, hamletFileDebug is no longer
-- used; to get the same auto-loading effect, it is recommended that you
-- use the devel server.

toHamletFile, toCassiusFile, toJuliusFile :: String -> FilePath
toHamletFile x = "hamlet/" ++ x ++ ".hamlet"
toCassiusFile x = "cassius/" ++ x ++ ".cassius"
toJuliusFile x = "julius/" ++ x ++ ".julius"

hamletFile :: FilePath -> Q Exp
hamletFile = H.hamletFile . toHamletFile

cassiusFile :: FilePath -> Q Exp
#ifdef PRODUCTION
cassiusFile = H.cassiusFile . toCassiusFile
#else
cassiusFile = H.cassiusFileDebug . toCassiusFile
#endif

juliusFile :: FilePath -> Q Exp
#ifdef PRODUCTION
juliusFile = H.juliusFile . toJuliusFile
#else
juliusFile = H.juliusFileDebug . toJuliusFile
#endif

widgetFile :: FilePath -> Q Exp
widgetFile x = do
    let h = unlessExists toHamletFile hamletFile
    let c = unlessExists toCassiusFile cassiusFile
    let j = unlessExists toJuliusFile juliusFile
    [|addWidget $h >> addCassius $c >> addJulius $j|]
  where
    unlessExists tofn f = do
        e <- qRunIO $ doesFileExist $ tofn x
        if e then f x else [|mempty|]

runMongoDB :: (Service s, MonadIO m) => Action m a -> ConnPool s -> m a
runMongoDB action pool = do
  Right v <- access safe Master pool action 
  return v

withMongoPool f  = newConnPool connectionCount (host "127.0.0.1") >>= f
