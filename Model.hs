{-# LANGUAGE QuasiQuotes, TypeFamilies, GeneralizedNewtypeDeriving, TemplateHaskell, OverloadedStrings, PackageImports #-}
{-# OPTIONS_GHC -fwarn-missing-signatures #-}
module Model where

import qualified Data.Text as T
import Data.Time.Clock
import Database.MongoDB as Mo
import Yesod
import Control.Applicative
import "mtl" Control.Monad.Reader
import Data.Maybe

data Post = Post T.Text UTCTime deriving (Eq, Show)

runClog = use (Database "clog")

parsePost document = do
  text <- T.pack <$> Mo.lookup "text" document
  time <- timestamp <$> Mo.lookup "_id" document
  return $ Post text time

posts = runClog $ do
  posts <- Mo.rest =<< (Mo.find (Mo.select [] "posts"))
  return $ (catMaybes . map parsePost) posts

insertPost text = runClog $ do
  Mo.insert "posts" $ ["text" =: text] 

  
