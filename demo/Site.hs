{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  -- ( app
  -- ) where
  where

------------------------------------------------------------------------------
import Control.Applicative
import Control.Lens
import Control.Monad
import Data.ByteString                             (ByteString)
import Data.Monoid
import Database.Persist.Sql
import Heist
import Snap.Core
import Snap.Extras.FlashNotice
import Snap.Extras.MethodOverride
import Snap.Snaplet
import Snap.Snaplet.Auth
import Snap.Snaplet.Auth.Backends.JsonFile
import Snap.Snaplet.Heist
import Snap.Snaplet.Persistent
import Snap.Snaplet.Session.Backends.CookieSession
import Snap.Util.FileServe
import Text.Digestive.Snap                         hiding (method)
------------------------------------------------------------------------------
import Application
import Snap.Snaplet.Assets

routes :: [(ByteString, AppHandler ())]
routes = [ ("/r/:slug",  cRender "single")
         , ("/e/:key",   needAuth $ method GET (cRender "edit")
                                <|> method POST handleEditPost)
         , ("/d/:key",   needAuth $ handleMethodOverride $ method DELETE handleDelete)
         , ("/n",        needAuth $ method GET (cRender "new")
                                <|> method POST handleNewPost)

         , ("/in",       needNoAuth $ with auth $ method GET (cRender "login")
                                              <|> method POST handleLoginPost)
         , ("/out",      needAuth $ with auth $ logout >> redirect "/")

         , ("/s",        serveDirectory "static")

         , ("/",         ifTop $ cRender "home")

         , ("",          handle404)
         ]
    where needAuth = requireUser auth pass
          needNoAuth x = requireUser auth x (redirect "/")
          handle404 = do
              modifyResponse $ setResponseCode 404
              cRender "404"

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "An snaplet example application." Nothing $ do
    let hc = emptyHeistConfig & hcNamespace .~ ""
                              & hcErrorNotBound .~ True
    h <- nestSnaplet "" heist $ heistInit' "templates" hc
    s <- nestSnaplet "sess" sess $
        initCookieSessionManager "site_key.txt" "sess" (Just 3600)
    a <- nestSnaplet "auth" auth $
        initJsonFileAuthManager defAuthSettings sess "users.json"
    p <- nestSnaplet "db" db (initPersist (runMigration migrateAll))
    ac <- nestSnaplet "assets" assets (initAssets h Nothing)

    addAssetRoutes ac
    initFlashNotice h sess
    addRoutes routes
    return $ App h s a ac p
