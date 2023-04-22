{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | Common handler functions.
module Handler.Common (getPhotoPlaceholderR, getFaviconR, getRobotsR) where

import GHC.Num ((*))
import Data.Function (($))
import Data.FileEmbed (embedFile)
import Yesod.Core.Handler (HandlerFor, cacheSeconds)
import Foundation (App)
import Yesod.Core.Content
  ( TypedContent (TypedContent)
  , ToContent (toContent), typePlain, typeSvg
  )
import Control.Monad (Monad(return))
-- import Import

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getPhotoPlaceholderR :: HandlerFor App TypedContent
getPhotoPlaceholderR = do
  cacheSeconds $ 60 * 60 * 24 * 30
  return $ TypedContent typeSvg
         $ toContent $(embedFile "static/img/person-fill.svg")

getFaviconR :: HandlerFor App TypedContent
getFaviconR = do cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
                 return $ TypedContent "image/x-icon"
                        $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: HandlerFor App TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")
