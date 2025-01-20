{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

-- | Common handler functions.
module Handler.Common
  ( getPhotoPlaceholderR, getFaviconR, getRobotsR
  , getSitemapR, getWebAppManifestR
  ) where

import Control.Monad (Monad(return))

import GHC.Num ((*))

import Data.Aeson (object, (.=))
import Data.Conduit (yield)
import Data.Function (($))
import Data.Functor ((<$>))
import Data.FileEmbed (embedFile)
import Data.Maybe (Maybe (Nothing, Just))

import Foundation
    ( App, Handler
    , Route (HomeR, DocsR, StaticR)
    , AppMessage (MsgAppName, MsgMetaDescription, MsgCandidateRanking)
    )

import Settings.StaticFiles
    ( img_logo_144x144_svg
    , img_screenshot_1_narrow_png, img_screenshot_1_wide_png
    , img_screenshot_1_narrow_fr_png, img_screenshot_1_wide_fr_png
    , img_screenshot_1_narrow_ro_png, img_screenshot_1_wide_ro_png
    , img_screenshot_1_narrow_ru_png, img_screenshot_1_wide_ru_png
    )

import Yesod.Core.Handler
    ( HandlerFor, cacheSeconds, getUrlRender, getMessageRender, selectRep
    , languages
    )
import Yesod.Core.Content
  ( TypedContent (TypedContent)
  , ToContent (toContent), typePlain, typeSvg
  )
import Yesod.Core.Json (provideJson, Value (String), array)
import Yesod.Sitemap
    ( sitemap, SitemapUrl (SitemapUrl), SitemapChangeFreq (Monthly)
    )
import qualified Data.List.Safe as LS

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getPhotoPlaceholderR :: Handler TypedContent
getPhotoPlaceholderR = do
  cacheSeconds $ 60 * 60 * 24 * 30
  return $ TypedContent typeSvg
         $ toContent $(embedFile "static/img/person-fill.svg")


getWebAppManifestR :: Handler TypedContent
getWebAppManifestR = do
    urlr <- getUrlRender
    msgr <- getMessageRender
    lang <- LS.head <$> languages
    selectRep $ provideJson $ object
        [ "name" .= msgr MsgAppName
        , "short_name" .= msgr MsgAppName
        , "description" .= msgr MsgMetaDescription
        , "categories" .= array [String "social"]
        , "start_url" .= urlr HomeR
        , "theme_color" .= String "#FFFFFF"
        , "background_color" .= String "#FFFFFF"
        , "display" .= String "standalone"
        , "icons" .= array [ object [ "src" .= urlr (StaticR img_logo_144x144_svg)
                                    , "type" .= String "image/svg+xml"
                                    , "sizes" .= String "144x144"
                                    , "purpose" .= String "any"
                                    ]
                           , object [ "src" .= urlr (StaticR img_logo_144x144_svg)
                                    , "type" .= String "image/svg+xml"
                                    , "sizes" .= String "144x144"
                                    , "purpose" .= String "maskable"
                                    ]
                           ]
        , "screenshots" .= array [ object [ "src" .= urlr ( StaticR $ case lang of
                                                              Just "fr" -> img_screenshot_1_narrow_fr_png
                                                              Just "ro" -> img_screenshot_1_narrow_ro_png
                                                              Just "ru" -> img_screenshot_1_narrow_ru_png
                                                              Just _ -> img_screenshot_1_narrow_png
                                                              Nothing -> img_screenshot_1_narrow_png
                                                          )
                                          , "sizes" .= String "450x796"
                                          , "type" .= String "image/png"
                                          , "form_factor" .= String "narrow"
                                          , "label" .= msgr MsgCandidateRanking
                                          ]
                                 , object [ "src" .= urlr ( StaticR $ case lang of
                                                              Just "fr" -> img_screenshot_1_wide_fr_png
                                                              Just "ro" -> img_screenshot_1_wide_ro_png
                                                              Just "ru" -> img_screenshot_1_wide_ru_png
                                                              Just _ -> img_screenshot_1_wide_png
                                                              Nothing -> img_screenshot_1_wide_png
                                                          )
                                          , "sizes" .= String "1920x930"
                                          , "type" .= String "image/png"
                                          , "form_factor" .= String "wide"
                                          , "label" .= msgr MsgCandidateRanking
                                          ]
                                 ]
        ]


getSitemapR :: Handler TypedContent
getSitemapR = sitemap $ do
    yield $ SitemapUrl HomeR Nothing (Just Monthly) (Just 1.0)
    yield $ SitemapUrl DocsR Nothing (Just Monthly) (Just 1.0)
    

getFaviconR :: HandlerFor App TypedContent
getFaviconR = do cacheSeconds $ 60 * 60 * 24 * 30 -- cache for a month
                 return $ TypedContent "image/x-icon"
                        $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: HandlerFor App TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")
