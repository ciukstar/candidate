{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Widgets (thSort, thSortDir) where

import Data.Text (Text)
import Yesod.Core.Widget (WidgetFor)
import Foundation (App, AppMessage)
import Settings (widgetFile)


thSort :: Maybe (Text,Text) -> Text -> AppMessage -> Text -> WidgetFor App ()
thSort msort field label form = $(widgetFile "widgets")

  
thSortDir :: Text -> Text
thSortDir "asc" = "desc"
thSortDir _ = "asc"
