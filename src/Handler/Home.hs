{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Home (getHomeR) where

import Data.Text (pack)

import Database.Esqueleto.Experimental
    ( select, from, table, orderBy, asc
    , (^.)
    )
import Database.Persist (Entity (Entity))
import Database.Persist.Sql (fromSqlKey)

import Foundation
  ( App
  , Route (SkillsR, ApplicantsR, JobsR , CandidatesR, DocsR)
  , AppMessage
    ( MsgApplicants, MsgHome, MsgDescription , MsgPositions, MsgBreadcrumbs
    , MsgCandidates, MsgClose, MsgDocs, MsgLinks, MsgWelcomeTo, MsgAppName
    , MsgSkills, MsgSelectPositionToRankCandidates
    )
  )
  
import Model (Job(Job), EntityField (JobName))
  
import Settings (widgetFile)

import Yesod (YesodPersist(runDB))
import Yesod.Core (Html, Yesod (defaultLayout), setTitleI)
import Yesod.Core.Handler (HandlerFor)


getHomeR :: HandlerFor App Html
getHomeR = do

    positions <- runDB $ select $ do
        x <- from $ table @Job
        orderBy [asc (x ^. JobName)]
        return x
    
    defaultLayout $ do    
        setTitleI MsgDescription
        $(widgetFile "homepage")
