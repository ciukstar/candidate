{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Home (getHomeR) where

import Yesod.Core.Handler (HandlerFor)
import Yesod.Core (Html, Yesod (defaultLayout), setTitleI)
import Settings (widgetFile)

import Foundation
  ( App
  , Route (SkillsR, ApplicantsR, JobsR , CandidatesR)
  , AppMessage
    ( MsgSkill, MsgSkills, MsgSeeLink, MsgApplicant, MsgApplicants, MsgHome
    , MsgDescription , MsgPosition, MsgPositions, MsgOverview, MsgBreadcrumbs
    , MsgCandidate, MsgCandidates, MsgClose, MsgTags , MsgDoc1, MsgDoc11
    , MsgDoc12, MsgDoc13, MsgDoc14 , MsgDoc15, MsgDoc16, MsgDoc17
    , MsgDoc18, MsgDoc19, MsgDoc20, MsgDoc21
    )
  )

getHomeR :: HandlerFor App Html
getHomeR = defaultLayout $ do
  setTitleI MsgDescription
  $(widgetFile "homepage")
