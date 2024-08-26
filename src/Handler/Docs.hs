{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}


module Handler.Docs (getDocsR) where

import Foundation
  ( Handler
  , Route (StaticR, SkillsR, ApplicantsR, JobsR , CandidatesR)
  , AppMessage
    ( MsgSkill, MsgSkills, MsgSeeLink, MsgApplicant, MsgApplicants, MsgHome
    , MsgDescription , MsgPosition, MsgPositions, MsgOverview, MsgBreadcrumbs
    , MsgCandidate, MsgCandidates, MsgClose, MsgBasicEntities, MsgErDiagram
    , MsgTags, MsgEntityRelationshipDiagram
    , MsgDoc1, MsgDoc11
    , MsgDoc12, MsgDoc13, MsgDoc14 , MsgDoc15, MsgDoc16
    , MsgDoc18, MsgDoc19, MsgDoc20, MsgDoc21
    )
  )
  
import Settings (widgetFile)
import Settings.StaticFiles (img_Candidate_ERD_svg)

import Text.Hamlet (Html)

import Yesod.Core (Yesod(defaultLayout), setTitleI)


getDocsR :: Handler Html
getDocsR = defaultLayout $ do
    setTitleI MsgDescription
    $(widgetFile "docs/docs")
