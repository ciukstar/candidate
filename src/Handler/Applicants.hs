{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE QuasiQuotes #-}

module Handler.Applicants
  ( getApplicantsR
  , postApplicantsR
  , deleteApplicantR
  , getApplicantR
  , postApplicantR
  , postAppSkillsR
  , postAppSkillsEditR
  , postAppSkillR
  , deleteAppSkillR
  , getApplicantCreateFormR
  , getApplicantEditFormR
  , postApplicantTagR
  , getAppPhotoR
  , getAppSkillsEditFormR
  , postApplicantSkillsR
  ) where


import ClassyPrelude.Yesod
    ( ReaderT, MonadIO, YesodPersist (runDB)
    )
  
import Control.Monad (when, forM, forM_)
import Control.Monad.IO.Class (liftIO)
  
import qualified Data.List.Safe as LS (head)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Text as T (null)
import Data.Text (Text, unpack, isInfixOf)
import Data.Text.ICU (LocaleName (Locale))
import Data.Text.ICU.Calendar
    ( setDay, calendar, CalendarType (TraditionalCalendarType) )
import Data.Text.ICU.DateFormatter
  ( formatCalendar, standardDateFormatter
  , FormatStyle (NoFormatStyle, ShortFormatStyle)
  )
import Data.Text.ICU.NumberFormatter (formatDouble')
import Data.Time.Clock (getCurrentTime, UTCTime (utctDay))
import Data.Time.Calendar (toGregorian)

import qualified Database.Persist as P ((=.))
import Database.Persist
  ( Entity (Entity)
  , get, insert, insert_, delete, deleteBy, replace
  , entityKey
  , Entity (entityVal)
  )

import Database.Persist.Sql (SqlBackend, fromSqlKey, toSqlKey)
import Database.Esqueleto.Experimental
  ( SqlQuery, SqlExpr
  , from, table
  , Value (Value, unValue), val
  , countRows, selectOne, select
  , (^.), (==.), (:&)((:&)), (%), (++.), (||.), (&&.), (=.)
  , where_, innerJoin, on, valList, notIn, orderBy, desc, asc
  , like, just, offset, limit, in_, justList, leftJoin, selectQuery
  , groupBy, coalesceDefault, distinct, not_, isNothing, update
  , set, upsert, upper_
  )

import Settings (widgetFile)

import Text.Julius (rawJS)
import Text.Read (readMaybe)

import Foundation
    ( App, Form
    , Route
      ( ApplicantsR, ApplicantR, AppSkillsR, AppSkillR
      , ApplicantCreateFormR, ApplicantEditFormR, HomeR
      , ApplicantTagR, AppPhotoR, PhotoPlaceholderR
      , AppSkillsEditFormR, AppSkillsEditR, ApplicantSkillsR
      )
    , AppMessage
      ( MsgApplicants, MsgSearch, MsgAdd, MsgNoDataFound, MsgPersonalData
      , MsgSave, MsgCancel, MsgDelete, MsgReallyRemove
      , MsgInvalidFormData, MsgEdit, MsgRemove, MsgBreadcrumbs
      , MsgPleaseConfirm, MsgReallyDelete, MsgSkills, MsgEditApplicant
      , MsgClose, MsgWeight, MsgSelect, MsgNumberOfSkills, MsgApplicant
      , MsgHome, MsgLabels, MsgPaginationLabel, MsgRowsPerPage
      , MsgPagination, MsgFirst, MsgPrevious, MsgNext, MsgLast
      , MsgDetails, MsgActions, MsgBirthday, MsgFullName, MsgBack
      , MsgAttributes, MsgDenom, MsgCategories, MsgCategory, MsgAge
      , MsgPhoto, MsgNumberSign, MsgNewApplicant, MsgWeightNotNormal
      , MsgSections, MsgInQuotes, MsgAppSkillAlreadyExists, MsgFamilyName
      , MsgGivenName, MsgAdditionalName, MsgRecordEdited, MsgEditCategory
      , MsgRecordDeleted, MsgFillOutTheFormAndSavePlease, MsgRecordAdded
      , MsgEditTheFormAndSavePlease, MsgBlankValueResetsCategory
      )
    )
  
import Text.Shakespeare.I18N (SomeMessage (SomeMessage))

import Widgets (thSort, thSortDir)

import Yesod.Core
  ( Yesod(defaultLayout), Html, whamlet, toContent, emptyContent
  , setTitleI, typeJpeg
  )
import Yesod.Core.Handler
    ( HandlerFor, lookupSession, reqGetParams
    , getRequest, selectRep, provideRep, redirect
    , getMessages, redirectUltDest, newIdent, fileSourceByteString
    , getUrlRenderParams, getUrlRender, languages, addMessageI
    )
import Yesod.Core.Types (TypedContent (TypedContent), WidgetFor, FileInfo)
import Yesod.Form.Input (runInputPost, runInputGet, ireq, iopt)
import Yesod.Form.Fields
    (textField, fileField, urlField, doubleField, intField, dayField)
import Yesod.Form.Functions (mreq, mopt, generateFormPost,runFormPost)
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess)
    , FieldSettings (FieldSettings, fsAttrs, fsLabel, fsId, fsTooltip, fsName)
    , FieldView (fvId, fvLabel, fvInput, fvErrors, fvRequired)
    )

import Model
    ( Applicant
      ( Applicant, applicantFamilyName, applicantGivenName, applicantBday
      , applicantAdditionalName, applicantTag
      )
    , ApplicantId
    , Skill (Skill, skillName), SkillId, AppSkillId
    , AppSkill (AppSkill, appSkillWeight)
    , Unique (UniqueAppSkill)
    , ultDestKey, Params (Params), AppPhoto (AppPhoto)
    , EntityField
      ( AppSkillApplicant, AppSkillSkill, SkillId, ApplicantId, SkillName
      , AppSkillWeight, AppSkillId, ApplicantFamilyName, ApplicantGivenName
      , ApplicantAdditionalName, ApplicantTag, AppPhotoApplicant, AppPhotoPhoto
      )
    )


getAppPhotoR :: ApplicantId -> HandlerFor App TypedContent
getAppPhotoR aid = do
    appPhoto <- runDB $ selectOne $ do
        x <- from $ table @AppPhoto
        where_ $ x ^. AppPhotoApplicant ==. val aid
        return x
    return $ case appPhoto of
      Just (Entity _ (AppPhoto _ bs _)) -> TypedContent typeJpeg $ toContent bs
      Nothing -> TypedContent typeJpeg emptyContent


deleteAppSkillR :: ApplicantId -> SkillId -> HandlerFor App ()
deleteAppSkillR app skill = do
    runDB $ deleteBy $ UniqueAppSkill app skill
    addMessageI "alert-info toast" MsgRecordDeleted


postAppSkillR :: ApplicantId -> SkillId -> HandlerFor App TypedContent
postAppSkillR aid sid = do
    (w,l) <- runInputPost $ (,)
      <$> ireq doubleField "weight"
      <*> ireq urlField "location"

    runDB $ update $ \s -> do
        set s [AppSkillWeight =. val w]
        where_ $ s ^. AppSkillApplicant ==. val aid
        where_ $ s ^. AppSkillSkill ==. val sid
    redirect l


postApplicantSkillsR :: ApplicantId -> HandlerFor App TypedContent
postApplicantSkillsR aid = do
    let tab = 1 :: Int
    location <- runInputPost $ ireq urlField "location"
    
    applicant <- runDB $ selectOne $ do
        x <- from $ table @Applicant
        where_ $ x ^. ApplicantId ==. val aid
        return x
        
    skills <- runDB $ select $ do
        x <- from $ table @Skill
        orderBy [asc (x ^. SkillName)]
        return x
        
    appSkills <- runDB (select $ do
        (as :& s) <- from $ table @AppSkill
            `innerJoin` table @Skill `on` (\(as :& s) -> as ^. AppSkillSkill ==. s ^. SkillId)
        where_ $ as ^. AppSkillApplicant ==. val aid
        orderBy [asc (as ^. AppSkillId)]
        return (as,s) ) :: HandlerFor App [(Entity AppSkill, Entity Skill)]

    categs <- runDB fetchCategs
    (fw,fe) <- generateFormPost $ formApplicant categs True applicant
    ((sfr,sfw),sfe) <- runFormPost $ formSkills appSkills True (fmtDbl ".######" (Locale "en"))
    
    case sfr of
      FormSuccess xs -> do
          forM_ (zip xs appSkills) (\(weight,(Entity asid _, _)) -> runDB $ update $ \as -> do
                                        set as [AppSkillWeight =. val weight]
                                        where_ $ as ^. AppSkillId ==. val asid
                                    )
          addMessageI "alert-info toast" MsgRecordEdited
          let notNormal = any (\x -> x < 0 || x > 1) xs
          when notNormal $ addMessageI "alert-warning tab-1" MsgWeightNotNormal
          if notNormal then redirect location else redirectUltDest ApplicantsR
        
      _ -> selectRep $ provideRep $ defaultLayout $ do
          setTitleI MsgApplicant
          addMessageI "alert-danger tab-1" MsgInvalidFormData
          msgs <- getMessages
          ult <- getUrlRender >>= \rndr -> fromMaybe (rndr ApplicantsR) <$> lookupSession ultDestKey
          $(widgetFile "applicants/edit")


postAppSkillsEditR :: ApplicantId -> HandlerFor App TypedContent
postAppSkillsEditR aid = do
  location <- runInputPost $ ireq urlField "location"
  applicant <- runDB $ selectOne $ do
    x <- from $ table @Applicant
    where_ $ x ^. ApplicantId ==. val aid
    return x
  skills <- runDB $ select $ do
    x <- from $ table @Skill
    orderBy [asc (x ^. SkillName)]
    return x
  appSkills <- runDB (select $ do
    (as :& s) <- from $ table @AppSkill
      `innerJoin` table @Skill `on` (\(as :& s) -> as ^. AppSkillSkill ==. s ^. SkillId)
    where_ $ as ^. AppSkillApplicant ==. val aid
    orderBy [asc (as ^. AppSkillId)]
    return (as,s) ) :: HandlerFor App [(Entity AppSkill, Entity Skill)]

  ((fr,fw),fe) <- runFormPost $ formSkills appSkills True (fmtDbl ".######" (Locale "en"))
  case fr of
    FormSuccess xs -> do
      forM_ (zip xs appSkills) (\(weight,(Entity asid _, _)) -> runDB $ update $ \as -> do
                                    set as [AppSkillWeight =. val weight]
                                    where_ $ as ^. AppSkillId ==. val asid
                                )
      addMessageI "alert-info toast" MsgRecordEdited
      let notNormal = any (\x -> x < 0 || x > 1) xs
      if notNormal then redirect location else redirectUltDest ApplicantsR
    _ -> selectRep $ provideRep $ defaultLayout $ do
      setTitleI MsgSkills
      addMessageI "alert-danger" MsgInvalidFormData
      msgs <- getMessages
      ult <- getUrlRender >>= \rndr -> fromMaybe (rndr ApplicantsR) <$> lookupSession ultDestKey
      $(widgetFile "applicants/skills")


formSkills :: [(Entity AppSkill, Entity Skill)]
           -> Bool -> (Double -> Text)
           -> Html -> MForm (HandlerFor App) (FormResult [Double], WidgetFor App ())
formSkills skills isPost _ extra = do
  res <- forM skills (
    \(Entity asid as,Entity _ s) -> mreq doubleField FieldSettings
               { fsLabel = SomeMessage (skillName s)
               , fsTooltip = Nothing
               , fsId = Nothing
               , fsName = Nothing
               , fsAttrs = [("class","form-control")]
               } (Just (appSkillWeight as)) >>= \x -> return (x, asid)
    ) :: MForm (HandlerFor App) [((FormResult Double, FieldView App), AppSkillId)]

  let r :: FormResult [Double]
      r = traverse (fst . fst) res

  let w = [whamlet|
#{extra}
<table.table.table-sm.table-borderless.table-hover.w-auto>
  $if not $ null res
    <thead>
      <tr>
        <th.text-body-secondary scope=col>_{MsgDenom}
        <th.text-body-secondary scope=col>_{MsgWeight}
        <th>
    <tbody.align-middle>
      $forall ((_, v), asid) <- res
        <tr>
          <td>
            <label.col-form-label.lh-1 for=#{fvId v}>
              #{fvLabel v}
          <td>
            <div :isJust (fvErrors v):.is-invalid :not (isJust (fvErrors v)) && isPost:.is-valid>
              ^{fvInput v}
          <td>
            <button.btn.btn-sm.btn-outline-danger type=button title=_{MsgRemove}
              data-bs-toggle=modal data-bs-target=#modalRemoveSkill#{fromSqlKey asid}>
              <i.bi.bi-trash>
        $maybe errs <- fvErrors v
          <tr>
            <td>
            <td.pt-0>
              <div.d-block.pt-0.mt-0.lh-1.invalid-feedback>
                #{errs}
            <td>
|]

  return (r,w)


postAppSkillsR :: ApplicantId -> HandlerFor App TypedContent
postAppSkillsR aid = do
  (location,sid,weight) <- runInputPost $ (,,)
    <$> ireq urlField "location"
    <*> (toSqlKey <$> ireq intField "skill")
    <*> ireq doubleField "weight"

  mAppSkill <- runDB $ selectOne $ do
    x <- from $ table @AppSkill
    where_ $ x ^. AppSkillApplicant ==. val aid &&. x ^. AppSkillSkill ==. val sid
    return x

  case mAppSkill of
    Nothing -> do
      runDB $ insert_ $ AppSkill aid sid weight
      addMessageI "alert-info toast" MsgRecordAdded
    _ -> addMessageI "alert-danger" MsgAppSkillAlreadyExists
  redirect location


getAppSkillsEditFormR :: ApplicantId -> HandlerFor App Html
getAppSkillsEditFormR aid = do
  applicant <- runDB $ selectOne $ do
    x <- from $ table @Applicant
    where_ $ x ^. ApplicantId ==. val aid
    return x
  appSkills <- runDB $ select $ do
    (a :& s) <- from $ table @AppSkill
      `innerJoin` table @Skill `on` (\(a :& s) -> a ^. AppSkillSkill ==. s ^. SkillId)
    where_ $ a ^. AppSkillApplicant ==. val aid
    orderBy [asc (a ^. AppSkillId)]
    return (a,s)

  skills <- runDB $ select $ do
    s <- from $ table @Skill
    where_ $ (s ^. SkillId) `notIn` valList (entityKey . snd <$> appSkills)
    return s

  (fw,fe) <- generateFormPost $ formSkills appSkills False (fmtDbl ".######" (Locale "en"))

  defaultLayout $ do
    setTitleI MsgSkills
    when (any (\(Entity _ (AppSkill _ _ w), _) -> w < 0 || w > 1) appSkills) $
      addMessageI "alert-warning" MsgWeightNotNormal
    msgs <- getMessages
    let ultParams  = [("desc","id"),("offset","0"),("limit","5")]
    ult <- getUrlRenderParams >>= \rndr -> fromMaybe (rndr ApplicantsR ultParams) <$> lookupSession ultDestKey
    $(widgetFile "applicants/skills")


postApplicantR :: ApplicantId -> HandlerFor App TypedContent
postApplicantR aid = selectRep $ provideRep $ do

  tab <- fromMaybe 0 <$> runInputGet (iopt intField "tab") :: HandlerFor App Int
  appSkills <- runDB $ select $ do
    (a :& s) <- from $ table @AppSkill
      `innerJoin` table @Skill `on` (\(a :& s) -> a ^. AppSkillSkill ==. s ^. SkillId)
    where_ $ a ^. AppSkillApplicant ==. val aid
    orderBy [asc (a ^. AppSkillId)]
    return (a,s)

  categs <- runDB fetchCategs
  applicant <- runDB $ selectOne $ do
    x <- from $ table @Applicant
    where_ $ x ^. ApplicantId ==. val aid
    return x
  ((fr,fw),fe) <- runFormPost $ formApplicant categs True applicant
  (sfw,sfe) <- generateFormPost $ formSkills appSkills True (fmtDbl ".######" (Locale "en"))
  case fr of
    FormSuccess (x,mph) -> do
      runDB $ replace aid x
      case mph of
        Just photo -> do
          bs <- fileSourceByteString photo
          _ <- runDB $ upsert (AppPhoto aid bs "image/jpeg") [AppPhotoPhoto P.=. bs]
          return ()
        Nothing -> return ()
      addMessageI "alert-info toast" MsgRecordEdited
      redirectUltDest ApplicantsR
    _ -> do
      skills <- runDB $ select $ do
        s <- from $ table @Skill
        where_ $ (s ^. SkillId) `notIn` valList (entityKey . snd <$> appSkills)
        return s

      defaultLayout $ do
        setTitleI MsgApplicant
        let ultParams  = [("desc","id"),("offset","0"),("limit","5")]
        ult <- getUrlRenderParams >>= \rndr -> fromMaybe (rndr ApplicantsR ultParams) <$> lookupSession ultDestKey
        addMessageI "alert-danger tab-0" MsgInvalidFormData
        msgs <- getMessages
        $(widgetFile "applicants/edit")


getApplicantR :: ApplicantId -> HandlerFor App TypedContent
getApplicantR aid = do
  tab <- runInputGet $ iopt intField "tab" :: HandlerFor App (Maybe Int)
  applicant <- do
    ma <- runDB $ get aid
    y <- liftIO $ (\(y,_,_) -> y) . toGregorian . utctDay <$> getCurrentTime
    return $ ma >>= \a -> return (a, (y -) . (\(y',_,_) -> y') . toGregorian <$> applicantBday a)

  appSkills <- runDB $ select $ do
    (a :& s) <- from $ table @AppSkill
      `innerJoin` table @Skill `on` (\(a :& s) -> a ^. AppSkillSkill ==. s ^. SkillId)
    where_ $ a ^. AppSkillApplicant ==. val aid
    orderBy [asc (a ^. AppSkillId)]
    return (a,s)

  loc <- Locale . unpack . fromMaybe "en" . LS.head <$> languages
  cal <- liftIO $ calendar "GMT+0300" loc TraditionalCalendarType
  fmtDay <- liftIO $ standardDateFormatter NoFormatStyle ShortFormatStyle loc "GMT+0300"

  selectRep $ provideRep $ defaultLayout $ do
    let ultParams  = [("desc","id"),("offset","0"),("limit","5")]
    ult <- getUrlRenderParams >>= \rndr -> fromMaybe (rndr ApplicantsR ultParams) <$> lookupSession ultDestKey
    setTitleI MsgApplicant
    $(widgetFile "applicants/applicant")


deleteApplicantR :: ApplicantId -> HandlerFor App ()
deleteApplicantR aid = do
  runDB $ delete aid
  addMessageI "alert-info toast" MsgRecordDeleted


postApplicantsR :: HandlerFor App TypedContent
postApplicantsR = selectRep $ provideRep $ do
  categs <- runDB fetchCategs
  ((r,widget),enctype) <- runFormPost $ formApplicant categs True Nothing
  case r of
    FormSuccess (a,Just ph) -> do
      aid <- runDB $ insert a
      bs <- fileSourceByteString ph
      let photo = AppPhoto aid bs "image/jpeg"
      runDB $ insert_ photo
      addMessageI "alert-info toast" MsgRecordAdded
      redirectUltDest ApplicantsR
    FormSuccess (x,Nothing) -> do
      runDB $ insert_ x
      addMessageI "alert-info toast" MsgRecordAdded
      redirectUltDest ApplicantsR
    _ -> defaultLayout $ do
      setTitleI MsgApplicant
      addMessageI "alert-danger" MsgInvalidFormData
      msgs <- getMessages
      let ultParams  = [("desc","id"),("offset","0"),("limit","5")]
      ult <- getUrlRenderParams >>= \rndr -> fromMaybe (rndr ApplicantsR ultParams) <$> lookupSession ultDestKey
      $(widgetFile "applicants/create")


postApplicantTagR :: HandlerFor App TypedContent
postApplicantTagR = do
    (old,mnew) <- runInputPost $ (,) <$> ireq textField "old" <*> iopt textField "new"
    
    case mnew of
      Just new -> runDB $ update $ \a -> do
          set a [ApplicantTag =. just (val new)]
          where_ $ a ^. ApplicantTag ==. just (val old)
      _otherwise -> runDB $ update $ \a -> do
          set a [ApplicantTag =. val Nothing]
          where_ $ a ^. ApplicantTag ==. just (val old)
          
    addMessageI "alert-info toast" MsgRecordEdited
    redirectUltDest ApplicantsR


getApplicantsR :: HandlerFor App TypedContent
getApplicantsR = do
    params@(Params mq moffset mlimit msort tags) <- do
        params <- reqGetParams <$> getRequest
        return $ Params
            (snd <$> LS.head (filter (\(k,v) -> (k == "q") && not (T.null v)) params))
            (readMaybe . unpack . snd =<< LS.head (filter ((== "offset") . fst) params))
            (readMaybe . unpack . snd =<< LS.head (filter ((== "limit") . fst) params))
            (LS.head (filter ((\x -> x == "asc" || x == "desc") . fst) params))
            (snd <$> filter ((== "tag") . fst) params)

    rc <- runDB $ fetchCount params
    applicants <- do
        xs <- runDB $ fetchApplicants params
        y <- liftIO $ (\(a,_,_) -> a) . toGregorian . utctDay <$> getCurrentTime
        return $ (\(e,ns) ->
                    (e,ns, (y -) . (\(y',_,_) -> y') . toGregorian <$> (applicantBday . entityVal) e)
                 ) <$> xs
    alltags <- do
        xs <- (unValue <$>) <$> runDB
            ( select $ distinct $ do
                  a <- from $ table @Applicant
                  where_ $ not_ $ isNothing (a ^. ApplicantTag)
                  orderBy [asc (a ^. ApplicantTag)]
                  return (a ^. ApplicantTag)
            )
        forM xs (\x -> newIdent >>= \i -> return (i,x))

    let maxo = fromMaybe 0 $ (*)
            <$> ((+) <$> (div rc <$> mlimit) <*> ((\x -> if mod rc x > 0 then 0 else -1) <$> mlimit))
            <*> mlimit
    let next = maybe 0 (min maxo) $ (+) <$> moffset <*> mlimit
    let prev = maybe 0 (max 0) $ (-) <$> moffset <*> mlimit
    let start = maybe 0 (\x -> if maxo < 0 then 0 else x + 1) moffset
    let end = fromMaybe 0 $ min <$> ((+) <$> moffset <*> mlimit) <*> pure rc

    let ultParams  = [("desc","id"),("offset","0"),("limit","5")]
    ult <- getUrlRenderParams >>= \r -> fromMaybe (r ApplicantsR ultParams) <$> lookupSession ultDestKey
        
    msgs <- getMessages
    selectRep $ provideRep $ defaultLayout $ do
        setTitleI MsgApplicants
        idInputSearch <- newIdent
        idSelectLimit <- newIdent
        idSelectLimit2 <- newIdent
        $(widgetFile "applicants/applicants")


getApplicantEditFormR :: ApplicantId -> HandlerFor App Html
getApplicantEditFormR aid = do

  tab <- fromMaybe 0 <$> runInputGet (iopt intField "tab") :: HandlerFor App Int

  applicant <- runDB $ selectOne $ do
      x <- from $ table @Applicant
      where_ $ x ^. ApplicantId ==. val aid
      return x
    
  appSkills <- runDB $ select $ do
      (a :& s) <- from $ table @AppSkill
          `innerJoin` table @Skill `on` (\(a :& s) -> a ^. AppSkillSkill ==. s ^. SkillId)
      where_ $ a ^. AppSkillApplicant ==. val aid
      orderBy [asc (a ^. AppSkillId)]
      return (a,s)

  skills <- runDB $ select $ do
      s <- from $ table @Skill
      where_ $ (s ^. SkillId) `notIn` valList (entityKey . snd <$> appSkills)
      return s

  categs <- runDB fetchCategs

  (fw,fe) <- generateFormPost $ formApplicant categs False applicant
  (sfw,sfe) <- generateFormPost $ formSkills appSkills False (fmtDbl ".######" (Locale "en"))
  
  defaultLayout $ do
      setTitleI MsgApplicant
      let ultParams  = [("desc","id"),("offset","0"),("limit","5")]
      ult <- getUrlRenderParams >>= \rndr -> fromMaybe (rndr ApplicantsR ultParams) <$> lookupSession ultDestKey
      msgs <- getMessages
      $(widgetFile "applicants/edit")


getApplicantCreateFormR :: HandlerFor App Html
getApplicantCreateFormR = do
    
    categs <- runDB fetchCategs
    
    (widget,enctype) <- generateFormPost $ formApplicant categs False Nothing
    
    let ultParams  = [("desc","id"),("offset","0"),("limit","5")]
    ult <- getUrlRenderParams >>= \rndr -> fromMaybe (rndr ApplicantsR ultParams) <$> lookupSession ultDestKey
    
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgApplicant
        $(widgetFile "applicants/create")


fetchCategs :: MonadIO m => ReaderT SqlBackend m [Text]
fetchCategs = do
    xs <- select $ distinct queryCategs
    return $ unValue <$> xs


queryCategs :: SqlQuery (SqlExpr (Value Text))
queryCategs = do
    a <- from $ table @Applicant
    where_ $ not_ $ isNothing (a ^. ApplicantTag)
    return $ coalesceDefault [a ^. ApplicantTag] (val "")


formApplicant :: [Text] -> Bool -> Maybe (Entity Applicant) -> Form (Applicant, Maybe FileInfo)
formApplicant categs isPost applicant extra = do

    (sR,sV) <- mreq textField (fs MsgFamilyName) (applicantFamilyName . entityVal <$> applicant)

    (nR,nV) <- mreq textField (fs MsgGivenName) (applicantGivenName . entityVal <$> applicant)

    (pR,pV) <- mopt textField (fs MsgAdditionalName) (applicantAdditionalName . entityVal <$> applicant)
    
    (bR,bV) <- mopt dayField (fs MsgBirthday) (applicantBday . entityVal <$> applicant)
    
    (tR,tV) <- mopt textField FieldSettings
        { fsLabel = SomeMessage MsgCategory
        , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
        , fsAttrs = [("class","form-control"),("list","datalistCategory")]
        } (applicantTag . entityVal <$> applicant)
        
    (fR,fV) <- mopt fileField FieldSettings
        { fsLabel = SomeMessage MsgPhoto
        , fsTooltip = Just (SomeMessage MsgPhoto), fsId = Nothing, fsName = Nothing
        , fsAttrs = [("style","display:none")]
        } Nothing

    let r  = (,) <$> (Applicant <$> sR <*> nR <*> pR <*> bR <*> tR) <*> fR
    
    idImgPhoto <- newIdent
    
    let w = $(widgetFile "applicants/form")

    return (r,w)
    
  where
      
      fs msg = FieldSettings (SomeMessage msg) Nothing Nothing Nothing
          [("class","form-control")]


menuActions :: ApplicantId -> WidgetFor App ()
menuActions aid = [whamlet|
<div.dropdown>
  <button.btn.btn-light.rounded-circle type=button
    data-bs-toggle=dropdown aria-expanded=false title=_{MsgActions}>
    <i.bi.bi-three-dots-vertical>
  <ul.dropdown-menu>
    <li>
      <a.dropdown-item href=@{ApplicantEditFormR aid}?tab=0 rel=edit-form>
        <i.bi.bi-pencil.me-2>
        _{MsgEdit}
    <li>
      <button.dropdown-item type=button
        data-bs-toggle=modal data-bs-target=#modalDelete#{fromSqlKey aid}>
        <i.bi.bi-trash.me-2>
        _{MsgDelete}
    <li>
      <a.dropdown-item href=@{ApplicantR aid}?tab=0 rel=item>
        <i.bi.bi-card-text.me-2>
        _{MsgDetails}
    <li>
      <hr.dropdown-divider>
    <li>
      <a.dropdown-item href=@{AppSkillsEditFormR aid} rel=edit-form>
        <i.bi.bi-pencil.me-2>
        _{MsgSkills}
|]


fetchApplicants :: MonadIO m => Params -> ReaderT SqlBackend m [(Entity Applicant, Value Int)]
fetchApplicants params = select $ queryApplicants params


queryApplicants :: Params -> SqlQuery (SqlExpr (Entity Applicant), SqlExpr (Value Int))
queryApplicants (Params mq moffset mlimit msort tags) = do
  (x :& (_, ns)) <- from $ table @Applicant
    `leftJoin` ( selectQuery $ do
                   s <- from $ table @AppSkill
                   groupBy (s ^. AppSkillApplicant)
                   return (s ^. AppSkillApplicant, countRows :: SqlExpr (Value Int))
               ) `on` (\(a :& (asid, _)) -> asid ==. just (a ^. ApplicantId))

  case mq of
    Just q -> where_ $ (upper_ (x ^. ApplicantFamilyName) `like` (%) ++. upper_ (val q) ++. (%))
      ||. (upper_ (x ^. ApplicantGivenName) `like` (%) ++. upper_ (val q) ++. (%))
      ||. (upper_ (x ^. ApplicantAdditionalName) `like` (%) ++. just (upper_ (val q)) ++. (%))
    Nothing -> return ()

  case tags of
    [] -> return ()
    xs -> where_ $ x ^. ApplicantTag `in_` justList (valList xs)

  let nskills = coalesceDefault [ns] (val 0)

  case msort of
    Just ("asc","surname") -> orderBy [asc (x ^. ApplicantFamilyName)]
    Just ("desc","surname") -> orderBy [desc (x ^. ApplicantFamilyName)]
    Just ("asc","name") -> orderBy [asc (x ^. ApplicantGivenName)]
    Just ("desc","name") -> orderBy [desc (x ^. ApplicantGivenName)]
    Just ("asc","patronymic") -> orderBy [asc (x ^. ApplicantAdditionalName)]
    Just ("desc","patronymic") -> orderBy [desc (x ^. ApplicantAdditionalName)]
    Just ("asc","skills") -> orderBy [asc nskills]
    Just ("desc","skills") -> orderBy [desc nskills]
    _ -> orderBy [desc (x ^. ApplicantId)]

  case moffset of
    Just n -> offset $ fromIntegral n
    Nothing -> return ()

  case mlimit of
    Just n -> limit $ fromIntegral n
    Nothing -> return ()

  return (x, nskills)

fetchCount :: MonadIO m => Params -> ReaderT SqlBackend m Int
fetchCount params = unValue . fromMaybe (Value 0) <$> selectOne (queryCount params)


queryCount :: Params -> SqlQuery (SqlExpr (Value Int))
queryCount (Params mq _ _ _ tags) = do
  x <- from $ table @Applicant

  case mq of
    Just q -> where_ $ (upper_ (x ^. ApplicantFamilyName) `like` (%) ++. upper_ (val q) ++. (%))
      ||. (upper_ (x ^. ApplicantGivenName) `like` (%) ++. upper_ (val q) ++. (%))
      ||. (upper_ (x ^. ApplicantAdditionalName) `like` (%) ++. just (upper_ (val q)) ++. (%))
    Nothing -> return ()

  case tags of
    [] -> return ()
    xs -> where_ $ x ^. ApplicantTag `in_` justList (valList xs)

  return countRows

fmtDbl :: Text -> LocaleName -> Double -> Text
fmtDbl = formatDouble'
