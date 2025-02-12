{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}

module Handler.Jobs
  ( getJobsR, postJobsR
  , getJobR, postJobR  
  , getJobCreateFormR
  , getJobEditFormR
  , deleteJobR
  , postJobSkillsR
  , deleteJobSkillR
  , putJobSkillR
  , getJobSkillsEditFormR
  , postJobSkillsEditR
  , getJobCandidatesR
  , getJobCandidateR
  ) where

import ClassyPrelude.Yesod (ReaderT)

import Control.Monad (forM, forM_, when, unless)

import Data.Bifunctor (first)
import Data.List (sortBy)
import Data.List.Safe as LS (head)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (Text, pack, unpack, isInfixOf)
import qualified Data.Text as T (null)
import Data.Time (formatTime, Day, defaultTimeLocale)
import Data.Text.ICU.Calendar (setDay)
import Data.Text.ICU
    ( LocaleName(Locale), calendar
    , CalendarType (TraditionalCalendarType), formatDouble'
    )
import Data.Text.ICU.DateFormatter
    ( formatCalendar, standardDateFormatter
    , FormatStyle (ShortFormatStyle, NoFormatStyle)
    )
  
import Database.Esqueleto.Experimental
    ( select, from, table, SqlQuery, SqlExpr, orderBy, desc
    , (^.), (?.), (==.), (%), (++.), (||.), (&&.), (:&)((:&)), (=.)
    , where_, valList, notIn, select, val
    , innerJoin, on, withRecursive, unionAll_
    , isNothing, just, like, limit, offset, asc, countRows
    , Value (Value, unValue), selectOne, coalesceDefault, groupBy
    , not_, exists, leftJoin, in_, justList, update, set, selectQuery
    , upper_
    )
import qualified Database.Persist as P ((=.), update)
import Database.Persist as P
    ( PersistStoreWrite(insert_, replace, delete)
    , PersistStoreRead(get)
    , Entity (Entity, entityKey, entityVal)
    , selectList, SelectOpt (Asc)
    )
import Database.Persist.Sql (SqlBackend, fromSqlKey, toSqlKey)

import Handler.Candidates
    ( fetchNumCandidates, calcWeights, gtZero, allLeafs, filterAppSkills
    , maxHeight, bldThead, candidateInfo
    )
import Handler.Depts (deptTreeWidget)

import Foundation
    ( App, Handler, Form
    , Route
      ( JobsR, JobR, JobSkillsR, JobSkillR, JobCreateFormR, JobEditFormR
      , JobCandidatesR, HomeR, JobSkillsEditFormR, DeptsR, JobSkillsEditR
      , DeptR, JobCandidateR, PhotoPlaceholderR, AppPhotoR
      )
    , AppMessage
      ( MsgPositions, MsgSearch, MsgAdd, MsgSave, MsgCancel
      , MsgCode, MsgName, MsgDescription, MsgDayStart, MsgDayEnd
      , MsgDenom, MsgPosition, MsgNoDataFound, MsgEdit, MsgRemove
      , MsgNumberSign, MsgInvalidFormData, MsgClose, MsgPleaseConfirm
      , MsgReallyDelete, MsgSkills, MsgSelect, MsgReallyRemove
      , MsgCandidates, MsgActions, MsgEditPosition, MsgCreatePosition
      , MsgHome, MsgDetails, MsgBack, MsgDayStart, MsgDayEnd, MsgTags
      , MsgRowsPerPage, MsgPaginationLabel, MsgPagination, MsgFirst
      , MsgPrevious, MsgNext, MsgLast, MsgNumberOfSkills, MsgEditSkills
      , MsgNumberOfCandidates, MsgDivisions, MsgDivision, MsgLabels
      , MsgAttributes, MsgActions, MsgSections, MsgCumulativeWeightNotNormal
      , MsgTotalValue, MsgExpand, MsgCollapse, MsgDuplicateCode, MsgInvalidEndDay
      , MsgJobSkillAlreadyExists, MsgInQuotes, MsgDelete, MsgRecordAdded
      , MsgRecordDeleted, MsgRecordEdited, MsgFillOutTheFormAndSavePlease
      , MsgNewPosition, MsgEditTheFormAndSavePlease, MsgNewDivision
      , MsgEditDivision, MsgFieldRequired, MsgNewSubdivision, MsgExpertAssessment
      , MsgWeight, MsgApplicant, MsgCalculationAnalysis, MsgPhoto, MsgBreadcrumbs
      , MsgCandidates, MsgCandidate
      )
    )

import Model
    ( Job (Job, jobCode, jobName, jobDayStart, jobDayEnd, jobDescr, jobDept)
    , JobId, JobSkillId, Skill (Skill), JobSkill (JobSkill)
    , Params (Params), Dept (Dept)
    , ApplicantId, Applicant (Applicant), AppSkill (AppSkill)
    , EntityField
      ( JobId, SkillId, JobSkillJob, JobSkillSkill , JobSkillParent
      , JobSkillId, JobSkillWeight, JobCode , JobName, JobDescr
      , JobDayStart, JobDayEnd, DeptName, DeptId, JobSkillExpanded
      , JobDept, AppSkillSkill, ApplicantId, AppSkillApplicant
      )
    )
  
import Settings (widgetFile)

import Text.Julius (rawJS)
import Text.Read (readMaybe)
import Tree (Tree (Node))

import Widgets (thSort, thSortDir)

import Yesod (YesodPersist(runDB))
import Yesod.Core
    ( Yesod(defaultLayout)
    , Html, WidgetFor, whamlet, SomeMessage (SomeMessage)
    , MonadIO (liftIO), setTitleI, getRequest
    , YesodRequest (reqGetParams), languages, MonadTrans (lift)
    , newIdent, lookupGetParams, getMessages, addMessageI
    )
import Yesod.Core.Content (TypedContent)
import Yesod.Core.Handler
    ( HandlerFor, selectRep, provideRep, redirect
    , getUrlRenderParams
    )
import Yesod.Form.Fields
    ( Textarea (Textarea), selectFieldList, intField, doubleField
    , urlField, boolField, textField, dayField, textareaField
    )
import Yesod.Form.Functions (mreq, mopt, generateFormPost, runFormPost, checkM)
import Yesod.Form.Input (runInputGet, iopt, ireq, runInputPost)
import Yesod.Form.Types
    ( MForm, FormResult (FormSuccess)
    , FieldView (fvInput, fvId, fvLabel, fvErrors, fvRequired)
    , FieldSettings (FieldSettings, fsLabel, fsAttrs, fsName, fsId, fsTooltip)
    )


getJobCandidateR :: JobId -> ApplicantId -> HandlerFor App TypedContent
getJobCandidateR jid aid = selectRep $ provideRep $ defaultLayout $ do
    stati <- filter ((/= "tab") . fst) . reqGetParams <$> getRequest
    rndr <- getUrlRenderParams
    setTitleI MsgCandidate
    $(widgetFile "jobs/candidate")


getJobCandidatesR :: JobId -> HandlerFor App TypedContent
getJobCandidatesR jid = do
    stati <- filter ((/= "tab") . fst) . reqGetParams <$> getRequest
    
    job <- do
        mjob <- runDB $ selectOne $ do
            j <- from $ table @Job
            where_ $ j ^. JobId ==. val jid
            return j
        case mjob of
          Just job -> do
              skillTree <- bldTree <$> runDB (fetchJobSkills jid)
              return $ Just (job, skillTree)
          Nothing -> return Nothing

    applicants <- runDB $ select $ do
        x <- from $ table @Applicant
        where_ $ exists $ do
            (as :& js) <- from $ table @AppSkill
                `innerJoin` table @JobSkill `on` (\(as :& js) -> as ^. AppSkillSkill ==. js ^. JobSkillSkill)
            where_ $ js ^. JobSkillJob ==. val jid
            where_ $ as ^. AppSkillApplicant ==. x ^. ApplicantId

        return x

    appSkills <- mapM ( \e@(Entity aid _) -> (e,) <$> runDB ( select $ do 
            x <- from $ table @AppSkill
            where_ $ x ^. AppSkillApplicant ==. val aid
            where_ $ exists $ do
                js <- from $ table @JobSkill
                where_ $ js ^. JobSkillSkill ==. x ^. AppSkillSkill
                where_ $ js ^. JobSkillJob ==. val jid
            return x ) ) applicants

    loc <- Locale . unpack . fromMaybe "en" . LS.head <$> languages
    selectRep $ provideRep $ defaultLayout $ do
        setTitleI MsgCandidates
        $(widgetFile "jobs/candidates")


putJobSkillR :: JobId -> JobSkillId -> HandlerFor App ()
putJobSkillR jid jsid = do
    stati <- reqGetParams <$> getRequest
    expanded <- runInputPost $ ireq boolField "expanded"
    runDB $ P.update jsid [JobSkillExpanded P.=. expanded]
    redirect (JobSkillsEditFormR jid, stati)


deleteJobSkillR :: JobId -> JobSkillId -> HandlerFor App ()
deleteJobSkillR jid jsid = do
    stati <- reqGetParams <$> getRequest
    runDB $ delete jsid
    addMessageI "alert-info toast" MsgRecordDeleted
    redirect (JobSkillsEditFormR jid, stati)


postJobSkillsR :: JobId -> HandlerFor App TypedContent
postJobSkillsR jid = do
    (sid,w,p,l) <- runInputPost $ ((,,,) . toSqlKey <$> ireq intField "skill")
        <*> ireq doubleField "weight"
        <*> ((toSqlKey <$>) <$> iopt intField "parent")
        <*> ireq urlField "location"

    mJobSkill <- runDB $ selectOne $ do
        x <- from $ table @JobSkill
        where_ $ x ^. JobSkillJob ==. val jid &&. x ^. JobSkillSkill ==. val sid

    case mJobSkill of
      Nothing -> do
          runDB $ insert_ $ JobSkill jid sid w p True
          addMessageI "alert-info toast" MsgRecordAdded
      Just _ -> addMessageI "alert-danger tab-1" MsgJobSkillAlreadyExists
    redirect l


deleteJobR :: JobId -> HandlerFor App ()
deleteJobR jid = do
    runDB $ delete jid
    addMessageI "alert-info toast" MsgRecordDeleted


postJobR :: JobId -> HandlerFor App TypedContent
postJobR jid = selectRep $ provideRep $ do
    stati <- filter ((/= "tab") . fst) . reqGetParams <$> getRequest
    job <- runDB $ selectOne $ do
        x <- from $ table @Job
        where_ $ x ^. JobId ==. val jid
        return x
      
    ((fr,widget),enctype) <- runFormPost $ formJob job True
    
    case fr of
      FormSuccess r -> do
          runDB $ replace jid r
          addMessageI "alert-info toast" MsgRecordEdited 
          redirect (JobsR,stati)
          
      _otherwise -> do
          selected <- fromMaybe [] . mapM ((toSqlKey <$>) . readMaybe . unpack) <$> lookupGetParams "jsid"
          tab <- fromMaybe 0 <$> runInputGet (iopt intField "tab") :: HandlerFor App Int
          addMessageI "alert-danger tab-0" MsgInvalidFormData
          skills <- runDB $ fetchJobSkills jid :: HandlerFor App [(Entity JobSkill, Entity Skill)]
          let trees = bldTree skills
          skillPool <- runDB $ fetchJoblessSkills (snd <$> skills)
          location <- getUrlRenderParams >>= \rndr -> return $ rndr (JobEditFormR jid) [("tab","1")]
          
          (fw,fe) <- generateFormPost $ formSkills stati trees (fmtDbl ".######" (Locale "en")) selected

          when (any (> 1) (weightLevels trees)) $ addMessageI "alert-warning tab-1" MsgCumulativeWeightNotNormal

          loc <- maybe "en" (Locale . unpack) . LS.head <$> languages
          unless (null selected) $ addMessageI "alert-info tab-1"
              (MsgTotalValue (fmtDbl ".######" loc $ calcLevelWeight selected trees))
            
          msgs <- getMessages
          defaultLayout $ do
              setTitleI MsgEditPosition
              $(widgetFile "jobs/edit")


getJobR :: JobId -> Handler TypedContent
getJobR jid = do
    stati <- filter ((/= "tab") . fst) . reqGetParams <$> getRequest
    tab <- runInputGet $ iopt intField "tab" :: HandlerFor App (Maybe Int)

    mjob <- runDB $ selectOne $ do
        (j :& d) <- from $ table @Job
            `leftJoin` table @Dept `on` (\(j :& d) -> j ^. JobDept ==. d ?. DeptId)
        where_ $ j ^. JobId ==. val jid
        return (j,d)

    nSkills <- runDB $ fetchJobSkillN jid
    nCandidates <- fetchNumCandidates jid
    trees <- bldTree <$> runDB (fetchJobSkills jid)
    loc <- Locale . unpack . fromMaybe "en" . LS.head <$> languages
    selectRep $ provideRep $ defaultLayout $ do
        setTitleI MsgPosition
        $(widgetFile "jobs/job")


fmtDbl :: Text -> LocaleName -> Double -> Text
fmtDbl = formatDouble'


postJobsR :: HandlerFor App TypedContent
postJobsR = do
    stati <- reqGetParams <$> getRequest
    ((r,widget),enctype) <- runFormPost $ formJob Nothing True
    selectRep $ provideRep $ do
        case r of
          FormSuccess x -> do
              runDB $ insert_ x
              addMessageI "alert-info toast" MsgRecordAdded
              redirect (JobsR,stati)
              
          _otherwise -> defaultLayout $ do
              addMessageI "alert-danger" MsgInvalidFormData
              msgs <- getMessages
              setTitleI MsgCreatePosition
              $(widgetFile "jobs/create")


getJobsR :: Handler TypedContent
getJobsR = do
    stati <- reqGetParams <$> getRequest
    params@(Params mq moffset mlimit msort labels) <- do
        params <- reqGetParams <$> getRequest
        return $ Params
            (snd <$> LS.head (filter (\(x,y) -> x == "q" && not (T.null y)) params))
            (readMaybe . unpack . snd =<< LS.head (filter ((== "offset") . fst) params))
            (readMaybe . unpack . snd =<< LS.head (filter ((== "limit") . fst) params))
            (LS.head (filter ((\x -> x == "asc" || x == "desc") . fst) params))
            (snd <$> filter (\(x,y) -> x == "label" && not (T.null y)) params)

    rcnt <- runDB $ fetchJobCount params
    jobs <- do
        jobs <- runDB $ fetchJobs params :: HandlerFor App [(Entity Job, Value Int)]
        forM jobs (\(j@(Entity jid _), n) -> fetchNumCandidates jid >>= \m -> return (j,n,m))

    let maxo = fromMaybe 0 $ (*)
            <$> (((+) . div rcnt <$> mlimit) <*> ((\x -> if x > 0 then 0 else -1) . mod rcnt <$> mlimit))
            <*> mlimit
    let prev = fromMaybe 0 $ max <$> ((-) <$> moffset <*> mlimit) <*> pure 0
    let next = fromMaybe 0 $ min <$> ((+) <$> moffset <*> mlimit) <*> pure maxo
    let start = maybe 0 (\x -> if maxo < 0 then 0 else x + 1) moffset
    let end = fromMaybe 0 $ min <$> ((+) <$> moffset <*> mlimit) <*> pure rcnt

    depts <- runDB $ select $ from $ table @Dept
    
    msgs <- getMessages
    selectRep $ provideRep $ defaultLayout $ do
        setTitleI MsgPositions
        loc <- Locale . unpack . fromMaybe "en" . LS.head <$> languages
        fmt <- liftIO $ standardDateFormatter NoFormatStyle ShortFormatStyle loc "GMT+0300"
        cal <- liftIO $ calendar "GMT+0300" loc TraditionalCalendarType
        idFormSearch <- newIdent
        idInputSearch <- newIdent
        idSelectLimit <- newIdent
        idSelectLimit2 <- newIdent
        $(widgetFile "jobs/jobs")


postJobSkillsEditR :: JobId -> HandlerFor App TypedContent
postJobSkillsEditR jid = do
    stati <- filter ((/= "jsid") . fst) . reqGetParams <$> getRequest
    selected <- fromMaybe [] . mapM ((toSqlKey <$>) . readMaybe . unpack) <$> lookupGetParams "jsid"
    location <- runInputPost $ ireq urlField "location"
    skills <- runDB $ fetchJobSkills jid :: Handler [(Entity JobSkill, Entity Skill)]
    let trees = bldTree skills
    skillPool <- runDB $ fetchJoblessSkills (snd <$> skills)
    ((fr,fw),fe) <- runFormPost $ formSkills stati trees (fmtDbl ".######" (Locale "en")) selected
    msgs <- getMessages
    job <- runDB $ get jid
    
    case fr of
      FormSuccess xs -> do
          forM_ xs (\(jsid,weight) -> runDB $ update $ \x -> do
                         set x [JobSkillWeight =. val weight]
                         where_ $ x ^. JobSkillId ==. val jsid
                   )
          addMessageI "alert-info toast" MsgRecordEdited
          redirect (JobSkillsEditFormR jid,stati)
        
      _otherwise -> selectRep $ provideRep $ defaultLayout $ do
          setTitleI MsgEditSkills
          $(widgetFile "jobs/skills")


getJobSkillsEditFormR :: JobId -> HandlerFor App Html
getJobSkillsEditFormR jid = do
    stati <- filter ((/= "jsid") . fst) . reqGetParams <$> getRequest
    selected <- fromMaybe [] . mapM ((toSqlKey <$>) . readMaybe . unpack) <$> lookupGetParams "jsid"
    job <- runDB $ get jid
    skills <- runDB $ fetchJobSkills jid :: Handler [(Entity JobSkill, Entity Skill)]
    let trees = bldTree skills
    skillPool <- runDB $ fetchJoblessSkills (snd <$> skills)
    location <- getUrlRenderParams >>= \rndr -> return $ rndr (JobSkillsEditFormR jid) stati
    
    (fw,fe) <- generateFormPost $ formSkills stati trees (fmtDbl ".######" (Locale "en")) selected

    when (any (> 1) (weightLevels trees)) $ addMessageI "alert-warning" MsgCumulativeWeightNotNormal
    loc <- maybe "en" (Locale . unpack) . LS.head <$> languages
    unless (null selected) $ addMessageI "alert-info"
        (MsgTotalValue (fmtDbl ".######" loc $ calcLevelWeight selected trees))
        
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgEditSkills
        $(widgetFile "jobs/skills")


calcLevelWeight :: [JobSkillId] -> [Tree (Entity JobSkill, Entity Skill)] -> Double
calcLevelWeight _ [] = 0
calcLevelWeight jsids ((Node (Entity jsid (JobSkill _ _ w _ _),_) []):rs)
    | jsid `elem` jsids = w + calcLevelWeight jsids rs
    | otherwise = calcLevelWeight jsids rs
calcLevelWeight jsids ((Node (Entity jsid (JobSkill _ _ w _ _),_) cs):rs)
    | (jsid `elem` jsids) && hasSelectedChildren jsids cs = (w * calcLevelWeight jsids cs) + calcLevelWeight jsids rs
    | (jsid `elem` jsids) && not (hasSelectedChildren jsids cs) = w + calcLevelWeight jsids rs
    | otherwise = calcLevelWeight jsids cs + calcLevelWeight jsids rs

  where
    hasSelectedChildren :: [JobSkillId] -> [Tree (Entity JobSkill, Entity Skill)] -> Bool
    hasSelectedChildren xs ys = not (null (xs `intersect` extractJsids ys))

    intersect :: [JobSkillId] -> [JobSkillId] -> [JobSkillId]
    intersect [] = const []
    intersect xs = filter (`elem` xs)

    extractJsids :: [Tree (Entity JobSkill, Entity Skill)] -> [JobSkillId]
    extractJsids = map (\(Node (Entity jobSkillId _,_) _) -> jobSkillId)


weightLevels :: [Tree (Entity JobSkill, Entity Skill)] -> [Double]
weightLevels trees =
    sum ((\(Node (Entity _ (JobSkill _ _ w _ _),_) _) -> w) <$> trees) : concatMap weightTree trees


weightTree :: Tree (Entity JobSkill, Entity Skill) -> [Double]
weightTree (Node _ []) = [0]
weightTree (Node _ xs@(c:_)) =
    sum ((\(Node (Entity _ (JobSkill _ _ w _ _),_) _) -> w) <$> xs) : weightTree c


getJobEditFormR :: JobId -> HandlerFor App Html
getJobEditFormR jid = do
    stati <- filter ((/= "tab") . fst) . reqGetParams <$> getRequest
    selected <- fromMaybe [] . mapM ((toSqlKey <$>) . readMaybe . unpack) <$> lookupGetParams "jsid"
    tab <- fromMaybe 0 <$> runInputGet (iopt intField "tab") :: HandlerFor App Int
    
    job <- runDB $ selectOne $ do
        x <- from $ table @Job
        where_ $ x ^. JobId ==. val jid
        return x
      
    skills <- runDB $ fetchJobSkills jid :: HandlerFor App [(Entity JobSkill, Entity Skill)]
    let trees = bldTree skills
    skillPool <- runDB $ fetchJoblessSkills (snd <$> skills)
    (widget,enctype) <- generateFormPost $ formJob job False
    location <- getUrlRenderParams >>= \rndr -> return $ rndr (JobEditFormR jid) (("tab","1") : stati)
    (fw,fe) <- generateFormPost $ formSkills stati trees (fmtDbl ".######" (Locale "en")) selected
    when (any (> 1) (weightLevels trees)) $ addMessageI "alert-warning tab-1" MsgCumulativeWeightNotNormal
    loc <- maybe "en" (Locale . unpack) . LS.head <$> languages
    unless (null selected) $ addMessageI "alert-info tab-1"
        (MsgTotalValue (fmtDbl ".######" loc $ calcLevelWeight selected trees))
        
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgEditPosition
        $(widgetFile "jobs/edit")


getJobCreateFormR :: HandlerFor App Html
getJobCreateFormR = do
    stati <- reqGetParams <$> getRequest
    (widget,enctype) <- generateFormPost $ formJob Nothing False
    msgs <- getMessages
    defaultLayout $ do
        setTitleI MsgCreatePosition
        $(widgetFile "jobs/create")


formJob :: Maybe (Entity Job) -> Bool -> Form Job
formJob job isPost extra = do
    (codeR, codeV) <- mreq uniqueTextField (fs MsgCode) (jobCode . entityVal <$> job)
    (nameR, nameV) <- mreq textField (fs MsgName) (jobName . entityVal <$> job)
    (dayStartR, dayStartV) <- mreq dayField (fs MsgDayStart) (jobDayStart . entityVal <$> job)
    (dayEndR, dayEndV) <- mreq (afterDayField dayStartR) (fs MsgDayEnd) (jobDayEnd . entityVal <$> job)
    (descrR, descrV) <- mopt textareaField (fs MsgDescription) (jobDescr . entityVal <$> job)

    depts <- lift $ runDB $ selectList [] [Asc DeptName]
    (deptR,deptV) <- mopt
        (selectFieldList $ (\(Entity ident (Dept name _)) -> (name,ident)) <$> depts)
        (fs MsgDivision)
        (jobDept . entityVal <$> job)

    let r = Job <$> codeR <*> nameR <*> dayStartR <*> dayEndR <*> descrR <*> deptR
    let w = [whamlet|
                    #{extra}
                    <div.d-flex.flex-column.gap-2>
                      $forall v <- [codeV,nameV,dayStartV,dayEndV,descrV,deptV]
                        <div :isJust (fvErrors v):.is-invalid
                             :not (isJust (fvErrors v)) && isPost:.is-valid>
                          <label.form-label.mb-0.ps-1 for=#{fvId v}>
                            #{fvLabel v}
                            $if fvRequired v
                              <sup>*
                          ^{fvInput v}
                          $maybe errs <- fvErrors v
                            <div.invalid-feedback>
                              #{errs}
                    |]
    return (r,w)
  where
      
      fs :: AppMessage -> FieldSettings App
      fs lbl = FieldSettings (SomeMessage lbl) Nothing Nothing Nothing [("class","form-control")]
    
      afterDayField (FormSuccess x) = checkM (afterDay x) dayField
      afterDayField _ = dayField

      afterDay :: Day -> Day -> HandlerFor App (Either AppMessage Day)
      afterDay x y = return $ if y < x then Left MsgInvalidEndDay else Right y

      uniqueTextField = checkM checkUniqueCode textField

      checkUniqueCode :: Text -> HandlerFor App (Either AppMessage Text)
      checkUniqueCode code = do
          isDup <- (isJust <$>) <$> runDB $ selectOne $ do
              x <- from $ table @Job
              where_ $ x ^. JobCode ==. val code
              case job of
                Just (Entity jid _) -> where_ $ not_ (x ^. JobId ==. val jid)
                Nothing -> return ()
              return x
          return $ if isDup then Left MsgDuplicateCode else Right code


fetchJobSkillN :: MonadIO m => JobId -> ReaderT SqlBackend m Int
fetchJobSkillN jid =  maybe 0 unValue <$> selectOne (queryJobSkillN jid)


queryJobSkillN :: JobId -> SqlQuery (SqlExpr (Value Int))
queryJobSkillN jid = do
    j <- from $ table @JobSkill
    where_ $ j ^. JobSkillJob ==. val jid
    where_ $ not_ $ exists $ do
      j' <- from $ table @JobSkill
      where_ $ just (j ^. JobSkillId) ==. j' ^. JobSkillParent
    return countRows


fetchJobSkills :: MonadIO m => JobId -> ReaderT SqlBackend m [(Entity JobSkill, Entity Skill)]
fetchJobSkills ident = select $ queryJobSkills ident


queryJobSkills :: JobId -> SqlQuery (SqlExpr (Entity JobSkill), SqlExpr (Entity Skill))
queryJobSkills ident = do
    cte <- withRecursive
      (do
          s <- from $ table @JobSkill
          where_ $ s ^. JobSkillJob ==. val ident
          where_ $ isNothing $ s ^. JobSkillParent
          return s
      )
      unionAll_
      (\parent -> do
          (c :& _) <- from $  table @JobSkill `innerJoin` parent
            `on` (\(c :& p) -> c ^. JobSkillParent ==. just (p ^. JobSkillId))
          return c
      )
    (h :& s) <- from $ table @Skill `innerJoin` cte `on` (\(s :& h) -> s ^. SkillId ==. h ^. JobSkillSkill)
    return (s,h)


fetchJoblessSkills :: MonadIO m => [Entity Skill] -> ReaderT SqlBackend m [Entity Skill]
fetchJoblessSkills js = select $ queryJoblessSkills js


queryJoblessSkills :: [Entity Skill] -> SqlQuery (SqlExpr (Entity Skill))
queryJoblessSkills js = do
  x <- from $ table @Skill
  where_ $ x ^. SkillId `notIn` valList (entityKey <$> js)
  pure x


fetchJobs :: MonadIO m => Params -> ReaderT SqlBackend m [(Entity Job, Value Int)]
fetchJobs params = select $ queryJobs params


queryJobs :: Params -> SqlQuery (SqlExpr (Entity Job), SqlExpr (Value Int))
queryJobs (Params mq moffset mlimit msort lbls) = do
    (x :& (_,cnt) :& d) <- from $ table @Job
        `leftJoin` ( selectQuery $ do
                       s <- from $ table @JobSkill
                       where_ $ not_ $ exists $ do
                         s' <- from $ table @JobSkill
                         where_ $ just (s ^. JobSkillId) ==. s' ^. JobSkillParent
                       groupBy (s ^. JobSkillJob)
                       return (s ^. JobSkillJob, countRows :: SqlExpr (Value Int))
                   ) `on` (\(x :& (sjid,_)) -> sjid ==. just (x ^. JobId))
        `leftJoin` table @Dept `on` (\(x :& _ :& d) -> x ^. JobDept ==. d ?. DeptId)

    case mq of
      Just q -> where_ $ (upper_ (x ^. JobCode) `like` (%) ++. upper_ (val q) ++. (%))
          ||. (upper_ (x ^. JobName) `like` (%) ++. upper_ (val q) ++. (%))
          ||. (upper_ (x ^. JobDescr) `like` (%) ++. just (upper_ (val (Textarea q))) ++. (%))
      Nothing -> return ()

    case lbls of
      [] -> return ()
      xs -> where_ $ d ?. DeptName `in_` justList (valList xs)

    case moffset of
      Just n -> offset $ fromIntegral n
      Nothing -> return ()

    case mlimit of
      Just n -> limit $ fromIntegral n
      Nothing -> return ()

    let rcnt = coalesceDefault [cnt] (val 0)

    case msort of
      Just ("asc","id") -> orderBy [asc (x ^. JobId)]
      Just ("desc","id") -> orderBy [desc (x ^. JobId)]
      Just ("asc","code") -> orderBy [asc (x ^. JobCode)]
      Just ("desc","code") -> orderBy [desc (x ^. JobCode)]
      Just ("asc","name") -> orderBy [asc (x ^. JobName)]
      Just ("desc","name") -> orderBy [desc (x ^. JobName)]
      Just ("asc","dayStart") -> orderBy [asc (x ^. JobDayStart)]
      Just ("desc","dayStart") -> orderBy [desc (x ^. JobDayStart)]
      Just ("asc","dayEnd") -> orderBy [asc (x ^. JobDayEnd)]
      Just ("desc","dayEnd") -> orderBy [desc (x ^. JobDayEnd)]
      Just ("asc","descr") -> orderBy [asc (x ^. JobDescr)]
      Just ("desc","descr") -> orderBy [desc (x ^. JobDescr)]
      Just ("asc","skills") -> orderBy [asc rcnt]
      Just ("desc","skills") -> orderBy [desc rcnt]
      _otherwise -> return ()

    orderBy [desc (x ^. JobId)]
    return (x, rcnt)


fetchJobCount :: MonadIO m => Params -> ReaderT SqlBackend m Int
fetchJobCount params = maybe 0 unValue <$> selectOne (queryJobCount params)


queryJobCount :: Params -> SqlQuery (SqlExpr (Value Int))
queryJobCount (Params mq _ _ _ lbls) = do
    (x :& d) <- from $ table @Job
        `leftJoin` table @Dept `on` (\(x :& d) -> x ^. JobDept ==. d ?. DeptId)
    case mq of
      Just q -> where_ $ (upper_ (x ^. JobCode) `like` (%) ++. upper_ (val q) ++. (%))
        ||. (upper_ (x ^. JobName) `like` (%) ++. upper_ (val q) ++. (%))
        ||. (upper_ (x ^. JobDescr) `like` (%) ++. just (upper_ (val (Textarea q))) ++. (%))
      Nothing -> return ()

    case lbls of
      [] -> return ()
      xs -> where_ $ d ?. DeptName `in_` justList (valList xs)

    return countRows


fmtDay :: Day -> Text
fmtDay = pack . formatTime defaultTimeLocale "%d.%m.%Y"


bldTree :: [(Entity JobSkill, Entity Skill)] -> [Tree (Entity JobSkill, Entity Skill)]
bldTree [] = []
bldTree xs = go (sortBy compareJobSkillId (filter root xs)) xs
  where
    child :: JobSkillId -> (Entity JobSkill, Entity Skill) -> Bool
    child parent (Entity _ (JobSkill _ _ _ par _), _) = Just parent == par

    root :: (Entity JobSkill, Entity Skill) -> Bool
    root (Entity _ (JobSkill _ _ _ Nothing _), _) = True
    root _ = False

    compareJobSkillId :: (Entity JobSkill, Entity Skill) -> (Entity JobSkill, Entity Skill) -> Ordering
    compareJobSkillId (Entity jsid _, _) (Entity jsid' _, _) = jsid `compare` jsid'

    go :: [(Entity JobSkill, Entity Skill)] -> [(Entity JobSkill, Entity Skill)] -> [Tree (Entity JobSkill, Entity Skill)]
    go [] _ = []
    go ((js@(Entity p _), se) : ys) as =
      Node (js,se) (go (sortBy compareJobSkillId (filter (child p) as)) as) : go ys as


rndrTree :: [(Text,Text)] -> (Double -> Text) -> [Tree (Entity JobSkill, Entity Skill)] -> WidgetFor App ()
rndrTree _ _ [] = return ()
rndrTree _ fmt [Node (Entity _ (JobSkill _ _ weight _ _), Entity _ (Skill code name _ _)) []] =
    [whamlet|
            <li.mb-1 role=treeitem>
              <div.d-flex.flex-row.justify-content-start.align-items-center>
                <button.invisible.btn.btn-light.rounded-circle.me-1 disabled>
                  <i.bi.bi-dot>
                <span.badge.text-bg-secondary.me-1>#{fmt weight}
                <span.lh-1 title=#{name}>#{code}
            |]
    
rndrTree stati fmt (Node (Entity _ (JobSkill _ _ weight _ _), Entity _ (Skill code name _ _)) []:ns) =
    [whamlet|
            <li.mb-1 role=treeitem>
              <div.d-flex.flex-row.justify-content-start.align-items-center>
                <button.invisible.btn.btn-light.rounded-circle.me-1 disabled>
                  <i.bi.bi-dot>
                <span.badge.text-bg-secondary.me-1>#{fmt weight}
                <span.lh-1 title=#{name}>#{code}
            ^{rndrTree stati fmt ns}
            |]
    
rndrTree stati fmt (Node (Entity jsid (JobSkill jid _ weight _ expanded), Entity _ (Skill code name _ _)) xs:ns) =
    [whamlet|
            <li.mb-1 role=treeitem>
              <div.d-flex.flex-row.justify-content-start.align-items-center>
                $if not (null xs)
                  <div>
                    <button.btn.btn-light.rounded-circle.me-1.chevron type=button
                      data-url=@?{(JobSkillR jid jsid,stati)}
                      data-bs-toggle=collapse data-bs-target=#ulCollapse#{fromSqlKey jsid}
                      :expanded:aria-expanded=true :not expanded:aria-expanded=false
                      aria-controls=ulCollapse#{fromSqlKey jsid} aria-label=_{MsgExpand}>
                <span.badge.text-bg-secondary.me-1>#{fmt weight}
                <span.lh-1 title=#{name}>#{code}
              <ul.collapse :expanded:.show #ulCollapse#{fromSqlKey jsid} role=group>
                ^{rndrTree stati fmt xs}
            ^{rndrTree stati fmt ns}
            |]


formSkills :: [(Text,Text)] -> [Tree (Entity JobSkill, Entity Skill)]
           -> (Double -> Text)
           -> [JobSkillId]
           -> Form [(JobSkillId, Double)]
formSkills stati tree fmt selected extra = do
    (r,w') <- treeSkills stati tree fmt selected
    let w = [whamlet|
                    #{extra}
                    <ul role=tree>
                      ^{w'}
                    |]
    return (r,w)


treeSkills :: [(Text,Text)] -> [Tree (Entity JobSkill, Entity Skill)]
           -> (Double -> Text)
           -> [JobSkillId]
           -> MForm (HandlerFor App) (FormResult [(JobSkillId, Double)], WidgetFor App ())
treeSkills _ [] _ _ = return (FormSuccess [],[whamlet||])
treeSkills _ [Node (Entity jsid (JobSkill _ _ w _ _), Entity _ (Skill _ name _ _)) []] _ selected = do
    (r,v) <- first ((jsid,) <$>) <$> mreq doubleField FieldSettings
             { fsLabel = SomeMessage name
             , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
             , fsAttrs = [("class","form-control")]
             } (Just w)

    return ( (:) <$> r <*> FormSuccess []
           , [whamlet|
                     <li.pb-1 role=treeitem>
                       <div.d-flex.flex-row.justify-content-start.align-items-end>
                         <button.invisible.btn.btn-light.rounded-circle.border-0.me-1 disabled>
                           <i.bi.bi-dot>
                         ^{itemSkill jsid (elem jsid selected) v}
                     |]
           )
      
treeSkills stati (Node (Entity jsid (JobSkill _ _ weight _ _), Entity _ (Skill _ name _ _)) []:ns) fmt selected = do
    (r',w') <- treeSkills stati ns fmt selected :: MForm (HandlerFor App) (FormResult [(JobSkillId,Double)], WidgetFor App ())

    (r,v) <- first ((jsid,) <$>) <$> mreq doubleField FieldSettings
             { fsLabel = SomeMessage name
             , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
             , fsAttrs = [("class","form-control")]
             } (Just weight)

    let w = [whamlet|
                    <li.pb-1 role=treeitem>
                      <div.d-flex.flex-row.justify-content-start.align-items-end>
                        <button.invisible.btn.btn-light.rounded-circle.border-0.me-1 disabled>
                          <i.bi.bi-dot>
                        ^{itemSkill jsid (elem jsid selected) v}
                    ^{w'}
                    |]
    return ((:) <$> r <*> r', w)
    
treeSkills stati (Node (Entity jsid (JobSkill jid _ weight _ expanded), Entity _ (Skill _ name _ _)) xs:ns) fmt selected = do
    ident <- newIdent
    (r',w') <- treeSkills stati ns fmt selected
    (r'',w'') <- treeSkills stati xs fmt selected

    (r,v) <- first ((jsid,) <$>) <$> mreq doubleField FieldSettings
             { fsLabel = SomeMessage name
             , fsTooltip = Nothing, fsId = Nothing, fsName = Nothing
             , fsAttrs = [("class","form-control")]
             } (Just weight)

    let w = [whamlet|
                    <li.pb-1 role=treeitem>
                      <div.d-flex.flex-row.justify-content-start.align-items-end>
                        $if not (null xs)
                          <button.btn.btn-light.rounded-circle.me-1.chevron type=button
                            data-bs-toggle=collapse data-bs-target=#ulCollapse#{ident}
                            data-url=@?{(JobSkillR jid jsid,stati)}
                            :not expanded:aria-expanded=false :expanded:aria-expanded=true aria-controls=ulCollapse#{ident}
                            :not expanded:aria-label=_{MsgExpand} :expanded:aria-label=_{MsgCollapse}>
                          ^{itemSkill jsid (elem jsid selected) v}
                      <ul.collapse.tree :expanded:.show role=group #ulCollapse#{ident}>
                        ^{w''}
                    ^{w'}
                    |]
    return ((++) <$> ((:) <$> r <*> r') <*> r'',w)


itemSkill :: JobSkillId -> Bool -> FieldView App -> WidgetFor App ()
itemSkill jsid checked v = [whamlet|
    <div>
      <label.col-form-label.lh-1.m-0.p-0 for=#{fvId v}>
        <ps-1>#{fvLabel v}
      <div.d-flex.flex-row.flex-nowrap.justify-content-start.align-items-center.gap-1>
        <div.input-group>
          <div.input-group-text>
            <input.form-check-input.mt-0 type=checkbox name=jsid value=#{fromSqlKey jsid}
              :checked:checked form=formGetJobSkillWeights aria-label=_{MsgSelect}
              oninput=this.form.submit()>
          ^{fvInput v}
        <button.btn.btn-light.rounded-circle type=button
          data-bs-toggle=dropdown aria-expanded=false title=_{MsgActions}>
          <i.bi.bi-three-dots-vertical>
        <ul.dropdown-menu>
          <li>
            <button.dropdown-item type=button
              data-bs-toggle=modal data-bs-target=#modalChildSkills#{fromSqlKey jsid}>
              <i.bi.bi-plus-lg.me-2>
              _{MsgAdd}
          <li>
            <button.dropdown-item type=button
              data-bs-toggle=modal data-bs-target=#modalChildSkillsRemove#{fromSqlKey jsid}>
              <i.bi.bi-trash.me-2>
              _{MsgRemove}
    |]
