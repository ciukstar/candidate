{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module Handler.Candidates
  ( getCandidateR
  , getCandidatesR
  , fetchNumCandidates
  , calcWeights, gtZero, allLeafs, filterAppSkills, maxHeight, bldThead, candidateInfo
  ) where

import ClassyPrelude.Yesod (ReaderT, when)

import Data.List (sortBy)
import qualified Data.List.Safe as LS (head)
import Data.Maybe (fromMaybe, isJust)
import Data.Text (pack, unpack, Text)
import Data.Text.ICU
    ( LocaleName (Locale), FormatStyle (NoFormatStyle, ShortFormatStyle)
    , CalendarType (TraditionalCalendarType)
    , standardDateFormatter, calendar, formatCalendar
    )
import Data.Text.ICU.Calendar (setDay)
import Data.Text.ICU.NumberFormatter (formatDouble')
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (toGregorian)

import Database.Esqueleto.Experimental
    ( SqlQuery, SqlExpr, select, from, table, orderBy, desc
    , innerJoin, on, where_, val, in_, valList
    , (^.), (?.), (:&)((:&)), (==.), exists, selectOne, leftJoin
    )
import Database.Persist (Entity (Entity), entityVal, entityKey)
import Database.Persist.Sql (SqlBackend, fromSqlKey, toSqlKey)

import Foundation
    ( App
    , Route
      ( AppPhotoR, PhotoPlaceholderR, CandidatesR, CandidateR, HomeR
      )
    , AppMessage
      ( MsgCandidates, MsgSelectAPosition, MsgClose, MsgActions
      , MsgSelect, MsgCancel, MsgSelectApplicants, MsgApplicants
      , MsgCandidate, MsgWeight, MsgFullName
      , MsgHome, MsgBreadcrumbs, MsgCalculationAnalysis
      , MsgSearch, MsgPositions, MsgPosition, MsgExpertAssessment
      , MsgTop, MsgShowTop, MsgAge, MsgApplicant, MsgBirthday
      , MsgPhoto, MsgCode, MsgDenom, MsgDayStart, MsgDayEnd
      , MsgDescription, MsgDivision, MsgTags, MsgBack, MsgToggle
      , MsgSelectPositionToRankCandidates
      )
    )

import Model
  ( ultDestKey, Dept (Dept), Job (Job), JobId, SkillId, Skill (Skill)
  , JobSkill (JobSkill)
  , Applicant (Applicant), ApplicantId, AppSkill (AppSkill, appSkillWeight)
  , EntityField
    ( JobId, SkillId, JobSkillSkill, JobSkillJob, ApplicantId
    , AppSkillApplicant, AppSkillSkill, JobDept, DeptId
    )
  )

import Settings (widgetFile)

import Text.Shakespeare.I18N (renderMessage, Lang)
import Tree ( Tree(..), leafs, width, height, children)

import Yesod (YesodPersist(runDB))
import Yesod.Core
    ( Yesod(defaultLayout), MonadIO (liftIO), WidgetFor, whamlet, setTitleI
    , lookupSession, getUrlRender, setUltDestCurrent
    , MonadHandler (liftHandler), ToWidget (toWidget), julius, newIdent
    )
import Yesod.Core.Content (TypedContent)
import Yesod.Core.Handler
    ( HandlerFor, selectRep, provideRep, languages, getRequest
    , YesodRequest (reqGetParams), getYesod
    )
import Yesod.Form (unTextarea)
import Yesod.Form.Input (runInputGet, iopt)
import qualified Yesod.Form.Fields as YF (intField)


getCandidatesR :: HandlerFor App TypedContent
getCandidatesR = do
    (mjid,limit) <- runInputGet $ ((,) . (toSqlKey <$>) <$> iopt YF.intField "job")
        <*> (fromMaybe 5 <$> iopt YF.intField "limit")

    aids <- (toSqlKey . read . unpack . snd <$>) . filter ((== "app") . fst) . reqGetParams <$> getRequest

    job <- case mjid of
      Just jid -> do
          applicants <- runDB $ fetchSkilledApplicants jid aids
          appSkills <- mapM (\e@(Entity aid _) -> (e,) <$> runDB (fetchAppSkillsForJob aid jid)) applicants
          mjob <- runDB $ selectOne $ do
              j <- from $ table @Job
              where_ $ j ^. JobId ==. val jid
              return j
          case mjob of
            Just job -> do
                skillTree <- bldTree <$> runDB (fetchJobSkills jid)
                return $ Just ((job, skillTree), appSkills)
            _otherwise -> return Nothing
      _otherwise -> return Nothing

    applicants <- runDB $ select $ do
        x <- from $ table @Applicant
        orderBy [desc (x ^. ApplicantId)]
        return x
        
    jobs <- runDB $ select $ do
        x <- from $ table @Job
        orderBy [desc (x ^. JobId)]
        return x
        
    loc <- Locale . unpack . fromMaybe "en" . LS.head <$> languages
    selectRep $ provideRep $ defaultLayout $ do
        setTitleI MsgCandidate
        setUltDestCurrent
        idModalPositions <- newIdent
        idInputSearchJobs <- newIdent
        idModalApplicants <- newIdent
        idInputSearchApplicants <- newIdent
        idFormGetCandidates <- newIdent
        idSelectTopCandidates <- newIdent
        idSelectLimit <- newIdent
        idSelectTop <- newIdent
        idJobName <- newIdent
        idBadgeApplicants <- newIdent
        idListCandidate <- newIdent
        when (isJust job) $ toWidget
            [julius|
                   [ [#{idSelectLimit},#{idSelectTop},#{idSelectTopCandidates}],
                     [#{idSelectTop},#{idSelectTopCandidates},#{idSelectLimit}],
                     [#{idSelectTopCandidates},#{idSelectLimit},#{idSelectTop}]
                   ].map(
                     ([x,y,z]) => [document.getElementById(x),document.getElementById(y),document.getElementById(z)]
                   ).forEach(([x,y,z]) => {
                     x.addEventListener('change', e => {
                       y.value = e.target.value;
                       z.value = e.target.value;
                       document.getElementById(#{idFormGetCandidates}).submit();
                     });
                   });
                   |]
        $(widgetFile "candidates/candidates")


getCandidateR :: JobId -> ApplicantId -> HandlerFor App TypedContent
getCandidateR jid aid = selectRep $ provideRep $ defaultLayout $ do
    setTitleI MsgCandidate
    ult <- getUrlRender >>= \rndr -> fromMaybe (rndr CandidatesR) <$> lookupSession ultDestKey
    $(widgetFile "candidates/candidate")


candidateInfo :: Text -> JobId -> ApplicantId -> WidgetFor App ()
candidateInfo ult jid aid = do
    mtab <- runInputGet $ iopt YF.intField "tab" :: WidgetFor App (Maybe Int)
    app <- liftHandler getYesod
    mjob <- liftHandler $ runDB $ selectOne $ do
        (x :& d) <- from $ table @Job
            `leftJoin` table @Dept `on` (\(x :& d) -> x ^. JobDept ==. d ?. DeptId)
        where_ $ x ^. JobId ==. val jid
        return (x,d)

    mapp <- liftHandler $ runDB $ selectOne $ do
        x <- from $ table @Applicant
        where_ $ x ^. ApplicantId ==. val aid
        return x

    langs <- languages
    let loc = Locale . unpack . fromMaybe "en" . LS.head $ langs 
    cal <- liftIO $ calendar "GMT+0300" loc TraditionalCalendarType
    fmtDay <- liftIO $ standardDateFormatter NoFormatStyle ShortFormatStyle loc "GMT+0300"
    appSkills <- liftHandler $ runDB $ fetchAppSkillsForJob aid jid
    skillTree <- liftHandler $ bldTree <$> runDB (fetchJobSkills jid)
    mage <- age mapp
    let weight = calcWeight appSkills skillTree

    idCollapseAnalysis <- newIdent
    $(widgetFile "candidates/candidate-info")

  where
      age :: MonadIO m => Maybe (Entity Applicant) -> m (Maybe Integer)
      age (Just (Entity _ (Applicant _ _ _ bday _))) = do
          (y,_,_) <- liftIO $ toGregorian . utctDay <$> getCurrentTime
          return $ (\(y',_,_) -> y - y') . toGregorian <$> bday
      age Nothing = return Nothing


rndrExpr :: (Double -> Text) -> [Tree (Text,Double,Text,Text)] -> Text
rndrExpr _ [] = ""
rndrExpr fmt [Node (_,w,_,_) []] = fmt w
rndrExpr fmt (Node (_,w,_,_) cs:ns)
    | length cs > 1 = fmt w <> " * (" <> rndrExpr fmt cs <> ")" <> (if null ns then "" else " + ") <> rndrExpr fmt ns
    | otherwise     = fmt w <> " * " <> rndrExpr fmt cs <> (if null ns then "" else " + ") <> rndrExpr fmt ns


rndrTree :: (Double -> Text) -> [Tree (Text,Double,Text,Text)] -> WidgetFor App ()
rndrTree _ [] = return ()

rndrTree fmt [Node (_,weight,code,name) []] = [whamlet|
    <li.mb-1 role=treeitem>
      <div.d-flex.flex-row.justify-content-start.align-items-center>
        <div>
          <button.invisible.btn.btn-light.rounded-circle.me-1 disabled>
            <i.bi.bi-dot>
        <span.badge.text-bg-secondary.me-1>#{fmt weight}
        <span.lh-1 title=#{code}>#{name}
    |]

rndrTree fmt (Node (_,weight,code,name) []:ns) = [whamlet|
    <li.mb-2 role=treeitem>
      <div.d-flex.flex-row.justify-content-start.align-items-center>
        <div>
          <button.invisible.btn.btn-light.rounded-circle.me-1 disabled>
            <i.bi.bi-dot>
        <span.badge.text-bg-secondary.me-1>#{fmt weight}
        <span.lh-1 title=#{code}>#{name}
    ^{rndrTree fmt ns}
    |]

rndrTree fmt (Node (eid,weight,code,name) xs:ns) = [whamlet|
    <li.mb-2 role=treeitem
      data-bs-toggle=collapse data-bs-target=#ulCollapse#{eid}
      aria-expanded=false aria-controls=ulCollapse#{eid}>

      <div.d-flex.flex-row.justify-content-start.align-items-center>
        $if not (null xs)
          <div>
            <button.btn.btn-light.rounded-circle.me-1.chevron
              type=button title=_{MsgToggle}
              data-bs-toggle=collapse data-bs-target=#ulCollapse#{eid}
              aria-expanded=false aria-controls=ulCollapse#{eid}>
        <div.d-flex.flex-row.gap-1.align-items-center>
          <span.badge.text-bg-secondary>#{fmt weight}
          <span.lh-1 title=#{code}>#{name}
      <ul.collapse #ulCollapse#{eid} role=group>
        ^{rndrTree fmt xs}
    ^{rndrTree fmt ns}
    |]


fetchAppSkillsForJob :: MonadIO m => ApplicantId -> JobId -> ReaderT SqlBackend m [Entity AppSkill]
fetchAppSkillsForJob aid jid = select $ queryAppSkills aid jid


queryAppSkills :: ApplicantId -> JobId -> SqlQuery (SqlExpr (Entity AppSkill))
queryAppSkills aid jid = do
    x <- from $ table @AppSkill
    where_ $ x ^. AppSkillApplicant ==. val aid
    where_ $ exists $ do
        js <- from $ table @JobSkill
        where_ $ js ^. JobSkillSkill ==. x ^. AppSkillSkill
        where_ $ js ^. JobSkillJob ==. val jid
    return x


fetchSkilledApplicants :: MonadIO m => JobId -> [ApplicantId] -> ReaderT SqlBackend m [Entity Applicant]
fetchSkilledApplicants jid aids = select $ querySkilledApplicants jid aids


querySkilledApplicants :: JobId -> [ApplicantId] -> SqlQuery (SqlExpr (Entity Applicant))
querySkilledApplicants jid aids = do
    a <- from $ table @Applicant
    where_ $ exists $ do
        (as :& js) <- from $ table @AppSkill
            `innerJoin` table @JobSkill `on` (\(as :& js) -> as ^. AppSkillSkill ==. js ^. JobSkillSkill)
        where_ $ js ^. JobSkillJob ==. val jid
        where_ $ as ^. AppSkillApplicant ==. a ^. ApplicantId

    case aids of
      [] -> return ()
      xs -> where_ $ a ^. ApplicantId `in_` valList xs

    return a


fetchApplicants :: MonadIO m => [ApplicantId] -> ReaderT SqlBackend m [Entity Applicant]
fetchApplicants = select . queryApplicants


queryApplicants :: [ApplicantId] -> SqlQuery (SqlExpr (Entity Applicant))
queryApplicants aids = do
  x <- from $ table @Applicant
  case aids of
    [] -> return ()
    xs -> where_ $ x ^. ApplicantId `in_` valList xs
  orderBy [desc (x ^. ApplicantId)]
  return x


fetchJobSkills :: MonadIO m => JobId -> ReaderT SqlBackend m [(Entity JobSkill,Entity Skill)]
fetchJobSkills jid = select $ queryJobSkills jid


queryJobSkills :: JobId -> SqlQuery (SqlExpr (Entity JobSkill), SqlExpr (Entity Skill))
queryJobSkills jid = do
  (js :& s) <- from $ table @JobSkill
    `innerJoin` table @Skill `on` (\(js :& s) -> js ^. JobSkillSkill ==. s ^. SkillId)
  where_ $ js ^. JobSkillJob ==. val jid
  return (js,s)


fetchNumCandidates :: JobId -> HandlerFor App Int
fetchNumCandidates jid = do
  mjob <- do
    mjob <- runDB $ selectOne $ do
      j <- from $ table @Job
      where_ $ j ^. JobId ==. val jid
      return j
    case mjob of
      Just job -> do
        skillTree <- bldTree <$> runDB (fetchJobSkills jid)
        return $ Just (job, skillTree)
      _ -> return Nothing

  case mjob of
    Just (Entity _ _, jobSkills) -> do
      applicants <- runDB $ fetchApplicants []

      -- appSkills :: [(Entity Applicant, [Entity AppSkill])]
      appSkills <- mapM (\e@(Entity aid _) -> (e,) <$> runDB (fetchAppSkillsForJob aid jid)) applicants
      return $ length $ filter (\(w,_,_) -> w > 0) (calcWeights appSkills jobSkills)
    Nothing -> return 0


gtZero :: (Double, Entity Applicant, [Entity AppSkill]) -> Bool
gtZero (w,_,_) | w > 0 = True
               | otherwise = False


calcWeights :: [(Entity Applicant, [Entity AppSkill])]
            -> [Tree (Entity JobSkill, Entity Skill)]
            -> [(Double, Entity Applicant, [Entity AppSkill])]
calcWeights apps js = sortBy
  (\(a,_,_) (b,_,_) -> b `compare` a)
  ((\(a,as) -> (calcWeight as js,a,as)) <$> apps)


calcWeight :: [Entity AppSkill] -> [Tree (Entity JobSkill, Entity Skill)] -> Double
calcWeight _ [] = 0
calcWeight as ((Node (Entity _ (JobSkill _ sid w _ _),_) []):rs) =
  maybe 0 ((*w) . appSkillWeight . entityVal) mAppSkill + calcWeight as rs
  where
    mAppSkill :: Maybe (Entity AppSkill)
    mAppSkill = LS.head (filter (\(Entity _ (AppSkill _ sid' _)) -> sid' == sid) as)
calcWeight as ((Node (Entity _ (JobSkill _ _ w _ _),_) cs):rs) =
  w * calcWeight as cs + calcWeight as rs


filterAppSkills :: SkillId -> [Entity AppSkill] -> Maybe (Entity AppSkill)
filterAppSkills sid apps = LS.head $ filter ( \(Entity _ ( AppSkill _ sid' _ )) -> (==) sid' sid) apps


bldThead :: (Double -> Text) -> Int -> Int -> [Tree (Entity JobSkill, Entity Skill)] -> WidgetFor App ()
bldThead _ _ _ [] = [whamlet||]
bldThead fmt h l ns = [whamlet|
<tr>
  $if l == 1
    <th rowspan=#{h}>_{MsgCandidate}
    <th rowspan=#{h}>_{MsgWeight}
  $forall n@(Node (Entity _ (JobSkill _ _ weight _ _), Entity _ (Skill code name _ _)) cs) <- ns
    $if null cs && (l < h)
      <th.text-center title=#{name} colspan=#{width n} rowspan=#{(h - l) + 1}>
        <span.position-relative>
          #{code}
          <span.badge.rounded-pill.position-absolute.top-0.start-100.text-body-secondary.fw-light
            style="transform:translate(-80%,-60%)">
            #{fmt weight}
    $else
      <th.text-center title=#{name} colspan=#{width n} rowspan=1>
        <span.position-relative>
          #{code}
          <span.badge.rounded-pill.position-absolute.top-0.start-100.text-body-secondary.fw-light
            style="transform:translate(-80%,-60%)">
            #{fmt weight}

^{bldThead fmt h (l + 1) (allChildren ns)}
|]


bldFullTree :: App -> [Lang]
            -> [Entity AppSkill]
            -> [Tree (Entity JobSkill, Entity Skill)]
            -> [Tree (Text,Double,Text,Text)]
bldFullTree _ _ _ [] = []
bldFullTree app langs xs (Node (Entity jsid (JobSkill _ _ w _ _), Entity sid (Skill code name _ _)) [] : t) =
  Node (ident,w,code,name) [Node (ident,weight,expasses,expasses) []] : bldFullTree app langs xs t
  where
    ident = pack . show $ fromSqlKey jsid
    weight = maybe 0 (appSkillWeight . entityVal) (LS.head $ filter (\(Entity _ (AppSkill _ sid' _)) -> sid' == sid) xs)
    expasses = renderMessage app langs MsgExpertAssessment
bldFullTree app langs xs (Node (Entity jsid (JobSkill _ _ w _ _), Entity _ (Skill code name _ _)) c : t) =
  Node (ident,w,code,name) (bldFullTree app langs xs c) : bldFullTree app langs xs t
  where
    ident = pack . show $ fromSqlKey jsid


bldTree :: [(Entity JobSkill, Entity Skill)]
        -> [Tree (Entity JobSkill, Entity Skill)]
bldTree xs = go (filter root xs) xs
  where

    root :: (Entity JobSkill, Entity Skill) -> Bool
    root (Entity _ (JobSkill _ _ _ Nothing _),_) = True
    root _ = False

    child :: (Entity JobSkill, Entity Skill)
          -> (Entity JobSkill, Entity Skill)
          -> Bool
    child (Entity pid _, _) (Entity _ (JobSkill _ _ _ (Just pid') _), _) = pid == pid'
    child _ _ = False

    go :: [(Entity JobSkill, Entity Skill)]
       -> [(Entity JobSkill, Entity Skill)]
       -> [Tree (Entity JobSkill, Entity Skill)]
    go [] _ = []
    go (h:t) as = Node h (go (filter (child h) as) as) : go t as


allChildren :: [Tree a] -> [Tree a]
allChildren xs = children =<< xs


maxHeight :: [Tree a] -> Int
maxHeight ns = maximum (height <$> ns)


allLeafs :: [Tree a] -> [Tree a]
allLeafs = concatMap leafs


fmtDbl :: Text -> LocaleName -> Double -> Text
fmtDbl = formatDouble'
