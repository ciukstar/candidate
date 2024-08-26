{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Skills
  ( getSkillsR
  , postSkillsR
  , getSkillR
  , postSkillR
  , deleteSkillR
  , getSkillCreateFormR
  , getSkillEditFormR
  , postSkillsLabelR
  ) where

import Control.Applicative ( Alternative((<|>)) )
import Data.Maybe (fromMaybe, isJust)
import Data.Text (unpack, Text, isInfixOf)
import qualified Data.Text as T (null)
import Text.Read (readMaybe)
import qualified Data.List.Safe as LS (head)
import Text.Julius (RawJS(rawJS))

import Yesod.Core.Handler
  ( HandlerFor, YesodRequest (reqGetParams)
  , selectRep, provideRep
  , addMessageI, getRequest, redirectUltDest, setUltDestCurrent
  , lookupSession, newIdent, getUrlRenderParams, getMessages
  )

import Yesod.Core.Content (TypedContent)
import Yesod.Core
  ( Yesod (defaultLayout), Html, WidgetFor, whamlet, MonadIO
  , setTitleI, SomeMessage (SomeMessage)
  )

import Settings (widgetFile)
import ClassyPrelude.Yesod (ReaderT, forM, FormResult, FieldView (fvLabel))
import Yesod.Form.Types (FieldSettings (fsLabel, fsTooltip, fsId, fsAttrs, fsName))
import Yesod.Form.Fields (Textarea (Textarea))
import Yesod.Form.Input (runInputPost)
import Yesod.Form.Functions (checkM)
import Yesod.Form
  ( MForm, FormResult (FormSuccess)
  , FieldView (fvInput, fvId, fvErrors), FieldSettings (FieldSettings)
  , generateFormPost, mreq, mopt, runFormPost, unTextarea, ireq, iopt
  )

import qualified Yesod.Form as YF (textField, textareaField)
import Widgets (thSort, thSortDir)

import Foundation
  ( App
  , Route (HomeR, SkillsR, SkillR, SkillCreateFormR, SkillEditFormR, SkillsLabelR)
  , AppMessage
    ( MsgSkills, MsgSkill, MsgSearch, MsgAdd, MsgSave, MsgCode
    , MsgDenom, MsgDescription, MsgCancel, MsgEdit, MsgDelete
    , MsgPleaseConfirm, MsgReallyDelete, MsgInvalidFormData
    , MsgRowsPerPage, MsgPaginationLabel, MsgFirst, MsgPrevious
    , MsgNext, MsgLast, MsgNoDataFound, MsgHome, MsgActions
    , MsgNumberSign, MsgDetails, MsgCategories, MsgCategory
    , MsgClose, MsgEnterNewName, MsgId, MsgLabels, MsgAttributes
    , MsgBack, MsgDuplicateCode, MsgInQuotes, MsgRecordDeleted
    , MsgRecordAdded, MsgRecordEdited
    )
  )

import Yesod ( YesodPersist(runDB) )
import Model
  ( Skill (Skill, skillCode, skillName, skillDescr, skillLabel)
  , EntityField (SkillId, SkillCode, SkillName, SkillDescr, SkillLabel)
  , SkillId, ultDestKey, Params (Params)
  )

import Database.Persist
  ( PersistStoreRead(get), Entity(Entity)
  , PersistStoreWrite(insert_, delete, replace)
  , entityVal
  )

import Database.Persist.Sql (SqlBackend, fromSqlKey)
import Database.Esqueleto.Experimental
  ( SqlQuery, SqlExpr, Value (unValue, Value), from, table
  , select, orderBy, desc, where_, val, just, like, asc
  , selectOne, countRows, offset, limit
  , (^.), (++.), (||.), (%), (==.), (=.), distinct, coalesceDefault
  , isNothing, not_, justList, in_, valList, update, set, upper_
  )


deleteSkillR :: SkillId -> HandlerFor App ()
deleteSkillR sid = do
  runDB $ delete sid
  addMessageI "alert-info toast" MsgRecordDeleted


postSkillR :: SkillId -> HandlerFor App TypedContent
postSkillR sid = do
  categs <- runDB fetchSkillCategs
  mskill <- runDB $ selectOne $ do
    x <- from $ table @Skill
    where_ $ x ^. SkillId ==. val sid
    return x
  selectRep $ provideRep $ do
    ((r,widget),enctype) <- runFormPost $ formSkill categs True mskill
    case r of
      FormSuccess skill -> do
        runDB $ replace sid skill
        addMessageI "alert-info toast" MsgRecordEdited
        redirectUltDest SkillsR
      _ -> do
        addMessageI "alert-danger" MsgInvalidFormData
        msgs <- getMessages
        let params = [("desc","id"),("offset","0"),("limit","5")]
        ult <- getUrlRenderParams >>= \rndr -> fromMaybe (rndr SkillsR params)  <$> lookupSession ultDestKey
        defaultLayout $ do
          setTitleI MsgSkill
          $(widgetFile "skills/edit")


getSkillR :: SkillId -> HandlerFor App TypedContent
getSkillR ident = selectRep $ do
  provideRep $ do
    skill <- runDB $ get ident
    defaultLayout $ do
      setTitleI MsgSkill
      let ultParams = [("desc","id"),("offset","0"),("limit","5")]
      ult <- getUrlRenderParams >>= \rndr -> fromMaybe (rndr SkillsR ultParams) <$> lookupSession ultDestKey
      $(widgetFile "skills/skill")


postSkillsR :: HandlerFor App TypedContent
postSkillsR = do
  categs <- runDB fetchSkillCategs
  selectRep $ provideRep $ do
    ((r,widget),enctype) <- runFormPost $ formSkill categs True Nothing
    case r of
      FormSuccess skill -> do
        runDB $ insert_ skill
        addMessageI "alert-info toast" MsgRecordAdded 
        redirectUltDest SkillsR
      _ -> do
        addMessageI "alert-danger" MsgInvalidFormData
        msgs <- getMessages
        ult <- lookupSession ultDestKey
        defaultLayout $(widgetFile "skills/create")


postSkillsLabelR :: HandlerFor App TypedContent
postSkillsLabelR = do
  (old,mnew) <- runInputPost $ (,) <$> ireq YF.textField "old" <*> iopt YF.textField "new"
  case mnew of
    Just new -> runDB $ update $ \s -> do
      set s [SkillLabel =. just (val new)]
      where_ $ s ^. SkillLabel ==. just (val old)
    Nothing -> runDB $ update $ \s -> do
      set s [SkillLabel =. val Nothing]
      where_ $ s ^. SkillLabel ==. just (val old)
  addMessageI "alert-info toast" MsgRecordEdited
  redirectUltDest SkillsR


getSkillsR :: HandlerFor App TypedContent
getSkillsR = do
  params@(Params mq moffset mlimit msort lbls) <- do
    params <- reqGetParams <$> getRequest
    return $ Params
        (snd <$> LS.head (filter (\(x,y) -> x == "q" && not (T.null y)) params))
        (readMaybe . unpack . snd =<< LS.head (filter ((== "offset") . fst) params))
        (readMaybe . unpack . snd =<< LS.head (filter ((== "limit") . fst) params))
        (LS.head (filter ((\x -> x == "asc" || x == "desc") . fst) params))
        (snd <$> filter (\(x,y) -> x == "label" && not (T.null y)) params)

  labels <- runDB fetchSkillCategs >>= \xs -> forM xs (\x -> newIdent >>= \y -> return (y,x))
  rcnt <- runDB $ fetchCount params
  skills <- runDB $ fetchSkills params
  let maxo = fromMaybe 0 $ (*)
        <$> ((+) <$> (div rcnt <$> mlimit) <*> ((\x -> if x > 0 then 0 else -1) . mod rcnt <$> mlimit))
        <*> mlimit
  let prev = fromMaybe 0 $ max <$> ((-) <$> moffset <*> mlimit) <*> pure 0
  let next = fromMaybe 0 $ min <$> ((+) <$> moffset <*> mlimit) <*> pure maxo
  let start = maybe 0 (+ 1) moffset
  let end = fromMaybe 0 $ min <$> ((+) <$> moffset <*> mlimit) <*> pure rcnt
  msgs <- getMessages
  selectRep $ provideRep $ defaultLayout $ do
      setTitleI MsgSkills
      setUltDestCurrent
      ult <- lookupSession ultDestKey
      idButtonSkillDelete <- newIdent
      $(widgetFile "skills/skills")


getSkillEditFormR :: SkillId -> HandlerFor App Html
getSkillEditFormR sid = do
  categs <- runDB fetchSkillCategs
  mskill <- runDB $ selectOne $ do
    x <- from $ table @Skill
    where_ $ x ^. SkillId ==. val sid
    return x
    
  (widget,enctype) <- generateFormPost $ formSkill categs False mskill
  msgs <- getMessages
  let params = [("desc","id"),("offset","0"),("limit","5")]
  ult <- getUrlRenderParams >>= \r -> fromMaybe (r SkillsR params)  <$> lookupSession ultDestKey
  defaultLayout $ do
    setTitleI MsgSkill
    $(widgetFile "skills/edit")


getSkillCreateFormR :: HandlerFor App Html
getSkillCreateFormR = do
  categs <- runDB fetchSkillCategs
  (widget,enctype) <- generateFormPost $ formSkill categs False Nothing
  msgs <- getMessages
  ult <- lookupSession ultDestKey
  defaultLayout $ do
    setTitleI MsgSkill
    $(widgetFile "skills/create")


formSkill :: [Text]
          -> Bool
          -> Maybe (Entity Skill)
          -> Html
          -> MForm (HandlerFor App) (FormResult Skill, WidgetFor App ())
formSkill categs isPost skill extra = do
  (codeR,codeV) <- mreq uniqueTextField (fs MsgCode) (skillCode . entityVal <$> skill)
  (nameR,nameV) <- mreq YF.textField (fs MsgDenom) (skillName . entityVal <$> skill)
  (descR,descV) <- mopt YF.textareaField (fs MsgDescription) (skillDescr . entityVal <$> skill)
  (categR,categV) <- mopt YF.textField FieldSettings
    { fsLabel = SomeMessage MsgCategory
    , fsTooltip = Nothing
    , fsId = Nothing
    , fsName = Nothing
    , fsAttrs = [("class","form-control"),("list","taglist")]
    } (skillLabel . entityVal <$> skill)

  let skillR = Skill <$> codeR <*> nameR <*> descR <*> categR
  let widget = [whamlet|
#{extra}
<div.d-flex.flex-column.gap-2>

  $forall v <- [codeV,nameV,descV]
    <div :isJust (fvErrors v):.is-invalid
         :not (isJust (fvErrors v)) && isPost:.is-valid>
      <label.form-label.mb-0.ps-1 for=#{fvId v} #label#{fvId v}>#{fvLabel v}
      ^{fvInput v}
      $maybe errs <- fvErrors v
        <div.invalid-feedback>
          #{errs}
          
  <div>
    <label.form-label.mb-0.ps-1 for=#{fvId categV}>#{fvLabel categV}
    <div.input-group :isJust (fvErrors categV):.is-invalid
                     :not (isJust (fvErrors categV)) && isPost:.is-valid>
      ^{fvInput categV}
      $maybe errs <- fvErrors categV
        <div.invalid-feedback>
          #{errs}
      <button.btn.btn-outline-secondary.dropdown-toggle type=button data-bs-toggle=dropdown aria-expanded=false>
      <ul.dropdown-menu.dropdown-menu-end.w-100>
        $forall categ <- categs
          <li>
            <input.btn-check type=radio name=categ value=#{categ} ##{categ}
              onchange="document.getElementById('#{fvId categV}').value = this.value">
            <label.btn.dropdown-item for=#{categ}>#{categ}
          
    <datalist #taglist>
      $forall categ <- categs
        <option value=#{categ}>
|]
  return (skillR, widget)
  where
    fs :: AppMessage -> FieldSettings App
    fs msg = FieldSettings (SomeMessage msg) Nothing Nothing Nothing
      [("class","form-control")]

    uniqueTextField = checkM checkUniqueCode YF.textField

    checkUniqueCode :: Text -> HandlerFor App (Either AppMessage Text)
    checkUniqueCode code = case skill of
      Nothing -> do 
          me <- runDB $ selectOne $ do
            x <- from $ table @Skill
            where_ $ x ^. SkillCode ==. val code
            return x
          return $ case me of
            Just _ -> Left MsgDuplicateCode
            Nothing -> Right code

      Just (Entity sid _) -> do
        ms <- runDB $ selectOne $ do
          x <- from $ table @Skill
          where_ $ x ^. SkillCode ==. val code
          return x
        return $ case ms of
          Just (Entity sid' _) | sid' == sid -> Right code
                               | otherwise -> Left MsgDuplicateCode
          Nothing -> Right code      


fetchSkillCategs :: MonadIO m => ReaderT SqlBackend m [Text]
fetchSkillCategs = (unValue <$>) <$> (select $ distinct querySkillCategs)


querySkillCategs :: SqlQuery (SqlExpr (Value Text))
querySkillCategs = do
  x <- from $ table @Skill
  where_ $ not_ (isNothing (x ^. SkillLabel))
  let categ = coalesceDefault [x ^. SkillLabel] (val "")
  orderBy [asc categ]
  return categ


fetchSkills :: MonadIO m => Params -> ReaderT SqlBackend m [Entity Skill]
fetchSkills params = select $ querySkills params


querySkills :: Params -> SqlQuery (SqlExpr (Entity Skill))
querySkills (Params mq mo ml ms mls) = do
  x <- from $ table @Skill

  case mq of
    Nothing -> return ()
    Just q -> where_ $ (upper_ (x ^. SkillCode) `like` ((%) ++. upper_ (val q) ++. (%)))
      ||. (upper_ (x ^. SkillName) `like` ((%) ++. upper_ (val q) ++. (%)))
      ||. (upper_ (x ^. SkillDescr) `like` ((%) ++. just (upper_ (val (Textarea q))) ++. (%)))
      ||. (upper_ (x ^. SkillLabel) ==. just (upper_ (val q)))

  case mls of
    [] -> return ()
    xs -> where_ $ x ^. SkillLabel `in_` justList (valList xs)

  case ms of
    Just ("desc","id") -> orderBy [desc (x ^. SkillId)]
    Just ("asc","id") -> orderBy [asc (x ^. SkillId)]
    Just ("desc","code") -> orderBy [desc (x ^. SkillCode)]
    Just ("asc","code") -> orderBy [asc (x ^. SkillCode)]
    Just ("desc","name") -> orderBy [desc (x ^. SkillName)]
    Just ("asc","name") -> orderBy [asc (x ^. SkillName)]
    Just ("desc","descr") -> orderBy [desc (x ^. SkillName)]
    Just ("asc","descr") -> orderBy [asc (x ^. SkillName)]
    _ -> return ()

  case mo of
    Just n -> offset $ fromIntegral n
    Nothing -> return ()

  case ml of
    Just n -> limit $ fromIntegral n
    Nothing -> return ()

  return x


fetchCount :: MonadIO m => Params -> ReaderT SqlBackend m Int
fetchCount params = unValue . fromMaybe (Value 0) <$> selectOne (queryCount params)


queryCount :: Params -> SqlQuery (SqlExpr (Value Int))
queryCount (Params mq _ _ _ mls) = do
  x <- from $ table @Skill

  case mq of
    Nothing -> return ()
    Just q -> where_ $ (upper_ (x ^. SkillCode) `like` ((%) ++. upper_ (val q) ++. (%)))
      ||. (upper_ (x ^. SkillName) `like` ((%) ++. upper_ (val q) ++. (%)))
      ||. (upper_ (x ^. SkillDescr) `like` ((%) ++. just (upper_ (val (Textarea q))) ++. (%)))
      ||. (upper_ (x ^. SkillLabel) ==. just (upper_ (val q)))

  case mls of
    [] -> return ()
    xs -> where_ $ x ^. SkillLabel `in_` justList (valList xs)

  return countRows
