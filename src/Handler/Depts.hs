{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeApplications #-}

module Handler.Depts
  ( postDeptsR
  , deleteDeptR
  , deptTreeWidget
  , postDeptR
  ) where

import Control.Monad.IO.Class ( MonadIO )
import Data.Text (Text)
import Text.Cassius (cassius)
import Yesod.Core.Handler (HandlerFor, redirectUltDest, addMessageI)
import Yesod.Core.Widget (WidgetFor, whamlet, toWidget)
import Foundation
  ( App, Route (JobsR)
  , AppMessage
    ( MsgRemove, MsgAdd, MsgActions, MsgDuplicateName, MsgEdit
    , MsgRecordEdited, MsgRecordDeleted, MsgRecordAdded
    )
  )
import Yesod.Form (runInputPost, ireq, textField, intField, iopt)

import Model (Dept(Dept, deptName), DeptId, EntityField (DeptName, DeptId))
import Database.Persist (PersistStoreWrite(insert_, delete), Entity (Entity))
import Yesod (YesodPersist(runDB))
import Database.Persist.Sql (toSqlKey, fromSqlKey, SqlBackend)
import Tree (Tree (Node))
import Database.Esqueleto.Experimental
  ( selectOne, from, table, where_, val
  , (^.), (==.), (=.), update, set
  )
import ClassyPrelude.Yesod (ReaderT)


deleteDeptR :: DeptId -> HandlerFor App ()
deleteDeptR did = do
  runDB $ delete did
  addMessageI "alert-info toast" MsgRecordDeleted


postDeptR :: DeptId -> HandlerFor App ()
postDeptR did = do
  name <- runInputPost $ ireq textField "dept"
  mdept <- runDB $ selectOne $ do
    x <- from $ table @Dept
    where_ $ x ^. DeptName ==. val name
    return x
  case mdept of
    Just (Entity did' _)
      | did' /= did -> addMessageI "alert-danger" (MsgDuplicateName name)
      | otherwise -> runDB $ update' name did
    Nothing -> do
      runDB $ update' name did
      addMessageI "alert-info toast" MsgRecordEdited
  redirectUltDest JobsR
  where
    update' :: MonadIO m => Text -> DeptId -> ReaderT SqlBackend m ()
    update' name' did' = update $ \d -> do
      set d [DeptName =. val name']
      where_ $ d ^. DeptId ==. val did'


postDeptsR :: HandlerFor App ()
postDeptsR = do
  dept <- runInputPost $ Dept <$> ireq textField "dept" <*> ((toSqlKey <$>) <$> iopt intField "parent")
  mdept <- runDB $ selectOne $ do
    x <- from $ table @Dept
    where_ $ x ^. DeptName ==. val (deptName dept)
    return x
  case mdept of
    Just (Entity _ (Dept name _)) -> do
      addMessageI "alert-danger" (MsgDuplicateName name)
    Nothing -> do
      runDB $ insert_ dept
      addMessageI "alert-info toast" MsgRecordAdded
  redirectUltDest JobsR


deptTreeWidget :: Text -> [Text] -> [Entity Dept] -> WidgetFor App ()
deptTreeWidget idFormSearch labels = bldTreeWidget idFormSearch labels . bldTree


bldTreeWidget :: Text -> [Text] -> [Tree (Entity Dept)] -> WidgetFor App ()
bldTreeWidget _ _ [] = [whamlet||]
    
bldTreeWidget idFormSearch labels ns = do
    toWidget [cassius|
                     ul.labels
                       li.label:hover
                         background-color: rgba(0,0,0,0.075)
                     |]
    [whamlet|
            <ul.labels>
              $forall Node (Entity did (Dept name _)) cs <- ns
                <li.label.rounded-4.mb-1>
                  <div.text-nowrap.d-flex.flex-row.align-items-center.justify-content-between>
                    <span>
                      $if not (null cs)
                        <button.chevron.btn.btn-light.border-0.rounded-circle data-bs-toggle=collapse
                          data-bs-target=#target#{fromSqlKey did} aria-expanded=true>
                      <input.btn-check type=checkbox name=label value=#{name} form=#{idFormSearch} :elem name labels:checked
                        #dept#{fromSqlKey did} onchange="this.form.submit()">
                      <label.btn.btn-outline-secondary.text-nowrap.border-0 for=dept#{fromSqlKey did}>
                        #{name}
                    <div.dropdown>
                      <button.btn.btn-light.border-0.rounded-circle title=_{MsgActions}
                        data-bs-toggle=dropdown aria-expanded=false>
                        <i.bi.bi-three-dots-vertical>
                      <ul.dropdown-menu>
                        <li>
                          <button.dropdown-item type=button
                            data-bs-toggle=modal data-bs-target=#modalSubdept#{fromSqlKey did}>
                            <i.bi.bi-plus-lg.me-2>
                            _{MsgAdd}
                          <button.dropdown-item type=button
                            data-bs-toggle=modal data-bs-target=#modalEditDept#{fromSqlKey did}>
                            <i.bi.bi-pencil.me-2>
                            _{MsgEdit}
                          <button.dropdown-item.delete type=button
                            data-bs-toggle=modal data-bs-target=#modalDeleteDept#{fromSqlKey did}>
                            <i.bi.bi-trash.me-2>
                            _{MsgRemove}
                $if not (null cs)
                  <div.collapse.show.ms-3 #target#{fromSqlKey did}>
                    ^{bldTreeWidget idFormSearch labels cs}
            |]


bldTree :: [Entity Dept] -> [Tree (Entity Dept)]
bldTree xs = go (filter root xs) xs
  where
    root :: Entity Dept -> Bool
    root (Entity _ (Dept _ Nothing)) = True
    root _ = False

    child :: Entity Dept -> Entity Dept -> Bool
    child (Entity pid _) (Entity _ (Dept _ (Just pid'))) = pid' == pid
    child _ _ = False

    go :: [Entity Dept] -> [Entity Dept] -> [Tree (Entity Dept)]
    go [] _ = []
    go (h:t) as = Node h (go (filter (child h) as) as) : go t as
