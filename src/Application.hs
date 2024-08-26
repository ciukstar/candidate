{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application
    ( getApplicationDev
    , appMain
    , develMain
    , makeFoundation
    , makeLogWare
    -- * for DevelMain
    , getApplicationRepl
    , shutdownApp
    -- * for GHCI
    , handler
    , db
    ) where

import System.Environment.Blank (getEnv)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Logger (liftLoc, runLoggingT)

import Data.Maybe (Maybe (Just))
import Database.Persist.Sql (runSqlPool)

import Foundation
    ( Route
      ( RobotsR, DeptR, DeptsR, JobCandidateR, JobCandidatesR
      , CandidateR, CandidatesR, JobSkillsEditFormR, JobEditFormR
      , JobCreateFormR, JobSkillR, JobSkillsEditR, JobSkillsR, JobR, JobsR
      , AppSkillsEditFormR, ApplicantEditFormR, ApplicantCreateFormR
      , AppPhotoR, AppSkillR, AppSkillsEditR, AppSkillsR, ApplicantTagR
      , ApplicantR, ApplicantsR, SkillEditFormR, SkillCreateFormR
      , SkillsLabelR, SkillR, SkillsR, StaticR, AuthR, HomeR
      , PhotoPlaceholderR, FaviconR, ApplicantSkillsR, DocsR
      )
    , resourcesApp, unsafeHandler, App(..), Handler
    )
    
import Import.NoFoundation
    ( ($), Monad(return, (>>=)), Show(show), Bool(True), Int, IO
    , error, migrateAll, SqlBackend, flip, when, (++), runMigration
    , defaultMiddlewaresNoLogging, toWaiAppPlain, mkYesodDispatch
    , static, staticDevel, (.), Default(def), LogLevel(LevelError)
    , ReaderT, Application, Yesod(messageLoggerSource)
    , YesodPersist(runDB), loggerSet, configSettingsYmlValue
    , AppSettings
      ( appHost, appMutableStatic, appStaticDir, appDatabaseConf
      , appConnectionPoolConfig, appDetailedRequestLogging, appIpFromHeader
      , appPort
      )
    , loadYamlSettings, loadYamlSettingsArgs, useEnv
    , configSettingsYml, develMainHelper, getDevSettings
    , makeYesodLogger, getAuth
    )

import Language.Haskell.TH.Syntax (qLocation)
import Network.HTTP.Client.TLS (getGlobalManager)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp
  ( Settings, defaultSettings, defaultShouldDisplayException
  , runSettings, setHost, setOnException, setPort, getPort
  )
import Network.Wai.Middleware.RequestLogger
  ( Destination (Logger), IPAddrSource (..), OutputFormat (..)
  , destination, mkRequestLogger, outputFormat
  )
import System.Log.FastLogger
  ( defaultBufSize, newStdoutLoggerSet, toLogStr)

import Handler.Common (getPhotoPlaceholderR, getFaviconR, getRobotsR )
import Handler.Home ( getHomeR )

import Handler.Docs (getDocsR)

import Handler.Skills
  ( getSkillsR
  , postSkillsR
  , getSkillR
  , postSkillR
  , deleteSkillR
  , getSkillCreateFormR
  , getSkillEditFormR
  , postSkillsLabelR
  )

import Handler.Applicants
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
  )

import Handler.Jobs
  ( getJobsR
  , postJobsR
  , getJobCreateFormR
  , getJobEditFormR
  , getJobR
  , postJobR
  , deleteJobR
  , postJobSkillsR
  , deleteJobSkillR
  , putJobSkillR
  , getJobSkillsEditFormR
  , postJobSkillsEditR
  )

import Handler.Candidates
  ( getJobCandidatesR
  , getCandidateR
  , getCandidatesR
  , getJobCandidateR
  )

import Handler.Depts
  ( postDeptsR
  , deleteDeptR
  , postDeptR
  )

import Database.Persist.Sqlite
    (SqliteConf(sqlDatabase), createSqlitePoolWithConfig)
import Network.Wai.Middleware.Gzip
    ( gzip, GzipSettings (gzipFiles), GzipFiles (GzipCompress)
    )
    
import Demo.DemoDataEN (populateEN)
import Demo.DemoDataFR (populateFR)
import Demo.DemoDataRO (populateRO)
import Demo.DemoDataRU (populateRU)

mkYesodDispatch "App" resourcesApp

-- | This function allocates resources (such as a database connection pool),
-- performs initialization and returns a foundation datatype value. This is also
-- the place to put your migrate statements to have automatic database
-- migrations handled by Yesod.
makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    -- Some basic initializations: HTTP connection manager, logger, and static
    -- subsite.
    appHttpManager <- getGlobalManager
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appStatic <-
        (if appMutableStatic appSettings then staticDevel else static)
        (appStaticDir appSettings)

    -- We need a log function to create a connection pool. We need a connection
    -- pool to create our foundation. And we need our foundation to get a
    -- logging function. To get out of this loop, we initially create a
    -- temporary foundation without a real connection pool, get a log function
    -- from there, and then create the real foundation.
    let mkFoundation appConnPool = App {..}
        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation appLogger

    -- Create the database connection pool
    pool <- flip runLoggingT logFunc $ createSqlitePoolWithConfig
        (sqlDatabase  $ appDatabaseConf appSettings)
        (appConnectionPoolConfig appSettings)

    -- Perform database migration using our application's logging settings.
    flip runLoggingT logFunc $ flip runSqlPool pool $ do
      runMigration migrateAll

      demo <- liftIO $ getEnv "DEMO_LANG"
      case demo of
        Just "EN" -> populateEN
        Just "FR" -> populateFR
        Just "RO" -> populateRO
        Just "RU" -> populateRU
        _ -> populateEN

    -- Return the foundation
    return $ mkFoundation pool

-- | Convert our foundation to a WAI Application by calling @toWaiAppPlain@ and
-- applying some additional middlewares.
makeApplication :: App -> IO Application
makeApplication foundation = do
    logWare <- makeLogWare foundation
    -- Create the WAI application and apply middlewares
    appPlain <- toWaiAppPlain foundation
    return $ logWare $ defaultMiddlewaresNoLogging $ gzip def { gzipFiles = GzipCompress } appPlain

makeLogWare :: App -> IO Middleware
makeLogWare foundation =
    mkRequestLogger def
        { outputFormat =
            if appDetailedRequestLogging $ appSettings foundation
                then Detailed True
                else Apache
                        (if appIpFromHeader $ appSettings foundation
                            then FromFallback
                            else FromSocket)
        , destination = Logger $ loggerSet $ appLogger foundation
        }


-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
      setPort (appPort $ appSettings foundation)
    $ setHost (appHost $ appSettings foundation)
    $ setOnException (\_req e ->
        when (defaultShouldDisplayException e) $ messageLoggerSource
            foundation
            (appLogger foundation)
            $(qLocation >>= liftLoc)
            "yesod"
            LevelError
            (toLogStr $ "Exception from Warp: " ++ show e))
      defaultSettings

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app <- makeApplication foundation
    return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
    -- Get the settings from all relevant sources
    settings <- loadYamlSettingsArgs
        -- fall back to compile-time values, set to [] to require values at runtime
        [configSettingsYmlValue]

        -- allow environment variables to override
        useEnv

    -- Generate the foundation from the settings
    foundation <- makeFoundation settings

    -- Generate a WAI Application from the foundation
    app <- makeApplication foundation

    -- Run the application with Warp
    runSettings (warpSettings foundation) app


--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
    settings <- getAppSettings
    foundation <- makeFoundation settings
    wsettings <- getDevSettings $ warpSettings foundation
    app1 <- makeApplication foundation
    return (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()


---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------

-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

-- | Run DB queries
db :: ReaderT SqlBackend Handler a -> IO a
db = handler . runDB
