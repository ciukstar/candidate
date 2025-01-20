{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Foundation
  ( Handler, Form
  , App(..)
  , Route(..)
  , AppMessage(..)
  , resourcesApp
  , unsafeHandler
  , getServiceWorkerR
  ) where


import Control.Monad((>>), return)
import Control.Monad.Logger (LogSource)

import qualified Data.CaseInsensitive as CI
import Data.Bool (Bool (True, False), (||))
import Data.Either (Either)
import Data.Eq ((==))
import Data.Function (flip, (.), ($))
import Data.Functor ((<$>))
import Data.List ((++))
import qualified Data.List.Safe as LS
import Data.Maybe (Maybe (..), fromMaybe)
import Data.Text (Text)
import qualified Data.Text.Encoding as TE

import Database.Persist.Sql (ConnectionPool, runSqlPool, SqlBackend)

import Model
    ( JobSkillId, SkillId, ApplicantId, JobId, DeptId
    , Unique(UniqueUser)
    , UserId, User(User, userPassword, userIdent)
    )

import Import.NoFoundation
    ( Int, show, IO
    , AppSettings
      ( appShouldLogAll ,appAuthDummyLogin ,appRoot ,appAnalytics
      , appStaticDir
      )
    , Route(StaticRoute, LoginR)
    , Static
    , defaultClientSessionBackend
    , defaultYesodMiddleware
    , getApprootText
    , guessApproot
    , widgetToPageContent
    , mkYesodData
    , parseRoutesFile
    , defaultFormMessage
    , defaultGetDBRunner
    , base64md5
    , LByteString
    , HasHttpManager(..)
    , Manager
    , LogLevel(LevelError, LevelWarn)
    , Entity(Entity)
    , PersistStoreWrite(insert)
    , PersistUniqueRead(getBy)
    , SqlPersistT
    , Lang
    , RenderMessage(..)
    , MonadHandler (liftHandler, HandlerSite)
    , Yesod
      ( approot, makeLogger, shouldLogIO, addStaticContent, isAuthorized
      , authRoute, defaultLayout, yesodMiddleware, makeSessionBackend
      )
    , ToTypedContent
    , Approot(ApprootRequest)
    , AuthResult(Authorized)
    , PageContent(pageBody, pageTitle, pageHead)
    , SessionBackend
    , RenderRoute(Route, renderRoute)
    , FormMessage
    , Creds(credsIdent)
    , YesodAuthPersist
    , DBRunner
    , YesodPersist(..)
    , YesodPersistRunner(..)
    )

import Settings (widgetFile)
import Settings.StaticFiles (js_cookie_3_0_1_dist_js_cookie_js)

import Text.Hamlet (hamletFile, Html)
import Text.Jasmine (minifym)
import Text.Julius (juliusFile)
import Text.Shakespeare.I18N (mkMessage)

import Yesod.Auth
    ( YesodAuth
      ( AuthId, authPlugins, authenticate, redirectToReferer, logoutDest
      , loginDest
      )
    , getAuth, Auth, AuthPlugin
    , AuthenticationResult(Authenticated)
    )
-- Used only when in "auth-dummy-login" setting is enabled.
import Yesod.Auth.Dummy
import Yesod.Auth.OpenId (authOpenId, IdentifierType (Claimed))
import Yesod.Core (hamlet)
import Yesod.Core.Content
    ( TypedContent (TypedContent), toContent, typeJavascript
    )
import Yesod.Core.Handler
  ( HandlerFor, defaultCsrfCookieName, defaultCsrfHeaderName, getCurrentRoute
  , getYesod, languages, withUrlRenderer, setUltDestCurrent, getUrlRenderParams
  )
import Yesod.Default.Util (addStaticContentExternal)
import Yesod.Core.Types (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import Yesod.Core.Widget (toWidgetHead)
import Yesod.Form (MForm, FormResult)
import Yesod.Form.I18n.English (englishFormMessage)
import Yesod.Form.I18n.French (frenchFormMessage)
import Yesod.Form.I18n.Romanian (romanianFormMessage)
import Yesod.Form.I18n.Russian (russianFormMessage)

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
    { appSettings    :: AppSettings
    , appStatic      :: Static -- ^ Settings for static file serving.
    , appConnPool    :: ConnectionPool -- ^ Database connection pool.
    , appHttpManager :: Manager
    , appLogger      :: Logger
    }

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerFor App
-- type Widget = WidgetFor App ()
mkYesodData "App" $(parseRoutesFile "config/routes.yesodroutes")

mkMessage "App" "messages" "en"

type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
    -- Controls the base of generated URLs. For more information on modifying,
    -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
    approot :: Approot App
    approot = ApprootRequest $ \app req ->
        case appRoot $ appSettings app of
            Nothing -> getApprootText guessApproot app req
            Just root -> root

    -- Store session data on the client in encrypted cookies,
    -- default session idle timeout is 120 minutes
    makeSessionBackend :: App -> IO (Maybe SessionBackend)
    makeSessionBackend _ = Just <$> defaultClientSessionBackend
        120    -- timeout in minutes
        "config/client_session_key.aes"

    -- Yesod Middleware allows you to run code before and after each handler function.
    -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
    -- Some users may also want to add the defaultCsrfMiddleware, which:
    --   a) Sets a cookie with a CSRF token in it.
    --   b) Validates that incoming write requests include that token in either a header or POST parameter.
    -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
    -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
    yesodMiddleware :: ToTypedContent res => Handler res -> Handler res
    yesodMiddleware = defaultYesodMiddleware

    defaultLayout :: Widget -> Handler Html
    defaultLayout widget = do
        master <- getYesod
        langs <- languages
        let lang = fromMaybe "en" . LS.head $ langs
        currRoute <- getCurrentRoute

        -- We break up the default layout into two components:
        -- default-layout is the contents of the body tag, and
        -- default-layout-wrapper is the entire page. Since the final
        -- value passed to hamletToRepHtml cannot be a widget, this allows
        -- you to use normal widget features in default-layout.

        pc <- widgetToPageContent $ do
          toWidgetHead [hamlet|<script type=text/javascript src=@{StaticR js_cookie_3_0_1_dist_js_cookie_js}>|]
          $(widgetFile "default-layout")
        withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

    -- The page to be redirected to when authentication is required.
    authRoute :: App -> Maybe (Route App)
    authRoute _ = Just $ AuthR LoginR

    isAuthorized :: Route App -> Bool -> Handler AuthResult
    
    isAuthorized (AuthR _) _ = return Authorized
    isAuthorized FaviconR _ = return Authorized
    isAuthorized RobotsR _ = return Authorized
    isAuthorized SitemapR _ = return Authorized
    isAuthorized WebAppManifestR _ = return Authorized

    isAuthorized ServiceWorkerR _ = return Authorized    
    
    isAuthorized PhotoPlaceholderR _ = return Authorized

    isAuthorized HomeR _ = setUltDestCurrent >> return Authorized
    isAuthorized DocsR _ = return Authorized
    
    isAuthorized (StaticR _) _ = return Authorized
    isAuthorized SkillsR _ = setUltDestCurrent >> return Authorized
    isAuthorized (SkillR _) _ = return Authorized
    isAuthorized ApplicantsR _ = setUltDestCurrent >> return Authorized
    isAuthorized (ApplicantR _) _ = return Authorized
    isAuthorized (AppSkillsR _) _ = return Authorized
    isAuthorized (AppSkillsEditR _) _ = return Authorized
    isAuthorized (AppSkillR _ _) _ = return Authorized
    isAuthorized (JobR _) _ = return Authorized
    isAuthorized JobsR _ = setUltDestCurrent >> return Authorized
    isAuthorized JobCreateFormR _ = return Authorized
    isAuthorized (JobEditFormR _) _ = return Authorized
    isAuthorized (JobSkillsR _) _ = return Authorized
    isAuthorized (JobSkillR _ _) _ = return Authorized
    isAuthorized (JobCandidatesR _) _ = setUltDestCurrent >> return Authorized
    isAuthorized (CandidateR _ _) _ = return Authorized
    isAuthorized CandidatesR _ = setUltDestCurrent >> return Authorized
    isAuthorized SkillCreateFormR _ = return Authorized
    isAuthorized (SkillEditFormR _) _ = return Authorized
    isAuthorized (JobSkillsEditFormR _) _ = return Authorized
    isAuthorized SkillsLabelR _ = return Authorized
    isAuthorized DeptsR _ = return Authorized
    isAuthorized (DeptR _) _ = return Authorized
    isAuthorized ApplicantCreateFormR _ = return Authorized
    isAuthorized (ApplicantEditFormR _) _ = return Authorized
    isAuthorized ApplicantTagR _ = return Authorized
    isAuthorized (AppPhotoR _) _ = return Authorized
    isAuthorized (AppSkillsEditFormR _) _ = return Authorized
    isAuthorized (JobCandidateR _ _) _ = return Authorized
    isAuthorized (JobSkillsEditR _) _ = return Authorized
    isAuthorized (ApplicantSkillsR _) _ = return Authorized
    

    -- This function creates static content files in the static folder
    -- and names them based on a hash of their content. This allows
    -- expiration dates to be set far in the future without worry of
    -- users receiving stale content.
    addStaticContent
        :: Text  -- ^ The file extension
        -> Text -- ^ The MIME content type
        -> LByteString -- ^ The contents of the file
        -> Handler (Maybe (Either Text (Route App, [(Text, Text)])))
    addStaticContent ext mime content = do
        master <- getYesod
        let staticDir = appStaticDir $ appSettings master
        addStaticContentExternal
            minifym
            genFileName
            staticDir
            (StaticR . flip StaticRoute [])
            ext
            mime
            content
      where
        -- Generate a unique filename based on the content itself
        genFileName lbs = "autogen-" ++ base64md5 lbs

    -- What messages should be logged. The following includes all messages when
    -- in development, and warnings and errors in production.
    shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
    shouldLogIO app _source level =
        return $
        appShouldLogAll (appSettings app)
            || level == LevelWarn
            || level == LevelError

    makeLogger :: App -> IO Logger
    makeLogger = return . appLogger


isInvisibleButtonNavbarTogglerMainRigt :: Maybe (Route App) -> Bool
isInvisibleButtonNavbarTogglerMainRigt route = case route of
  Just ApplicantCreateFormR -> True
  Just (AppSkillsEditFormR _) -> True
  Just JobCreateFormR -> True
  Just (JobSkillsEditFormR _) -> True
  Just SkillCreateFormR -> True
  Just (SkillEditFormR _) -> True
  Just (SkillR _) -> True
  Just (JobCandidatesR _) -> True
  _otherwise -> False


getServiceWorkerR :: Handler TypedContent
getServiceWorkerR = do
    rndr <- getUrlRenderParams
    return $ TypedContent typeJavascript $ toContent $ $(juliusFile "static/js/sw.julius") rndr


-- How to run database actions.
instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB :: SqlPersistT Handler a -> Handler a
  runDB action = do
    master <- getYesod
    runSqlPool action $ appConnPool master


instance YesodPersistRunner App where
    getDBRunner :: Handler (DBRunner App, Handler ())
    getDBRunner = defaultGetDBRunner appConnPool


instance YesodAuth App where
    type AuthId App = UserId

    -- Where to send a user after successful login
    loginDest :: App -> Route App
    loginDest _ = HomeR
    -- Where to send a user after logout
    logoutDest :: App -> Route App
    logoutDest _ = HomeR
    -- Override the above two destinations when a Referer: header is present
    redirectToReferer :: App -> Bool
    redirectToReferer _ = True

    authenticate :: (MonadHandler m, HandlerSite m ~ App)
                 => Creds App -> m (AuthenticationResult App)
    authenticate creds = liftHandler $ runDB $ do
        x <- getBy $ UniqueUser $ credsIdent creds
        case x of
            Just (Entity uid _) -> return $ Authenticated uid
            Nothing -> Authenticated <$> insert User
                { userIdent = credsIdent creds
                , userPassword = Nothing
                }

    -- You can add other plugins like Google Email, email or OAuth here
    authPlugins :: App -> [AuthPlugin App]
    authPlugins app = [authOpenId Claimed []] ++ extraAuthPlugins
        -- Enable authDummy login if enabled.
        where extraAuthPlugins = [authDummy | appAuthDummyLogin $ appSettings app]


instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
  renderMessage :: App -> [Lang] -> FormMessage -> Text
  renderMessage _ [] = defaultFormMessage
  renderMessage _ ("en":_) = englishFormMessage
  renderMessage _ ("fr":_) = frenchFormMessage
  renderMessage _ ("ro":_) = romanianFormMessage
  renderMessage _ ("ru":_) = russianFormMessage
  renderMessage app (_:xs) = renderMessage app xs

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
    getHttpManager :: App -> Manager
    getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
