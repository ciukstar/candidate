{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
{-# OPTIONS_GHC -Wno-name-shadowing     #-}

module Model where

import ClassyPrelude.Yesod
  ( Eq, Ord, Show, Typeable, Bool, Double, Int, Maybe
  , persistFileWith, mkMigrate, sqlSettings, mkPersist, share
  , ByteString, Day, Textarea, Text
  )
import Database.Persist.Quasi ( lowerCaseSettings )
import Database.Esqueleto.Experimental (SqlString)


share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistFileWith lowerCaseSettings "config/models.persistentmodels")


data Params = Params
  { paramsQ :: Maybe Text
  , paramsOffset :: Maybe Int
  , paramsLimit :: Maybe Int
  , paramsSort :: Maybe (Text,Text)
  , paramsTag :: [Text]
  }


instance SqlString Textarea

ultDestKey :: Text
ultDestKey = "_ULT"
