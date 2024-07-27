{-# LANGUAGE OverloadedStrings #-}

module Demo.DemoDataRO (populateRO) where

import Data.ByteString.Base64 as B64 (decode)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Import.NoFoundation (ReaderT)
import Data.Time.Clock (getCurrentTime, utctDay)
import Data.Time.Calendar (addGregorianYearsClip, addGregorianMonthsClip)
import Database.Persist.Sql (SqlBackend)
import Database.Persist (PersistStoreWrite (insert, insert_))
import Demo.DemoPhotos
  ( man01, man02, man03, man04, man05, man06
  , woman01, woman02, woman03, woman04, woman05
  )

import Model
  ( Dept(Dept)
  , Job(Job, jobDept, jobCode, jobName, jobDayStart, jobDayEnd, jobDescr)
  , Applicant
    ( Applicant, applicantTag, applicantFamilyName, applicantGivenName
    , applicantAdditionalName, applicantBday
    )
  , AppPhoto(AppPhoto, appPhotoMime, appPhotoApplicant, appPhotoPhoto)
  , Skill(Skill, skillLabel, skillCode, skillName, skillDescr)
  , AppSkill(AppSkill, appSkillWeight, appSkillApplicant, appSkillSkill)
  , JobSkill
    ( JobSkill, jobSkillExpanded, jobSkillJob, jobSkillSkill
    , jobSkillWeight, jobSkillParent
    )
  )

populateRO :: MonadIO m => ReaderT SqlBackend m ()
populateRO = do
  today <- liftIO $ utctDay <$> getCurrentTime

  skill01 <- insert $ Skill { skillCode = "Dezvoltare web"
                            , skillName = "Dezvoltare web"
                            , skillDescr = Just "Abilități de dezvoltare web"
                            , skillLabel = Just "Dezvoltare"
                            }
  skill02 <- insert $ Skill { skillCode = "HTML"
                            , skillName = "HTML"
                            , skillDescr = Just "Cunoașterea specificațiilor HTML"
                            , skillLabel = Just "Dezvoltare"
                            }
  skill03 <- insert $ Skill { skillCode = "Proiectare web"
                            , skillName = "Proiectare web"
                            , skillDescr = Just "Principii de bază pentru design web"
                            , skillLabel = Just "Dezvoltare"
                            }
  skill04 <- insert $ Skill { skillCode = "JavaScript"
                            , skillName = "Limbajul de programare JavaScript"
                            , skillDescr = Just "Cunoștințe de programare JavaScript"
                            , skillLabel = Just "Dezvoltare"
                            }
  skill05 <- insert $ Skill { skillCode = "REST"
                            , skillName = "Principii de dezvoltare REST"
                            , skillDescr = Just "Abilități de dezvoltare a aplicațiilor RESTful"
                            , skillLabel = Just "Arhitectură"
                            }
  skill06 <- insert $ Skill { skillCode = "Baze de date"
                            , skillName = "Baze de date"
                            , skillDescr = Just "Abilități de gestionare a bazelor de date"
                            , skillLabel = Just "Baze de date"
                            }
  skill07 <- insert $ Skill { skillCode = "SQL"
                            , skillName = "SQL"
                            , skillDescr = Just "Abilități de scriere a interogărilor SQL"
                            , skillLabel = Just "Baze de date"
                            }
  skill08 <- insert $ Skill { skillCode = "Administrarea bazei de date"
                            , skillName = "Administrarea bazei de date"
                            , skillDescr = Just "Abilități de administrare a bazelor de date"
                            , skillLabel = Just "Baze de date"
                            }

  idIT <- insert $ Dept "Tehnologii informaţionale" Nothing
  jobIT01 <- insert $ Job { jobCode = "Dezvoltator JavaScript"
                          , jobName = "Dezvoltator JavaScript"
                          , jobDayStart = addGregorianMonthsClip (-2) today
                          , jobDayEnd = addGregorianMonthsClip 3 today
                          , jobDescr = Just "Programator JavaScript"
                          , jobDept = Just idIT
                          }
  jobSkill01 <- insert $ JobSkill { jobSkillJob = jobIT01
                                  , jobSkillSkill = skill01
                                  , jobSkillWeight = 0.5
                                  , jobSkillParent = Nothing
                                  , jobSkillExpanded = True
                                  }
  insert_ $ JobSkill { jobSkillJob = jobIT01
                     , jobSkillSkill = skill04
                     , jobSkillWeight = 0.8
                     , jobSkillParent = Just jobSkill01
                     , jobSkillExpanded = True
                     }
  insert_ $ JobSkill { jobSkillJob = jobIT01
                     , jobSkillSkill = skill02
                     , jobSkillWeight = 0.1
                     , jobSkillParent = Just jobSkill01
                     , jobSkillExpanded = True
                     }
  insert_ $ JobSkill { jobSkillJob = jobIT01
                     , jobSkillSkill = skill03
                     , jobSkillWeight = 0.1
                     , jobSkillParent = Just jobSkill01
                     , jobSkillExpanded = True
                     }
  insert_ $ JobSkill { jobSkillJob = jobIT01
                     , jobSkillSkill = skill05
                     , jobSkillWeight = 0.3
                     , jobSkillParent = Nothing
                     , jobSkillExpanded = True
                     }
  jobSkill03 <- insert $ JobSkill { jobSkillJob = jobIT01
                                  , jobSkillSkill = skill06
                                  , jobSkillWeight = 0.2
                                  , jobSkillParent = Nothing
                                  , jobSkillExpanded = True
                                  }
  insert_ $ JobSkill { jobSkillJob = jobIT01
                     , jobSkillSkill = skill07
                     , jobSkillWeight = 0.8
                     , jobSkillParent = Just jobSkill03
                     , jobSkillExpanded = True
                     }
  insert_ $ JobSkill { jobSkillJob = jobIT01
                     , jobSkillSkill = skill08
                     , jobSkillWeight = 0.2
                     , jobSkillParent = Just jobSkill03
                     , jobSkillExpanded = True
                     }

  skill09 <- insert $ Skill { skillCode = "Dezvoltare backend"
                            , skillName = "Dezvoltare backend"
                            , skillDescr = Just "Dezvoltarea logicii de afaceri"
                            , skillLabel  = Just "Dezvoltare"
                            }
  skill010 <- insert $ Skill { skillCode = "JavaSE"
                            , skillName = "Java Standard Edition"
                            , skillDescr = Just "Cunoașterea Java 8+ Standard Edition"
                            , skillLabel  = Just "Dezvoltare"
                            }
  skill011 <- insert $ Skill { skillCode = "JakartaEE"
                            , skillName = "Jakarta Enterprise Edition"
                            , skillDescr = Just "Jakarta EE abilități de dezvoltare a aplicațiilor"
                            , skillLabel  = Just "Dezvoltare"
                            }
  skill012 <- insert $ Skill { skillCode = "Dezvoltare front-end"
                             , skillName = "Dezvoltare front-end"
                             , skillDescr = Just "Dezvoltarea interfeței cu utilizatorul web"
                             , skillLabel  = Just "Tehnologii web"
                             }
  skill013 <- insert $ Skill { skillCode = "CSS"
                             , skillName = "CSS"
                             , skillDescr = Just "Foi de stil în cascadă"
                             , skillLabel  = Just "Tehnologii web"
                             }

  job02 <- insert $ Job { jobCode = "Dezvoltator JakartaEE"
                        , jobName = "Dezvoltator Jakarta Enterprise Edition"
                        , jobDayStart = addGregorianMonthsClip (-3) today
                        , jobDayEnd = addGregorianMonthsClip 2 today
                        , jobDescr = Just "Programator Java Enterprise Edition"
                        , jobDept = Just idIT
                        }
  jobSkill02 <- insert $ JobSkill { jobSkillJob = job02
                                  , jobSkillSkill = skill09
                                  , jobSkillWeight = 0.7
                                  , jobSkillParent = Nothing
                                  , jobSkillExpanded = True
                                  }
  insert_ $ JobSkill { jobSkillJob = job02
                     , jobSkillSkill = skill010
                     , jobSkillWeight = 0.6
                     , jobSkillParent = Just jobSkill02
                     , jobSkillExpanded = True
                     }
  insert_ $ JobSkill { jobSkillJob = job02
                     , jobSkillSkill = skill011
                     , jobSkillWeight = 0.4
                     , jobSkillParent = Just jobSkill02
                     , jobSkillExpanded = True
                     }
  jobSkill04 <- insert $ JobSkill { jobSkillJob = job02
                                  , jobSkillSkill = skill06
                                  , jobSkillWeight = 0.2
                                  , jobSkillParent = Nothing
                                  , jobSkillExpanded = True
                                  }
  insert_ $ JobSkill { jobSkillJob = job02
                     , jobSkillSkill = skill07
                     , jobSkillWeight = 0.8
                     , jobSkillParent = Just jobSkill04
                     , jobSkillExpanded = True
                     }
  insert_ $ JobSkill { jobSkillJob = job02
                     , jobSkillSkill = skill08
                     , jobSkillWeight = 0.2
                     , jobSkillParent = Just jobSkill04
                     , jobSkillExpanded = True
                     }

  jobSkill05 <- insert $ JobSkill { jobSkillJob = job02
                                  , jobSkillSkill = skill012
                                  , jobSkillWeight = 0.1
                                  , jobSkillParent = Nothing
                                  , jobSkillExpanded = True
                                  }
  insert_ $ JobSkill { jobSkillJob = job02
                     , jobSkillSkill = skill02
                     , jobSkillWeight = 0.5
                     , jobSkillParent = Just jobSkill05
                     , jobSkillExpanded = True
                     }
  insert_ $ JobSkill { jobSkillJob = job02
                     , jobSkillSkill = skill013
                     , jobSkillWeight = 0.3
                     , jobSkillParent = Just jobSkill05
                     , jobSkillExpanded = True
                     }
  insert_ $ JobSkill { jobSkillJob = job02
                     , jobSkillSkill = skill04
                     , jobSkillWeight = 0.2
                     , jobSkillParent = Just jobSkill05
                     , jobSkillExpanded = True
                     }

  appl01 <- insert $ Applicant
    { applicantFamilyName = "Popescu"
    , applicantGivenName = "Ion"
    , applicantAdditionalName = Just "Andrei"
    , applicantBday = Just $ addGregorianYearsClip (-28) today
    , applicantTag = Just "Angajați noi"
    }

  case B64.decode man01 of
    Left _ -> return ()
    Right photo -> insert_ $ AppPhoto
      { appPhotoApplicant = appl01
      , appPhotoPhoto = photo
      , appPhotoMime = "image/avif"
      }

  insert_ $ AppSkill { appSkillApplicant = appl01
                     , appSkillSkill = skill04
                     , appSkillWeight = 0.9
                     }
  insert_ $ AppSkill { appSkillApplicant = appl01
                     , appSkillSkill = skill02
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl01
                     , appSkillSkill = skill03
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl01
                     , appSkillSkill = skill05
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl01
                     , appSkillSkill = skill07
                     , appSkillWeight = 1
                     }

  appl02 <- insert $ Applicant
    { applicantFamilyName = "Radu"
    , applicantGivenName = "Ana-Maria"
    , applicantAdditionalName = Nothing
    , applicantBday = Just $ addGregorianYearsClip (-26) today
    , applicantTag = Just "Angajați noi"
    }

  case B64.decode woman01 of
    Left _ -> return ()
    Right photo -> insert_ $ AppPhoto
      { appPhotoApplicant = appl02
      , appPhotoPhoto = photo
      , appPhotoMime = "image/avif"
      }

  insert_ $ AppSkill { appSkillApplicant = appl02
                     , appSkillSkill = skill04
                     , appSkillWeight = 0.8
                     }
  insert_ $ AppSkill { appSkillApplicant = appl02
                     , appSkillSkill = skill02
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl02
                     , appSkillSkill = skill03
                     , appSkillWeight = 0.5
                     }
  insert_ $ AppSkill { appSkillApplicant = appl02
                     , appSkillSkill = skill05
                     , appSkillWeight = 0.7
                     }
  insert_ $ AppSkill { appSkillApplicant = appl02
                     , appSkillSkill = skill07
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl02
                     , appSkillSkill = skill08
                     , appSkillWeight = 1
                     }

  appl03 <- insert $ Applicant
    { applicantFamilyName = "Popa"
    , applicantGivenName = "Andrei"
    , applicantAdditionalName = Nothing
    , applicantBday = Just $ addGregorianYearsClip (-21) today
    , applicantTag = Just "Profesionişti"
    }

  case B64.decode man02 of
    Left _ -> return ()
    Right photo -> insert_ $ AppPhoto
      { appPhotoApplicant = appl03
      , appPhotoPhoto = photo
      , appPhotoMime = "image/avif"
      }

  insert_ $ AppSkill { appSkillApplicant = appl03
                     , appSkillSkill = skill04
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl03
                     , appSkillSkill = skill02
                     , appSkillWeight = 0.5
                     }
  insert_ $ AppSkill { appSkillApplicant = appl03
                     , appSkillSkill = skill03
                     , appSkillWeight = 0.1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl03
                     , appSkillSkill = skill05
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl03
                     , appSkillSkill = skill07
                     , appSkillWeight = 0.55
                     }
  insert_ $ AppSkill { appSkillApplicant = appl03
                     , appSkillSkill = skill08
                     , appSkillWeight = 0.56
                     }

  appl04 <- insert $ Applicant
    { applicantFamilyName = "Stoica"
    , applicantGivenName = "Maria"
    , applicantAdditionalName = Just "Alexandra"
    , applicantBday = Just $ addGregorianYearsClip (-30) today
    , applicantTag = Just "Profesionişti"
    }

  case B64.decode woman02 of
    Left _ -> return ()
    Right photo -> insert_ $ AppPhoto
      { appPhotoApplicant = appl04
      , appPhotoPhoto = photo
      , appPhotoMime = "image/avif"
      }

  insert_ $ AppSkill { appSkillApplicant = appl04
                     , appSkillSkill = skill04
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl04
                     , appSkillSkill = skill02
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl04
                     , appSkillSkill = skill03
                     , appSkillWeight = 1
                     }

  appl05 <- insert $ Applicant
    { applicantFamilyName = "Ionescu"
    , applicantGivenName = "Alexandru"
    , applicantAdditionalName = Just "Victor"
    , applicantBday = Just $ addGregorianYearsClip (-32) today
    , applicantTag = Just "Experți"
    }

  case B64.decode man03 of
    Left _ -> return ()
    Right photo -> insert_ $ AppPhoto
      { appPhotoApplicant = appl05
      , appPhotoPhoto = photo
      , appPhotoMime = "image/avif"
      }

  insert_ $ AppSkill { appSkillApplicant = appl05
                     , appSkillSkill = skill07
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl05
                     , appSkillSkill = skill08
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl05
                     , appSkillSkill = skill010
                     , appSkillWeight = 0.8
                     }

  appl06 <- insert $ Applicant
    { applicantFamilyName = "Rusu"
    , applicantGivenName = "Ştefan"
    , applicantAdditionalName = Just "Alexandru"
    , applicantBday = Just $ addGregorianYearsClip (-39) today
    , applicantTag = Just "Experți"
    }

  case B64.decode man04 of
    Left _ -> return ()
    Right photo -> insert_ $ AppPhoto
      { appPhotoApplicant = appl06
      , appPhotoPhoto = photo
      , appPhotoMime = "image/avif"
      }

  insert_ $ AppSkill { appSkillApplicant = appl06
                     , appSkillSkill = skill07
                     , appSkillWeight = 0.6
                     }
  insert_ $ AppSkill { appSkillApplicant = appl06
                     , appSkillSkill = skill08
                     , appSkillWeight = 0.2
                     }
  insert_ $ AppSkill { appSkillApplicant = appl06
                     , appSkillSkill = skill010
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl06
                     , appSkillSkill = skill011
                     , appSkillWeight = 0.5
                     }
  insert_ $ AppSkill { appSkillApplicant = appl06
                     , appSkillSkill = skill04
                     , appSkillWeight = 1
                     }

  appl07 <- insert $ Applicant
    { applicantFamilyName = "Marin"
    , applicantGivenName = "Ioana"
    , applicantAdditionalName = Nothing
    , applicantBday = Just $ addGregorianYearsClip (-35) today
    , applicantTag = Just "Experți"
    }

  case B64.decode woman03 of
    Left _ -> return ()
    Right photo -> insert_ $ AppPhoto
      { appPhotoApplicant = appl07
      , appPhotoPhoto = photo
      , appPhotoMime = "image/avif"
      }
  insert_ $ AppSkill { appSkillApplicant = appl07
                     , appSkillSkill = skill010
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl07
                     , appSkillSkill = skill011
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl07
                     , appSkillSkill = skill07
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl07
                     , appSkillSkill = skill02
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl07
                     , appSkillSkill = skill04
                     , appSkillWeight = 0.8
                     }
  insert_ $ AppSkill { appSkillApplicant = appl07
                     , appSkillSkill = skill013
                     , appSkillWeight = 1
                     }

  dept02 <- insert $ Dept "Management" Nothing
  _ <- insert $ Dept "Marketing" Nothing
  _ <- insert $ Dept "Vânzări" Nothing
  _ <- insert $ Dept "Contabilitate" Nothing
  _ <- insert $ Dept "Finanțe" Nothing
  _ <- insert $ Dept "Productie" Nothing
  _ <- insert $ Dept "Cercetare și dezvoltare" Nothing
  _ <- insert $ Dept "Resurse umane" Nothing


  skill14 <- insert $ Skill { skillCode = "Administrarea procesului"
                            , skillName = "Administrarea procesului"
                            , skillDescr = Just "Crearea, lansarea și dezvoltarea unui proiect"
                            , skillLabel = Just "Management"
                            }
  skill15 <- insert $ Skill { skillCode = "Managementul sarcinilor"
                            , skillName = "Managementul sarcinilor"
                            , skillDescr = Just "Stabilirea sarcinilor și monitorizarea progresului acestora"
                            , skillLabel = Just "Management"
                            }
  skill16 <- insert $ Skill { skillCode = "Metodologia managementului"
                            , skillName = "Cunoașterea metodologiilor de management"
                            , skillDescr = Just "Cunoașterea metodologiilor de management"
                            , skillLabel = Just "Management"
                            }
  skill17 <- insert $ Skill { skillCode = "Managementul echipei"
                            , skillName = "Managementul echipei"
                            , skillDescr = Just "Unește echipa, distribuie munca, analizează rezultatele"
                            , skillLabel = Just "Management"
                            }
  skill18 <- insert $ Skill { skillCode = "Expertiza tehnica"
                            , skillName = "Expertiza tehnica"
                            , skillDescr = Just "Înțelegerea produsului în sine și a tuturor aspectelor dezvoltării acestuia"
                            , skillLabel = Just "Management"
                            }
  skill19 <- insert $ Skill { skillCode = "Managementul documentelor"
                            , skillName = "Managementul documentelor"
                            , skillDescr = Just "Raportare"
                            , skillLabel = Just "Management"
                            }
  skill20 <- insert $ Skill { skillCode = "Planificare"
                            , skillName = "Planificare"
                            , skillDescr = Just "Definirea bugetelor si a resurselor necesare"
                            , skillLabel = Just "Management"
                            }
  skill21 <- insert $ Skill { skillCode = "Managementul riscului"
                            , skillName = "Managementul riscului"
                            , skillDescr = Just "Anticiparea riscurilor și posibilitatea de a le evita"
                            , skillLabel = Just "Management"
                            }
  skill22 <- insert $ Skill { skillCode = "Gestionarea bugetului și a costurilor"
                            , skillName = "Gestionarea bugetului și a costurilor"
                            , skillDescr = Just "A fi capabil să se încadreze în constrângerile bugetare fără a afecta fluxurile de lucru"
                            , skillLabel = Just "Management"
                            }

  job03 <- insert $ Job { jobCode = "Manager de proiect"
                        , jobName = "Manager de proiect"
                        , jobDayStart = today
                        , jobDayEnd = addGregorianMonthsClip 4 today
                        , jobDescr = Just "Manager de proiect IT"
                        , jobDept = Just dept02
                        }
  insert_ $ JobSkill { jobSkillJob = job03
                     , jobSkillSkill = skill14
                     , jobSkillWeight = 0.2
                     , jobSkillParent = Nothing
                     , jobSkillExpanded = True
                     }
  insert_ $ JobSkill { jobSkillJob = job03
                     , jobSkillSkill = skill15
                     , jobSkillWeight = 0.1
                     , jobSkillParent = Nothing
                     , jobSkillExpanded = True
                     }
  insert_ $ JobSkill { jobSkillJob = job03
                     , jobSkillSkill = skill16
                     , jobSkillWeight = 0.1
                     , jobSkillParent = Nothing
                     , jobSkillExpanded = True
                     }
  insert_ $ JobSkill { jobSkillJob = job03
                     , jobSkillSkill = skill17
                     , jobSkillWeight = 0.1
                     , jobSkillParent = Nothing
                     , jobSkillExpanded = True
                     }
  insert_ $ JobSkill { jobSkillJob = job03
                     , jobSkillSkill = skill18
                     , jobSkillWeight = 0.1
                     , jobSkillParent = Nothing
                     , jobSkillExpanded = True
                     }
  insert_ $ JobSkill { jobSkillJob = job03
                     , jobSkillSkill = skill19
                     , jobSkillWeight = 0.1
                     , jobSkillParent = Nothing
                     , jobSkillExpanded = True
                     }
  insert_ $ JobSkill { jobSkillJob = job03
                     , jobSkillSkill = skill20
                     , jobSkillWeight = 0.1
                     , jobSkillParent = Nothing
                     , jobSkillExpanded = True
                     }
  insert_ $ JobSkill { jobSkillJob = job03
                     , jobSkillSkill = skill21
                     , jobSkillWeight = 0.1
                     , jobSkillParent = Nothing
                     , jobSkillExpanded = True
                     }
  insert_ $ JobSkill { jobSkillJob = job03
                     , jobSkillSkill = skill22
                     , jobSkillWeight = 0.1
                     , jobSkillParent = Nothing
                     , jobSkillExpanded = True
                     }

  appl08 <- insert $ Applicant
    { applicantFamilyName = "Matei"
    , applicantGivenName = "Andreea"
    , applicantAdditionalName = Just "Alexandra"
    , applicantBday = Just $ addGregorianYearsClip (-42) today
    , applicantTag = Just "Directori"
    }

  case B64.decode woman04 of
    Left _ -> return ()
    Right photo -> insert_ $ AppPhoto
      { appPhotoApplicant = appl08
      , appPhotoPhoto = photo
      , appPhotoMime = "image/avif"
      }
  insert_ $ AppSkill { appSkillApplicant = appl08
                     , appSkillSkill = skill14
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl08
                     , appSkillSkill = skill15
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl08
                     , appSkillSkill = skill16
                     , appSkillWeight = 0.4
                     }
  insert_ $ AppSkill { appSkillApplicant = appl08
                     , appSkillSkill = skill17
                     , appSkillWeight = 0.68
                     }
  insert_ $ AppSkill { appSkillApplicant = appl08
                     , appSkillSkill = skill18
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl08
                     , appSkillSkill = skill19
                     , appSkillWeight = 0
                     }
  insert_ $ AppSkill { appSkillApplicant = appl08
                     , appSkillSkill = skill20
                     , appSkillWeight = 0.8
                     }
  insert_ $ AppSkill { appSkillApplicant = appl08
                     , appSkillSkill = skill21
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl08
                     , appSkillSkill = skill22
                     , appSkillWeight = 1
                     }

  insert_ $ AppSkill { appSkillApplicant = appl07
                     , appSkillSkill = skill14
                     , appSkillWeight = 0.7
                     }
  insert_ $ AppSkill { appSkillApplicant = appl07
                     , appSkillSkill = skill15
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl07
                     , appSkillSkill = skill16
                     , appSkillWeight = 0.6
                     }
  insert_ $ AppSkill { appSkillApplicant = appl07
                     , appSkillSkill = skill17
                     , appSkillWeight = 0.8
                     }
  insert_ $ AppSkill { appSkillApplicant = appl07
                     , appSkillSkill = skill18
                     , appSkillWeight = 0.5
                     }
  insert_ $ AppSkill { appSkillApplicant = appl07
                     , appSkillSkill = skill19
                     , appSkillWeight = 0.5
                     }
  insert_ $ AppSkill { appSkillApplicant = appl07
                     , appSkillSkill = skill21
                     , appSkillWeight = 1
                     }


  skill23 <- insert $ Skill { skillCode = "OS"
                            , skillName = "Sisteme de operare"
                            , skillDescr = Just "Cunostinte de administrare a sistemului de operare"
                            , skillLabel = Just "DevOps"
                            }
  skill24 <- insert $ Skill { skillCode = "OS Linux"
                            , skillName = "Sistem de operare Linux"
                            , skillDescr = Just "Abilități de administrare Linux"
                            , skillLabel = Just "DevOps"
                            }
  skill25 <- insert $ Skill { skillCode = "OS Windows"
                            , skillName = "Sistem de operare Windows Server"
                            , skillDescr = Just "Cunoștințe de administrare Windows Server"
                            , skillLabel = Just "DevOps"
                            }
  skill26 <- insert $ Skill { skillCode = "PostgreSQL"
                            , skillName = "Administrare PostgreSQL"
                            , skillDescr = Just "Abilități de administrare PostgreSQL"
                            , skillLabel = Just "Baze de date"
                            }
  skill27 <- insert $ Skill { skillCode = "Oracle DBA"
                            , skillName = "Administrarea bazei de date Oracle"
                            , skillDescr = Just "Abilități de administrare a bazelor de date Oracle"
                            , skillLabel = Just "Baze de date"
                            }
  skill28 <- insert $ Skill { skillCode = "Administrare MySQL"
                            , skillName = "Administrarea bazei de date MySQL"
                            , skillDescr = Just "Cunoștințe de administrare a bazelor de date MySQL"
                            , skillLabel = Just "Baze de date"
                            }
  skill29 <- insert $ Skill { skillCode = "Containerizarea"
                            , skillName = "Containerizarea"
                            , skillDescr = Just "Impachetarea si implementarea software-ului"
                            , skillLabel = Just "DevOps"
                            }
  skill30 <- insert $ Skill { skillCode = "Docker"
                            , skillName = "Docker"
                            , skillDescr = Just "Abilități pentru utilizarea Docker pentru a împacheta și a implementa aplicații"
                            , skillLabel = Just "DevOps"
                            }
  skill31 <- insert $ Skill { skillCode = "Docker Compose"
                            , skillName = "Docker Compose"
                            , skillDescr = Just "Definirea și rularea aplicațiilor Docker cu mai multe containere"
                            , skillLabel = Just "DevOps"
                            }
  skill32 <- insert $ Skill { skillCode = "Kubernetes"
                            , skillName = "Kubernetes"
                            , skillDescr = Just "Automatizarea implementării, scalarea și gestionarea software-ului"
                            , skillLabel = Just "DevOps"
                            }
  skill33 <- insert $ Skill { skillCode = "Abilități cloud"
                            , skillName = "Abilități cloud"
                            , skillDescr = Just "Abilități pentru implementarea unei aplicații în cloud"
                            , skillLabel = Just "DevOps"
                            }
  skill34 <- insert $ Skill { skillCode = "GCP PaaS"
                            , skillName = "Google Cloud Platform"
                            , skillDescr = Just "Abilități pentru implementarea unei aplicații pe Google Cloud"
                            , skillLabel = Just "Cloud"
                            }
  skill35 <- insert $ Skill { skillCode = "AWS PaaS"
                            , skillName = "Amazon Web Services"
                            , skillDescr = Just "Abilități pentru implementarea unei aplicații pe Amazon Web Services"
                            , skillLabel = Just "Cloud"
                            }
  skill36 <- insert $ Skill { skillCode = "Heroku PaaS"
                            , skillName = "Heroku Cloud Application Platform"
                            , skillDescr = Just "Abilități pentru implementarea unei aplicații pe Heroku"
                            , skillLabel = Just "Cloud"
                            }
  skill37 <- insert $ Skill { skillCode = "Sisteme de control al versiunilor"
                            , skillName = "Sisteme de control al versiunilor"
                            , skillDescr = Just "Abilități pentru gestionarea modificărilor la programe și documente de calculator"
                            , skillLabel = Just "DevOps"
                            }
  skill38 <- insert $ Skill { skillCode = "Git"
                            , skillName = "Sistemul de control al versiunilor Git"
                            , skillDescr = Just "Abilități pentru gestionarea modificărilor cu Git"
                            , skillLabel = Just "DevOps"
                            }
  skill39 <- insert $ Skill { skillCode = "Mercurial"
                            , skillName = "Sistemul de control al versiunilor Mercurial"
                            , skillDescr = Just "Abilități pentru gestionarea schimbărilor cu Mercurial"
                            , skillLabel = Just "DevOps"
                            }
  skill40 <- insert $ Skill { skillCode = "Apache Subversion"
                            , skillName = "Apache Subversion VCS"
                            , skillDescr = Just "Abilități pentru gestionarea schimbărilor cu Apache Subversion"
                            , skillLabel = Just "DevOps"
                            }

  job04 <- insert $ Job { jobCode = "Inginer DevOps"
                        , jobName = "Inginer DevOps"
                        , jobDayStart = addGregorianMonthsClip (-2) today
                        , jobDayEnd = addGregorianMonthsClip 2 today
                        , jobDescr = Just "Inginer de integrare şi automatizare pentru dezvoltare software şi operaţiuni IT"
                        , jobDept = Just idIT
                        }
  jobSkill06 <- insert $ JobSkill { jobSkillJob = job04
                                  , jobSkillSkill = skill23
                                  , jobSkillWeight = 0.2
                                  , jobSkillParent = Nothing
                                  , jobSkillExpanded = True
                                  }
  insert_ $ JobSkill { jobSkillJob = job04
                     , jobSkillSkill = skill24
                     , jobSkillWeight = 0.75
                     , jobSkillParent = Just jobSkill06
                     , jobSkillExpanded = True
                     }
  insert_ $ JobSkill { jobSkillJob = job04
                     , jobSkillSkill = skill25
                     , jobSkillWeight = 0.25
                     , jobSkillParent = Just jobSkill06
                     , jobSkillExpanded = True
                     }
  jobSkill07 <- insert $ JobSkill { jobSkillJob = job04
                                  , jobSkillSkill = skill08
                                  , jobSkillWeight = 0.2
                                  , jobSkillParent = Nothing
                                  , jobSkillExpanded = False
                                  }
  insert_ $ JobSkill { jobSkillJob = job04
                     , jobSkillSkill = skill26
                     , jobSkillWeight = 0.5
                     , jobSkillParent = Just jobSkill07
                     , jobSkillExpanded = True
                     }
  insert_ $ JobSkill { jobSkillJob = job04
                     , jobSkillSkill = skill27
                     , jobSkillWeight = 0.25
                     , jobSkillParent = Just jobSkill07
                     , jobSkillExpanded = True
                     }
  insert_ $ JobSkill { jobSkillJob = job04
                     , jobSkillSkill = skill28
                     , jobSkillWeight = 0.25
                     , jobSkillParent = Just jobSkill07
                     , jobSkillExpanded = True
                     }
  jobSkill08 <- insert $ JobSkill { jobSkillJob = job04
                                  , jobSkillSkill = skill29
                                  , jobSkillWeight = 0.4
                                  , jobSkillParent = Nothing
                                  , jobSkillExpanded = False
                                  }
  insert_ $ JobSkill { jobSkillJob = job04
                     , jobSkillSkill = skill30
                     , jobSkillWeight = 0.5
                     , jobSkillParent = Just jobSkill08
                     , jobSkillExpanded = True
                     }
  insert_ $ JobSkill { jobSkillJob = job04
                     , jobSkillSkill = skill31
                     , jobSkillWeight = 0.25
                     , jobSkillParent = Just jobSkill08
                     , jobSkillExpanded = True
                     }
  insert_ $ JobSkill { jobSkillJob = job04
                     , jobSkillSkill = skill32
                     , jobSkillWeight = 0.25
                     , jobSkillParent = Just jobSkill08
                     , jobSkillExpanded = True
                     }
  jobSkill09 <- insert $ JobSkill { jobSkillJob = job04
                                  , jobSkillSkill = skill33
                                  , jobSkillWeight = 0.1
                                  , jobSkillParent = Nothing
                                  , jobSkillExpanded = False
                                  }
  insert_ $ JobSkill { jobSkillJob = job04
                     , jobSkillSkill = skill34
                     , jobSkillWeight = 0.4
                     , jobSkillParent = Just jobSkill09
                     , jobSkillExpanded = True
                     }
  insert_ $ JobSkill { jobSkillJob = job04
                     , jobSkillSkill = skill35
                     , jobSkillWeight = 0.4
                     , jobSkillParent = Just jobSkill09
                     , jobSkillExpanded = True
                     }
  insert_ $ JobSkill { jobSkillJob = job04
                     , jobSkillSkill = skill36
                     , jobSkillWeight = 0.2
                     , jobSkillParent = Just jobSkill09
                     , jobSkillExpanded = True
                     }
  jobSkill10 <- insert $ JobSkill { jobSkillJob = job04
                                  , jobSkillSkill = skill37
                                  , jobSkillWeight = 0.1
                                  , jobSkillParent = Nothing
                                  , jobSkillExpanded = False
                                  }
  insert_ $ JobSkill { jobSkillJob = job04
                     , jobSkillSkill = skill38
                     , jobSkillWeight = 0.8
                     , jobSkillParent = Just jobSkill10
                     , jobSkillExpanded = True
                     }
  insert_ $ JobSkill { jobSkillJob = job04
                     , jobSkillSkill = skill39
                     , jobSkillWeight = 0.1
                     , jobSkillParent = Just jobSkill10
                     , jobSkillExpanded = True
                     }
  insert_ $ JobSkill { jobSkillJob = job04
                     , jobSkillSkill = skill40
                     , jobSkillWeight = 0.1
                     , jobSkillParent = Just jobSkill10
                     , jobSkillExpanded = True
                     }

  appl09 <- insert $ Applicant
    { applicantFamilyName = "Munteanu"
    , applicantGivenName = "David"
    , applicantAdditionalName = Nothing
    , applicantBday = Just $ addGregorianYearsClip (-46) today
    , applicantTag = Just "DevOps"
    }

  case B64.decode man05 of
    Left _ -> return ()
    Right photo -> insert_ $ AppPhoto
      { appPhotoApplicant = appl09
      , appPhotoPhoto = photo
      , appPhotoMime = "image/avif"
      }
  insert_ $ AppSkill { appSkillApplicant = appl09
                     , appSkillSkill = skill24
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl09
                     , appSkillSkill = skill26
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl09
                     , appSkillSkill = skill27
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl09
                     , appSkillSkill = skill28
                     , appSkillWeight = 0.5
                     }
  insert_ $ AppSkill { appSkillApplicant = appl09
                     , appSkillSkill = skill30
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl09
                     , appSkillSkill = skill31
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl09
                     , appSkillSkill = skill34
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl09
                     , appSkillSkill = skill35
                     , appSkillWeight = 0.8
                     }
  insert_ $ AppSkill { appSkillApplicant = appl09
                     , appSkillSkill = skill36
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl09
                     , appSkillSkill = skill38
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl09
                     , appSkillSkill = skill40
                     , appSkillWeight = 0.2
                     }
  appl10 <- insert $ Applicant
    { applicantFamilyName = "Ciobanu"
    , applicantGivenName = "Ionuţ"
    , applicantAdditionalName = Just "Ştefan"
    , applicantBday = Just $ addGregorianYearsClip (-39) today
    , applicantTag = Just "DevOps"
    }

  case B64.decode man06 of
    Left _ -> return ()
    Right photo -> insert_ $ AppPhoto
      { appPhotoApplicant = appl10
      , appPhotoPhoto = photo
      , appPhotoMime = "image/avif"
      }
  insert_ $ AppSkill { appSkillApplicant = appl10
                     , appSkillSkill = skill24
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl10
                     , appSkillSkill = skill25
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl10
                     , appSkillSkill = skill26
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl10
                     , appSkillSkill = skill28
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl10
                     , appSkillSkill = skill30
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl10
                     , appSkillSkill = skill34
                     , appSkillWeight = 0.5
                     }
  insert_ $ AppSkill { appSkillApplicant = appl10
                     , appSkillSkill = skill35
                     , appSkillWeight = 0.5
                     }
  insert_ $ AppSkill { appSkillApplicant = appl10
                     , appSkillSkill = skill36
                     , appSkillWeight = 0.5
                     }
  insert_ $ AppSkill { appSkillApplicant = appl10
                     , appSkillSkill = skill38
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl10
                     , appSkillSkill = skill39
                     , appSkillWeight = 1
                     }
  appl11 <- insert $ Applicant
    { applicantFamilyName = "Florea"
    , applicantGivenName = "Ioana"
    , applicantAdditionalName = Just "Maria"
    , applicantBday = Just $ addGregorianYearsClip (-33) today
    , applicantTag = Just "DevOps"
    }

  case B64.decode woman05 of
    Left _ -> return ()
    Right photo -> insert_ $ AppPhoto
      { appPhotoApplicant = appl11
      , appPhotoPhoto = photo
      , appPhotoMime = "image/avif"
      }
  insert_ $ AppSkill { appSkillApplicant = appl11
                     , appSkillSkill = skill24
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl11
                     , appSkillSkill = skill26
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl11
                     , appSkillSkill = skill30
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl11
                     , appSkillSkill = skill35
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl11
                     , appSkillSkill = skill39
                     , appSkillWeight = 1
                     }
  insert_ $ AppSkill { appSkillApplicant = appl11
                     , appSkillSkill = skill40
                     , appSkillWeight = 1
                     }

  return ()
