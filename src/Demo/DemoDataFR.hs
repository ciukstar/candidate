{-# LANGUAGE OverloadedStrings #-}

module Demo.DemoDataFR (populateFR) where

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

populateFR :: MonadIO m => ReaderT SqlBackend m ()
populateFR = do
  today <- liftIO $ utctDay <$> getCurrentTime

  skill01 <- insert $ Skill { skillCode = "Développement web"
                            , skillName = "Développement web"
                            , skillDescr = Just "Compétences en développement Web"
                            , skillLabel = Just "Développement"
                            }
  skill02 <- insert $ Skill { skillCode = "HTML"
                            , skillName = "HTML"
                            , skillDescr = Just "Connaissance de la spécification HTML"
                            , skillLabel = Just "Développement"
                            }
  skill03 <- insert $ Skill { skillCode = "Conception Web"
                            , skillName = "Conception Web"
                            , skillDescr = Just "Principes de base de la conception Web"
                            , skillLabel = Just "Développement"
                            }
  skill04 <- insert $ Skill { skillCode = "JavaScript"
                            , skillName = "Langage de programmation JavaScript"
                            , skillDescr = Just "Compétences en programmation JavaScript"
                            , skillLabel = Just "Développement"
                            }
  skill05 <- insert $ Skill { skillCode = "REST"
                            , skillName = "Principes de développement REST"
                            , skillDescr = Just "Compétences en développement d'applications RESTful"
                            , skillLabel = Just "Architecture"
                            }
  skill06 <- insert $ Skill { skillCode = "Bases de données"
                            , skillName = "Bases de données"
                            , skillDescr = Just "Compétences en base de données"
                            , skillLabel = Just "Bases de données"
                            }
  skill07 <- insert $ Skill { skillCode = "SQL"
                            , skillName = "SQL"
                            , skillDescr = Just "Compétences en écriture de requête SQL"
                            , skillLabel = Just "Bases de données"
                            }
  skill08 <- insert $ Skill { skillCode = "Administration des bases de données"
                            , skillName = "Administration des bases de données"
                            , skillDescr = Just "Compétences en administration de base de données"
                            , skillLabel = Just "Bases de données"
                            }

  idIT <- insert $ Dept "Technologie de l'information" Nothing
  jobIT01 <- insert $ Job { jobCode = "Développeur JavaScript"
                          , jobName = "Développeur JavaScript"
                          , jobDayStart = addGregorianMonthsClip (-2) today
                          , jobDayEnd = addGregorianMonthsClip 3 today
                          , jobDescr = Just "Programmeur JavaScript"
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

  skill09 <- insert $ Skill { skillCode = "Développement back-end"
                            , skillName = "Développement back-end"
                            , skillDescr = Just "Développement de la logique métier"
                            , skillLabel  = Just "Développement"
                            }
  skill010 <- insert $ Skill { skillCode = "JavaSE"
                            , skillName = "Java Standard Edition"
                            , skillDescr = Just "Connaissance de Java 8+ Standard Edition"
                            , skillLabel  = Just "Développement"
                            }
  skill011 <- insert $ Skill { skillCode = "JakartaEE"
                            , skillName = "Jakarta Enterprise Edition"
                            , skillDescr = Just "Compétences en développement d'applications Jakarta EE"
                            , skillLabel  = Just "Développement"
                            }
  skill012 <- insert $ Skill { skillCode = "Développement front-end"
                             , skillName = "Développement front-end"
                             , skillDescr = Just "Développement d'interface utilisateur Web"
                             , skillLabel  = Just "Technologies Web"
                             }
  skill013 <- insert $ Skill { skillCode = "CSS"
                             , skillName = "CSS"
                             , skillDescr = Just "Feuilles de style en cascade"
                             , skillLabel  = Just "Technologies Web"
                             }

  job02 <- insert $ Job { jobCode = "Développeur JakartaEE"
                        , jobName = "Développeur Jakarta Enterprise Edition"
                        , jobDayStart = addGregorianMonthsClip (-3) today
                        , jobDayEnd = addGregorianMonthsClip 2 today
                        , jobDescr = Just "Programmeur Java Enterprise Edition"
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
    { applicantFamilyName = "Martin"
    , applicantGivenName = "Léo"
    , applicantAdditionalName = Nothing
    , applicantBday = Just $ addGregorianYearsClip (-28) today
    , applicantTag = Just "Nouveaux employés"
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
    { applicantFamilyName = "Bernard"
    , applicantGivenName = "Jade"
    , applicantAdditionalName = Nothing
    , applicantBday = Just $ addGregorianYearsClip (-26) today
    , applicantTag = Just "Nouveaux employés"
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
    { applicantFamilyName = "Thomas"
    , applicantGivenName = "Gabriel"
    , applicantAdditionalName = Just "Raphaël"
    , applicantBday = Just $ addGregorianYearsClip (-21) today
    , applicantTag = Just "Professionnels"
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
    { applicantFamilyName = "Robert"
    , applicantGivenName = "Louise"
    , applicantAdditionalName = Just "Emma"
    , applicantBday = Just $ addGregorianYearsClip (-30) today
    , applicantTag = Just "Professionnels"
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
    { applicantFamilyName = "Richard"
    , applicantGivenName = "Arthur"
    , applicantAdditionalName = Just "Louis"
    , applicantBday = Just $ addGregorianYearsClip (-32) today
    , applicantTag = Just "Experts"
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
    { applicantFamilyName = "Durand"
    , applicantGivenName = "Jules"
    , applicantAdditionalName = Nothing
    , applicantBday = Just $ addGregorianYearsClip (-39) today
    , applicantTag = Just "Experts"
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
    { applicantFamilyName = "Dubois"
    , applicantGivenName = "Alice"
    , applicantAdditionalName = Just "Ambre"
    , applicantBday = Just $ addGregorianYearsClip (-35) today
    , applicantTag = Just "Experts"
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

  dept02 <- insert $ Dept "Gestion" Nothing
  _ <- insert $ Dept "Commercialisation" Nothing
  _ <- insert $ Dept "Ventes" Nothing
  _ <- insert $ Dept "Comptabilité" Nothing
  _ <- insert $ Dept "Finance" Nothing
  _ <- insert $ Dept "Production" Nothing
  _ <- insert $ Dept "Recherche et développement" Nothing
  _ <- insert $ Dept "Ressources humaines" Nothing


  skill14 <- insert $ Skill { skillCode = "La gestion des processus"
                            , skillName = "La gestion des processus"
                            , skillDescr = Just "Créer, lancer et développer un projet"
                            , skillLabel = Just "Gestion"
                            }
  skill15 <- insert $ Skill { skillCode = "Gestion des tâches"
                            , skillName = "Gestion des tâches"
                            , skillDescr = Just "Définissez des tâches et surveillez leur progression"
                            , skillLabel = Just "Gestion"
                            }
  skill16 <- insert $ Skill { skillCode = "Méthodologie de gestion"
                            , skillName = "Connaissance des méthodologies de gestion"
                            , skillDescr = Just "Connaissance des méthodologies de gestion"
                            , skillLabel = Just "Gestion"
                            }
  skill17 <- insert $ Skill { skillCode = "Gestion d'équipe"
                            , skillName = "Gestion d'équipe"
                            , skillDescr = Just "Fédérer l'équipe, répartir le travail, analyser les résultats"
                            , skillLabel = Just "Gestion"
                            }
  skill18 <- insert $ Skill { skillCode = "Expertise technique"
                            , skillName = "Expertise technique"
                            , skillDescr = Just "Comprendre le produit lui-même et tous les aspects de son développement"
                            , skillLabel = Just "Gestion"
                            }
  skill19 <- insert $ Skill { skillCode = "Gestion de documents"
                            , skillName = "Gestion de documents"
                            , skillDescr = Just "Rapports"
                            , skillLabel = Just "Gestion"
                            }
  skill20 <- insert $ Skill { skillCode = "Planification"
                            , skillName = "Planification"
                            , skillDescr = Just "Définir les budgets et les ressources nécessaires"
                            , skillLabel = Just "Gestion"
                            }
  skill21 <- insert $ Skill { skillCode = "Gestion des risques"
                            , skillName = "Gestion des risques"
                            , skillDescr = Just "Anticiper les risques et savoir les éviter"
                            , skillLabel = Just "Gestion"
                            }
  skill22 <- insert $ Skill { skillCode = "Gestion du budget et des coûts"
                            , skillName = "Gestion du budget et des coûts"
                            , skillDescr = Just "Pouvoir respecter les contraintes budgétaires sans nuire aux flux de travail"
                            , skillLabel = Just "Gestion"
                            }

  job03 <- insert $ Job { jobCode = "Chef de projet"
                        , jobName = "Chef de projet"
                        , jobDayStart = today
                        , jobDayEnd = addGregorianMonthsClip 4 today
                        , jobDescr = Just "Chef de projet informatique"
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
    { applicantFamilyName = "Moreau"
    , applicantGivenName = "Lina"
    , applicantAdditionalName = Nothing
    , applicantBday = Just $ addGregorianYearsClip (-42) today
    , applicantTag = Just "Executives"
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
                            , skillName = "Systèmes d'exploitation"
                            , skillDescr = Just "Compétences en administration du système d'exploitation"
                            , skillLabel = Just "DevOps"
                            }
  skill24 <- insert $ Skill { skillCode = "OS Linux"
                            , skillName = "Système d'exploitation Linux"
                            , skillDescr = Just "Compétences en administration Linux"
                            , skillLabel = Just "DevOps"
                            }
  skill25 <- insert $ Skill { skillCode = "OS Windows"
                            , skillName = "Système d'exploitation Windows Server"
                            , skillDescr = Just "Compétences en administration de Windows Server"
                            , skillLabel = Just "DevOps"
                            }
  skill26 <- insert $ Skill { skillCode = "PostgreSQL"
                            , skillName = "Administration de PostgreSQL"
                            , skillDescr = Just "Compétences en administration PostgreSQL"
                            , skillLabel = Just "Bases de données"
                            }
  skill27 <- insert $ Skill { skillCode = "Oracle DBA"
                            , skillName = "Administration de la base de données Oracle"
                            , skillDescr = Just "Compétences en administration de base de données Oracle"
                            , skillLabel = Just "Bases de données"
                            }
  skill28 <- insert $ Skill { skillCode = "MySQL"
                            , skillName = "Administration de la base de données MySQL"
                            , skillDescr = Just "Compétences en administration de bases de données MySQL"
                            , skillLabel = Just "Bases de données"
                            }
  skill29 <- insert $ Skill { skillCode = "Conteneurisation"
                            , skillName = "Conteneurisation"
                            , skillDescr = Just "Packaging et déploiement de logiciels"
                            , skillLabel = Just "DevOps"
                            }
  skill30 <- insert $ Skill { skillCode = "Docker"
                            , skillName = "Docker"
                            , skillDescr = Just "Compétences pour utiliser Docker pour empaqueter et déployer des applications"
                            , skillLabel = Just "DevOps"
                            }
  skill31 <- insert $ Skill { skillCode = "Docker Compose"
                            , skillName = "Docker Compose"
                            , skillDescr = Just "Définir et exécuter des applications Docker multi-conteneurs"
                            , skillLabel = Just "DevOps"
                            }
  skill32 <- insert $ Skill { skillCode = "Kubernetes"
                            , skillName = "Kubernetes"
                            , skillDescr = Just "Automatisation du déploiement, de la mise à l'échelle et de la gestion des logiciels"
                            , skillLabel = Just "DevOps"
                            }
  skill33 <- insert $ Skill { skillCode = "Compétences infonuagiques"
                            , skillName = "Compétences infonuagiques"
                            , skillDescr = Just "Compétences pour déployer une application dans le cloud"
                            , skillLabel = Just "DevOps"
                            }
  skill34 <- insert $ Skill { skillCode = "GCP PaaS"
                            , skillName = "Google Cloud Platform"
                            , skillDescr = Just "Compétences pour déployer une application sur Google Cloud"
                            , skillLabel = Just "Infonuagiques"
                            }
  skill35 <- insert $ Skill { skillCode = "AWS PaaS"
                            , skillName = "Amazon Web Services"
                            , skillDescr = Just "Compétences pour déployer une application sur Amazon Web Services"
                            , skillLabel = Just "Infonuagiques"
                            }
  skill36 <- insert $ Skill { skillCode = "Heroku PaaS"
                            , skillName = "Heroku Cloud Application Platform"
                            , skillDescr = Just "Compétences pour déployer une application sur Heroku"
                            , skillLabel = Just "Infonuagiques"
                            }
  skill37 <- insert $ Skill { skillCode = "Systèmes de contrôle de version"
                            , skillName = "Systèmes de contrôle de version"
                            , skillDescr = Just "Compétences pour gérer les modifications des programmes informatiques et des documents"
                            , skillLabel = Just "DevOps"
                            }
  skill38 <- insert $ Skill { skillCode = "Git"
                            , skillName = "Systèmes de contrôle de version Git"
                            , skillDescr = Just "Compétences pour gérer les changements avec Git"
                            , skillLabel = Just "DevOps"
                            }
  skill39 <- insert $ Skill { skillCode = "Mercurial"
                            , skillName = "Systèmes de contrôle de version Mercurial"
                            , skillDescr = Just "Compétences pour gérer les changements avec Mercurial"
                            , skillLabel = Just "DevOps"
                            }
  skill40 <- insert $ Skill { skillCode = "Apache Subversion"
                            , skillName = "Apache Subversion VCS"
                            , skillDescr = Just "Compétences pour gérer les changements avec Apache Subversion"
                            , skillLabel = Just "DevOps"
                            }

  job04 <- insert $ Job { jobCode = "Ingénieur DevOps"
                        , jobName = "Ingénieur DevOps"
                        , jobDayStart = addGregorianMonthsClip (-2) today
                        , jobDayEnd = addGregorianMonthsClip 2 today
                        , jobDescr = Just "Ingénieur intégration et automatisation pour le développement logiciel et les opérations informatiques"
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
    { applicantFamilyName = "Laurent"
    , applicantGivenName = "Adam"
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
    { applicantFamilyName = "Simon"
    , applicantGivenName = "Maël"
    , applicantAdditionalName = Just "Lucas"
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
    { applicantFamilyName = "Michel"
    , applicantGivenName = "Rose"
    , applicantAdditionalName = Just "Chloé"
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
