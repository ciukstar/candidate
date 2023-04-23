{-# LANGUAGE OverloadedStrings #-}

module Demo.DemoDataEN (populateEN) where

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

populateEN :: MonadIO m => ReaderT SqlBackend m ()
populateEN = do
  today <- liftIO $ utctDay <$> getCurrentTime

  skill01 <- insert $ Skill { skillCode = "Web development"
                            , skillName = "Web development"
                            , skillDescr = Just "Web development skills"
                            , skillLabel = Just "Development"
                            }
  skill02 <- insert $ Skill { skillCode = "HTML"
                            , skillName = "HTML"
                            , skillDescr = Just "Knowledge of the HTML spec"
                            , skillLabel = Just "Development"
                            }
  skill03 <- insert $ Skill { skillCode = "Web design"
                            , skillName = "Web design"
                            , skillDescr = Just "Basic web design principles"
                            , skillLabel = Just "Development"
                            }
  skill04 <- insert $ Skill { skillCode = "JavaScript"
                            , skillName = "JavaScript programming language"
                            , skillDescr = Just "JavaScript programming skills"
                            , skillLabel = Just "Development"
                            }
  skill05 <- insert $ Skill { skillCode = "REST"
                            , skillName = "REST development principles"
                            , skillDescr = Just "RESTful application development skills"
                            , skillLabel = Just "Architecture"
                            }
  skill06 <- insert $ Skill { skillCode = "Databases"
                            , skillName = "Databases"
                            , skillDescr = Just "Database skills"
                            , skillLabel = Just "Databases"
                            }
  skill07 <- insert $ Skill { skillCode = "SQL"
                            , skillName = "SQL"
                            , skillDescr = Just "SQL query writing skills"
                            , skillLabel = Just "Databases"
                            }
  skill08 <- insert $ Skill { skillCode = "Database administration"
                            , skillName = "Database administration"
                            , skillDescr = Just "Database administration skills"
                            , skillLabel = Just "Databases"
                            }

  idIT <- insert $ Dept "Information Technology" Nothing
  jobIT01 <- insert $ Job { jobCode = "JavaScript Developer"
                          , jobName = "JavaScript Developer"
                          , jobDayStart = addGregorianMonthsClip (-2) today
                          , jobDayEnd = addGregorianMonthsClip 3 today
                          , jobDescr = Just "JavaScript programmer"
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

  skill09 <- insert $ Skill { skillCode = "Backend development"
                            , skillName = "Backend development"
                            , skillDescr = Just "Business logic development"
                            , skillLabel  = Just "Development"
                            }
  skill010 <- insert $ Skill { skillCode = "JavaSE"
                            , skillName = "Java Standard Edition"
                            , skillDescr = Just "Knowledge of Java 8+ Standard Edition"
                            , skillLabel  = Just "Development"
                            }
  skill011 <- insert $ Skill { skillCode = "JakartaEE"
                            , skillName = "Jakarta Enterprise Edition"
                            , skillDescr = Just "Jakarta EE Application Development Skills"
                            , skillLabel  = Just "Development"
                            }
  skill012 <- insert $ Skill { skillCode = "Frontend development"
                             , skillName = "Frontend development"
                             , skillDescr = Just "Web user interface development"
                             , skillLabel  = Just "Web technologies"
                             }
  skill013 <- insert $ Skill { skillCode = "CSS"
                             , skillName = "CSS"
                             , skillDescr = Just "Cascading Style Sheets"
                             , skillLabel  = Just "Web technologies"
                             }

  job02 <- insert $ Job { jobCode = "Developer JakartaEE"
                        , jobName = "Developer Jakarta Enterprise Edition"
                        , jobDayStart = addGregorianMonthsClip (-3) today
                        , jobDayEnd = addGregorianMonthsClip 2 today
                        , jobDescr = Just "Java Enterprise Edition programmer"
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
    { applicantFamilyName = "Smith"
    , applicantGivenName = "Johnny"
    , applicantAdditionalName = Nothing
    , applicantBday = Just $ addGregorianYearsClip (-28) today
    , applicantTag = Just "New employees"
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
    { applicantFamilyName = "Lopez"
    , applicantGivenName = "Mary"
    , applicantAdditionalName = Nothing
    , applicantBday = Just $ addGregorianYearsClip (-26) today
    , applicantTag = Just "New employees"
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
    { applicantFamilyName = "Johnson"
    , applicantGivenName = "John"
    , applicantAdditionalName = Just "Thomas"
    , applicantBday = Just $ addGregorianYearsClip (-21) today
    , applicantTag = Just "Professionals"
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
    { applicantFamilyName = "Brown"
    , applicantGivenName = "Patricia"
    , applicantAdditionalName = Just "Elizabeth"
    , applicantBday = Just $ addGregorianYearsClip (-30) today
    , applicantTag = Just "Professionals"
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
    { applicantFamilyName = "Wilson"
    , applicantGivenName = "Chris"
    , applicantAdditionalName = Just "Lee"
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
    { applicantFamilyName = "Davis"
    , applicantGivenName = "Philip"
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
    { applicantFamilyName = "Taylor"
    , applicantGivenName = "Helen"
    , applicantAdditionalName = Just "Renee"
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

  dept02 <- insert $ Dept "Management" Nothing
  _ <- insert $ Dept "Marketing" Nothing
  _ <- insert $ Dept "Sales" Nothing
  _ <- insert $ Dept "Accounting" Nothing
  _ <- insert $ Dept "Finance" Nothing
  _ <- insert $ Dept "Production" Nothing
  _ <- insert $ Dept "Research and development" Nothing
  _ <- insert $ Dept "Human Resources" Nothing


  skill14 <- insert $ Skill { skillCode = "Process management"
                            , skillName = "Process management"
                            , skillDescr = Just "Creating, launching and developing a project"
                            , skillLabel = Just "Management"
                            }
  skill15 <- insert $ Skill { skillCode = "Task Management"
                            , skillName = "Task Management"
                            , skillDescr = Just "Set tasks and monitor their progress"
                            , skillLabel = Just "Management"
                            }
  skill16 <- insert $ Skill { skillCode = "Management methodology"
                            , skillName = "Knowledge of management methodologies"
                            , skillDescr = Just "Knowledge of management methodologies"
                            , skillLabel = Just "Management"
                            }
  skill17 <- insert $ Skill { skillCode = "Team management"
                            , skillName = "Team management"
                            , skillDescr = Just "Unite the team, distribute work, analyze the results"
                            , skillLabel = Just "Management"
                            }
  skill18 <- insert $ Skill { skillCode = "Technical Expertise"
                            , skillName = "Technical Expertise"
                            , skillDescr = Just "Understand the product itself and all aspects of its development"
                            , skillLabel = Just "Management"
                            }
  skill19 <- insert $ Skill { skillCode = "Document management"
                            , skillName = "Document management"
                            , skillDescr = Just "Reporting"
                            , skillLabel = Just "Management"
                            }
  skill20 <- insert $ Skill { skillCode = "Planning"
                            , skillName = "Planning"
                            , skillDescr = Just "Defining budgets and required resources"
                            , skillLabel = Just "Management"
                            }
  skill21 <- insert $ Skill { skillCode = "Risk management"
                            , skillName = "Risk management"
                            , skillDescr = Just "Anticipate risks and be able to avoid them"
                            , skillLabel = Just "Management"
                            }
  skill22 <- insert $ Skill { skillCode = "Budget and cost management"
                            , skillName = "Budget and cost management"
                            , skillDescr = Just "Be able to fit within budget constraints without harming workflows"
                            , skillLabel = Just "Management"
                            }

  job03 <- insert $ Job { jobCode = "Project Manager"
                        , jobName = "Project Manager"
                        , jobDayStart = today
                        , jobDayEnd = addGregorianMonthsClip 4 today
                        , jobDescr = Just "IT project manager"
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
    { applicantFamilyName = "Young"
    , applicantGivenName = "Barbara"
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
                            , skillName = "Operating Systems"
                            , skillDescr = Just "Operating System administration skills"
                            , skillLabel = Just "DevOps"
                            }
  skill24 <- insert $ Skill { skillCode = "OS Linux"
                            , skillName = "Linux Operating System"
                            , skillDescr = Just "Linux administration skills"
                            , skillLabel = Just "DevOps"
                            }
  skill25 <- insert $ Skill { skillCode = "OS Windows"
                            , skillName = "Windows Server Operating System"
                            , skillDescr = Just "Windows Server administration skills"
                            , skillLabel = Just "DevOps"
                            }
  skill26 <- insert $ Skill { skillCode = "PostgreSQL"
                            , skillName = "PostgreSQL administration"
                            , skillDescr = Just "PostgreSQL administration skills"
                            , skillLabel = Just "Databases"
                            }
  skill27 <- insert $ Skill { skillCode = "Oracle DBA"
                            , skillName = "Oracle database administration"
                            , skillDescr = Just "Oracle database administration skills"
                            , skillLabel = Just "Databases"
                            }
  skill28 <- insert $ Skill { skillCode = "MySQL administration"
                            , skillName = "MySQL database administration"
                            , skillDescr = Just "MySQL database administration skills"
                            , skillLabel = Just "Databases"
                            }
  skill29 <- insert $ Skill { skillCode = "Containerization"
                            , skillName = "Containerization"
                            , skillDescr = Just "Packaging and deployment of software"
                            , skillLabel = Just "DevOps"
                            }
  skill30 <- insert $ Skill { skillCode = "Docker"
                            , skillName = "Docker"
                            , skillDescr = Just "Skills for using Docker to package and deploy applications"
                            , skillLabel = Just "DevOps"
                            }
  skill31 <- insert $ Skill { skillCode = "Docker Compose"
                            , skillName = "Docker Compose"
                            , skillDescr = Just "Defining and running multi-container Docker applications"
                            , skillLabel = Just "DevOps"
                            }
  skill32 <- insert $ Skill { skillCode = "Kubernetes"
                            , skillName = "Kubernetes"
                            , skillDescr = Just "Automating software deployment, scaling, and management"
                            , skillLabel = Just "DevOps"
                            }
  skill33 <- insert $ Skill { skillCode = "Cloud Skills"
                            , skillName = "Cloud Skills"
                            , skillDescr = Just "Skills for deploying an applications in the cloud"
                            , skillLabel = Just "DevOps"
                            }
  skill34 <- insert $ Skill { skillCode = "GCP PaaS"
                            , skillName = "Google Cloud Platform"
                            , skillDescr = Just "Skills for deploying an applications on Google Cloud"
                            , skillLabel = Just "Cloud"
                            }
  skill35 <- insert $ Skill { skillCode = "AWS PaaS"
                            , skillName = "Amazon Web Services"
                            , skillDescr = Just "Skills for deploying an applications on Amazon Web Services"
                            , skillLabel = Just "Cloud"
                            }
  skill36 <- insert $ Skill { skillCode = "Heroku PaaS"
                            , skillName = "Heroku Cloud Application Platform"
                            , skillDescr = Just "Skills for deploying an applications on Heroku"
                            , skillLabel = Just "Cloud"
                            }
  skill37 <- insert $ Skill { skillCode = "Version Control Systems"
                            , skillName = "Version Control Systems"
                            , skillDescr = Just "Skills for for managing changes to computer programs and documents"
                            , skillLabel = Just "DevOps"
                            }
  skill38 <- insert $ Skill { skillCode = "Git"
                            , skillName = "Git Version Control Systems"
                            , skillDescr = Just "Skills for for managing changes with Git"
                            , skillLabel = Just "DevOps"
                            }
  skill39 <- insert $ Skill { skillCode = "Mercurial"
                            , skillName = "Mercurial Version Control Systems"
                            , skillDescr = Just "Skills for for managing changes with Mercurial"
                            , skillLabel = Just "DevOps"
                            }
  skill40 <- insert $ Skill { skillCode = "Apache Subversion"
                            , skillName = "Apache Subversion VCS"
                            , skillDescr = Just "Skills for for managing changes with Apache Subversion"
                            , skillLabel = Just "DevOps"
                            }

  job04 <- insert $ Job { jobCode = "DevOps Engineer"
                        , jobName = "DevOps Engineer"
                        , jobDayStart = addGregorianMonthsClip (-2) today
                        , jobDayEnd = addGregorianMonthsClip 2 today
                        , jobDescr = Just "Integration and automation engineer for software development and IT operations"
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
    { applicantFamilyName = "Walker"
    , applicantGivenName = "Jorge"
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
    { applicantFamilyName = "Evans"
    , applicantGivenName = "Robert"
    , applicantAdditionalName = Just "William"
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
    { applicantFamilyName = "Hughes"
    , applicantGivenName = "Isabel"
    , applicantAdditionalName = Just "Mae"
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
