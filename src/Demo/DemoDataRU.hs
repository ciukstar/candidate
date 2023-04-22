{-# LANGUAGE OverloadedStrings #-}

module Demo.DemoDataRU (populateRU) where

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

populateRU :: MonadIO m => ReaderT SqlBackend m ()
populateRU = do
  today <- liftIO $ utctDay <$> getCurrentTime

  skill01 <- insert $ Skill { skillCode = "Веб-разработка"
                            , skillName = "Веб-разработка"
                            , skillDescr = Just "Навыки веб-разработки"
                            , skillLabel = Just "Программирование"
                            }
  skill02 <- insert $ Skill { skillCode = "HTML"
                            , skillName = "HTML"
                            , skillDescr = Just "Знание HTML-спецификации"
                            , skillLabel = Just "Программирование"
                            }
  skill03 <- insert $ Skill { skillCode = "Веб-дизайн"
                            , skillName = "Веб-дизайн"
                            , skillDescr = Just "Основные принципы веб-дизайна"
                            , skillLabel = Just "Программирование"
                            }
  skill04 <- insert $ Skill { skillCode = "JavaScript"
                            , skillName = "Язык программирования JavaScript"
                            , skillDescr = Just "Навыки программирования на JavaScript"
                            , skillLabel = Just "Программирование"
                            }
  skill05 <- insert $ Skill { skillCode = "REST"
                            , skillName = "Принципы REST-разработки"
                            , skillDescr = Just "Навыки разработки RESTful-приложений"
                            , skillLabel = Just "Архитектура"
                            }
  skill06 <- insert $ Skill { skillCode = "База данных"
                            , skillName = "База данных"
                            , skillDescr = Just "Навыки работы с базами данных"
                            , skillLabel = Just "Базы данных"
                            }
  skill07 <- insert $ Skill { skillCode = "SQL"
                            , skillName = "SQL"
                            , skillDescr = Just "Навыки написания SQL-запроса"
                            , skillLabel = Just "Базы данных"
                            }
  skill08 <- insert $ Skill { skillCode = "Администрирование базы данных"
                            , skillName = "Администрирование базы данных"
                            , skillDescr = Just "Навыки администрирования баз данных"
                            , skillLabel = Just "Базы данных"
                            }

  idIT <- insert $ Dept "Информационные технологии" Nothing
  jobIT01 <- insert $ Job { jobCode = "Разработчик JavaScript"
                          , jobName = "Разработчик JavaScript"
                          , jobDayStart = addGregorianMonthsClip (-2) today
                          , jobDayEnd = addGregorianMonthsClip 3 today
                          , jobDescr = Just "Программист на JavaScript"
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

  skill09 <- insert $ Skill { skillCode = "Бэкенд-разработка"
                            , skillName = "Бэкенд-разработка"
                            , skillDescr = Just "Разработка бизнес-логики продукта"
                            , skillLabel  = Just "Программирование"
                            }
  skill010 <- insert $ Skill { skillCode = "JavaSE"
                            , skillName = "Java Standard Edition"
                            , skillDescr = Just "Знание стандартной редакции Java 8+"
                            , skillLabel  = Just "Программирование"
                            }
  skill011 <- insert $ Skill { skillCode = "JakartaEE"
                            , skillName = "Jakarta Enterprise Edition"
                            , skillDescr = Just "Навыки разработки приложений Jakarta EE"
                            , skillLabel  = Just "Программирование"
                            }
  skill012 <- insert $ Skill { skillCode = "Фронтенд-разработка"
                             , skillName = "Фронтенд-разработка"
                             , skillDescr = Just "Разработка пользовательского веб-интерфейса"
                             , skillLabel  = Just "Веб-технологии"
                             }
  skill013 <- insert $ Skill { skillCode = "CSS"
                             , skillName = "CSS"
                             , skillDescr = Just "Каскадные таблицы стилей"
                             , skillLabel  = Just "Веб-технологии"
                             }

  job02 <- insert $ Job { jobCode = "Разработчик JakartaEE"
                        , jobName = "Разработчик Jakarta Enterprise Edition"
                        , jobDayStart = addGregorianMonthsClip (-3) today
                        , jobDayEnd = addGregorianMonthsClip 2 today
                        , jobDescr = Just "Программист на Java Enterprise Edition"
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
    { applicantFamilyName = "Иванов"
    , applicantGivenName = "Игорь"
    , applicantAdditionalName = Just "Васильевич"
    , applicantBday = Just $ addGregorianYearsClip (-28) today
    , applicantTag = Just "Новые сотрудники"
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
    { applicantFamilyName = "Буланова"
    , applicantGivenName = "Любовь"
    , applicantAdditionalName = Just "Михайловна"
    , applicantBday = Just $ addGregorianYearsClip (-26) today
    , applicantTag = Just "Новые сотрудники"
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
    { applicantFamilyName = "Петров"
    , applicantGivenName = "Иван"
    , applicantAdditionalName = Just "Александрович"
    , applicantBday = Just $ addGregorianYearsClip (-21) today
    , applicantTag = Just "Специалисты"
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
    { applicantFamilyName = "Лебедева"
    , applicantGivenName = "Марина"
    , applicantAdditionalName = Just "Викторовна"
    , applicantBday = Just $ addGregorianYearsClip (-30) today
    , applicantTag = Just "Специалисты"
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
    { applicantFamilyName = "Смирнов"
    , applicantGivenName = "Андрей"
    , applicantAdditionalName = Just "Васильевич"
    , applicantBday = Just $ addGregorianYearsClip (-32) today
    , applicantTag = Just "Эксперты"
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
    { applicantFamilyName = "Иванов"
    , applicantGivenName = "Алексей"
    , applicantAdditionalName = Just "Васильевич"
    , applicantBday = Just $ addGregorianYearsClip (-39) today
    , applicantTag = Just "Эксперты"
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
    { applicantFamilyName = "Сергеева"
    , applicantGivenName = "Александра"
    , applicantAdditionalName = Just "Владимировна"
    , applicantBday = Just $ addGregorianYearsClip (-35) today
    , applicantTag = Just "Эксперты"
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

  dept02 <- insert $ Dept "Департамент управления" Nothing
  _ <- insert $ Dept "Отдел маркетинга" Nothing
  _ <- insert $ Dept "Отдел продаж" Nothing
  _ <- insert $ Dept "Бухгалтерия" Nothing
  _ <- insert $ Dept "Финансовый отдел" Nothing
  _ <- insert $ Dept "Отдел производства" Nothing
  _ <- insert $ Dept "Исследование и разработка" Nothing
  _ <- insert $ Dept "Отдел кадров" Nothing


  skill14 <- insert $ Skill { skillCode = "Управление процессами"
                            , skillName = "Управление процессами"
                            , skillDescr = Just "Создание, запуск и развитие проекта"
                            , skillLabel = Just "Управление"
                            }
  skill15 <- insert $ Skill { skillCode = "Управление задачами"
                            , skillName = "Управление задачами"
                            , skillDescr = Just "Ставить задачи и следить за их выполнением"
                            , skillLabel = Just "Управление"
                            }
  skill16 <- insert $ Skill { skillCode = "Методология управления"
                            , skillName = "Знание методологий управления"
                            , skillDescr = Just "Знание методологий управления"
                            , skillLabel = Just "Управление"
                            }
  skill17 <- insert $ Skill { skillCode = "Управление командой"
                            , skillName = "Управление командой"
                            , skillDescr = Just "Объединять команду, распределять работы, анализировать результаты"
                            , skillLabel = Just "Управление"
                            }
  skill18 <- insert $ Skill { skillCode = "Техническая экспертность"
                            , skillName = "Техническая экспертность"
                            , skillDescr = Just "Разбираться в самом продукте и всех аспектах его разработки"
                            , skillLabel = Just "Управление"
                            }
  skill19 <- insert $ Skill { skillCode = "Введение документооборота"
                            , skillName = "Введение документооборота"
                            , skillDescr = Just "Отчётность"
                            , skillLabel = Just "Управление"
                            }
  skill20 <- insert $ Skill { skillCode = "Планирование"
                            , skillName = "Планирование"
                            , skillDescr = Just "Определение бюджетов и необходимых ресурсов"
                            , skillLabel = Just "Управление"
                            }
  skill21 <- insert $ Skill { skillCode = "Риск-менеджмент"
                            , skillName = "Риск-менеджмент"
                            , skillDescr = Just "Предвидеть риски, уметь их избегать"
                            , skillLabel = Just "Управление"
                            }
  skill22 <- insert $ Skill { skillCode = "Управление бюджетами и затратами"
                            , skillName = "Управление бюджетами и затратами"
                            , skillDescr = Just "Уметь укладываться в бюджетные ограничения без вреда рабочим процессам"
                            , skillLabel = Just "Управление"
                            }

  job03 <- insert $ Job { jobCode = "Руководитель проекта"
                        , jobName = "Руководитель проекта"
                        , jobDayStart = today
                        , jobDayEnd = addGregorianMonthsClip 4 today
                        , jobDescr = Just "Менеджер проектов в области ИТ"
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
    { applicantFamilyName = "Степанова"
    , applicantGivenName = "Татьяна"
    , applicantAdditionalName = Just "Николаевна"
    , applicantBday = Just $ addGregorianYearsClip (-42) today
    , applicantTag = Just "Руководители"
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


  skill23 <- insert $ Skill { skillCode = "ОС"
                            , skillName = "Операционные системы"
                            , skillDescr = Just "Навыки администрирования операционной системы"
                            , skillLabel = Just "DevOps"
                            }
  skill24 <- insert $ Skill { skillCode = "ОС Линукс"
                            , skillName = "Операционная система Linux"
                            , skillDescr = Just "Навыки администрирования Линукс"
                            , skillLabel = Just "DevOps"
                            }
  skill25 <- insert $ Skill { skillCode = "ОС Windows"
                            , skillName = "Операционная система Windows Server"
                            , skillDescr = Just "Навыки администрирования Windows Server"
                            , skillLabel = Just "DevOps"
                            }
  skill26 <- insert $ Skill { skillCode = "PostgreSQL"
                            , skillName = "Администрирование PostgreSQL"
                            , skillDescr = Just "Навыки администрирования PostgreSQL."
                            , skillLabel = Just "Базы данных"
                            }
  skill27 <- insert $ Skill { skillCode = "Oracle DBA"
                            , skillName = "Администрирование базы данных Oracle"
                            , skillDescr = Just "Навыки администрирования баз данных Oracle"
                            , skillLabel = Just "Базы данных"
                            }
  skill28 <- insert $ Skill { skillCode = "Администрирование MySQL"
                            , skillName = "Администрирование базы данных MySQL"
                            , skillDescr = Just "Навыки администрирования базы данных MySQL"
                            , skillLabel = Just "Базы данных"
                            }
  skill29 <- insert $ Skill { skillCode = "Контейнеризация"
                            , skillName = "Контейнеризация"
                            , skillDescr = Just "Упаковка и развертывание программного обеспечения"
                            , skillLabel = Just "DevOps"
                            }
  skill30 <- insert $ Skill { skillCode = "Докер"
                            , skillName = "Докер"
                            , skillDescr = Just "Навыки использования Docker для упаковки и развертывания приложений"
                            , skillLabel = Just "DevOps"
                            }
  skill31 <- insert $ Skill { skillCode = "Docker Compose"
                            , skillName = "Docker Compose"
                            , skillDescr = Just "Определение и запуск многоконтейнерных приложений Docker"
                            , skillLabel = Just "DevOps"
                            }
  skill32 <- insert $ Skill { skillCode = "Kubernetes"
                            , skillName = "Kubernetes"
                            , skillDescr = Just "Автоматизация развертывания, масштабирования и управления программным обеспечением"
                            , skillLabel = Just "DevOps"
                            }
  skill33 <- insert $ Skill { skillCode = "Облачные навыки"
                            , skillName = "Облачные навыки"
                            , skillDescr = Just "Навыки развертывания приложений в облаке"
                            , skillLabel = Just "DevOps"
                            }
  skill34 <- insert $ Skill { skillCode = "GCP PaaS"
                            , skillName = "Облачная платформа Google"
                            , skillDescr = Just "Навыки развертывания приложений в Google Cloud"
                            , skillLabel = Just "Облачные технологии"
                            }
  skill35 <- insert $ Skill { skillCode = "AWS PaaS"
                            , skillName = "Веб-сервисы Amazon"
                            , skillDescr = Just "Навыки развертывания приложений на Amazon Web Services"
                            , skillLabel = Just "Облачные технологии"
                            }
  skill36 <- insert $ Skill { skillCode = "Heroku PaaS"
                            , skillName = "Платформа облачных приложений Heroku"
                            , skillDescr = Just "Навыки развертывания приложений на Heroku"
                            , skillLabel = Just "Облачные технологии"
                            }
  skill37 <- insert $ Skill { skillCode = "Системы контроля версий"
                            , skillName = "Системы контроля версий"
                            , skillDescr = Just "Навыки управления изменениями в компьютерных программах и документах"
                            , skillLabel = Just "DevOps"
                            }
  skill38 <- insert $ Skill { skillCode = "Git"
                            , skillName = "Системы контроля версий Git"
                            , skillDescr = Just "Навыки для управления изменениями с помощью Git"
                            , skillLabel = Just "DevOps"
                            }
  skill39 <- insert $ Skill { skillCode = "Mercurial"
                            , skillName = "Системы управления версиями Mercurial"
                            , skillDescr = Just "Навыки для управления изменениями с помощью Mercurial"
                            , skillLabel = Just "DevOps"
                            }
  skill40 <- insert $ Skill { skillCode = "Apache Subversion"
                            , skillName = "Apache Subversion СКВ"
                            , skillDescr = Just "Навыки управления изменениями с помощью Apache Subversion"
                            , skillLabel = Just "DevOps"
                            }

  job04 <- insert $ Job { jobCode = "Инженер по DevOps"
                        , jobName = "Инженер по DevOps"
                        , jobDayStart = addGregorianMonthsClip (-2) today
                        , jobDayEnd = addGregorianMonthsClip 2 today
                        , jobDescr = Just "addGregorianMonthsClip 4 today"
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
    { applicantFamilyName = "Кузнецов"
    , applicantGivenName = "Артем"
    , applicantAdditionalName = Just "Сергеевич"
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
    { applicantFamilyName = "Попов"
    , applicantGivenName = "Дмитрий"
    , applicantAdditionalName = Just "Александрович"
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
    { applicantFamilyName = "Баранова"
    , applicantGivenName = "Алиса"
    , applicantAdditionalName = Just "Григорьевна"
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
