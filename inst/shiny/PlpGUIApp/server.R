library(shiny)
library(plotly)
library(shinycssloaders)

server <- shiny::shinyServer(function(input, output, session) {
  session$onSessionEnded(shiny::stopApp)


  # ===== WEBAPI

  webApi <- callModule(OhdsiShinyModules::webApiServer, 'webApiMain')
  cohortReactive <- callModule(OhdsiShinyModules::extractCohortsServer, 'cohortExtract',
                               webApi = webApi)

  # ===== END WEBAPI

  # ===== INSTALLER
  #callModule(OhdsiShinyModules::installServer, 'installPatientLevelPrediction',
  #           package = 'PatientLevelPrediction')
  #callModule(OhdsiShinyModules::installServer, 'installHydra',
  #           package = 'Hydra')
  #callModule(OhdsiShinyModules::installServer, 'installSkeletonPredictionStudy',
  #           package = 'SkeletonPredictionStudy')
  #callModule(OhdsiShinyModules::installServer, 'installSkeletonPredictionValidationStudy',
  #           package = 'SkeletonPredictionValidationStudy')

  # ===== END INSTALLER


  # ===== MODEL DEVELOPMENT

  analysisList <- callModule(analysisServer, 'analysisDev')

  # Cohorts
  targetList <- callModule(OhdsiShinyModules::cohortServer, 'targetDev', cohortReactive)
  outcomeList <- callModule(OhdsiShinyModules::cohortServer, 'outcomeDev', cohortReactive)

  # Models
  modelList <- callModule(OhdsiShinyModules::modelServer, 'modelDev')

  # Covariates
  covList <- callModule(OhdsiShinyModules::covariateServer, 'covariateDev',
                        cohortReactive, validation = F)

  # Population
  popList <- callModule(OhdsiShinyModules::populationServer, 'populationDev')

  # Restrict Analysis
  options1NamesOP <- shiny::reactiveVal(NULL)
  options1ValsOP <- shiny::reactiveVal(NULL)
    shiny::observeEvent(outcomeList(), {
                   options1NamesOP(outcomeList()$Name)
                   options1ValsOP(outcomeList()$Id)
                   })

  ## add module to restrict O/P, T/M, T/C pairs
  OPList <- callModule(restrictionServer, 'restrictionPODev',
                       options1Names = options1NamesOP,
                       options1Vals = options1ValsOP,
                       options2Names = shiny::reactive(names(popList())),
                       options2Vals = shiny::reactive(1:length(popList())),
                       colnameValues = c('outcomeId', 'populationSettingId'))

  options1NamesMC <- shiny::reactiveVal(NULL)
    shiny::observeEvent(modelList(), {
      options1NamesMC(paste(1:length(modelList()),
                            unlist(lapply(modelList(), function(x) x$name)),
                            sep = '-'))
    })

  MCList <- callModule(restrictionServer, 'restrictionMCDev',
                       options1Names = options1NamesMC,
                       options1Vals = shiny::reactive(1:length(options1NamesMC())),
                       options2Names = shiny::reactive(names(covList())),
                       options2Vals = shiny::reactive(1:length(covList())),
                       colnameValues = c('modelSettingId', 'covariateSettingId'))

  options1NamesTC <- shiny::reactiveVal(NULL)
  options1ValsTC <- shiny::reactiveVal(NULL)
    shiny::observeEvent(targetList(), {
      options1NamesTC(targetList()$Name)
      options1ValsTC(targetList()$Id)
    })

  TCList <- callModule(restrictionServer, 'restrictionTCDev',
                       options1Names = options1NamesTC,
                       options1Vals = options1ValsTC,
                       options2Names = shiny::reactive(names(covList())),
                       options2Vals = shiny::reactive(1:length(covList())),
                       colnameValues = c('targetId', 'covariateSettingId'))

  # Execute settings
  executeList <- callModule(trainServer, 'trainDev')

  jsonForStudy <- callModule(jsonServer, 'jsonDev',
                             type = 'development',
                             analysisList= analysisList,
                             targetList = targetList,
                             outcomeList = outcomeList,
                             modelList = modelList,
                             covList = covList,
                             popList = popList,
                             OPList = OPList,
                             MCList = MCList,
                             TCList = TCList,
                             executeList = executeList,
                             webApi = webApi)

  callModule(downloadServer, 'downloadDev',
             jsonForStudy = jsonForStudy,
             analysisList= analysisList,
             baseUrl = webApi)

  # =====





  # ===== VALIDATION

  analysisValList <- callModule(analysisServer, 'analysisVal')

  # Models
  modelValList <- callModule(existingModelServer, 'existingModel', cohortReactive, webApi)

  jsonForValStudy <- callModule(jsonServer, 'jsonVal',
                                type = 'validation',
                             analysisList= analysisValList,
                             modelList = modelValList,
                             webApi = webApi)

  callModule(downloadServer, 'downloadVal',
             jsonForStudy = jsonForValStudy,
             analysisList = analysisValList,
             baseUrl = webApi)

  # =====






  # ===== EXECUTE
  callModule(executeServer, 'executePrediction')

  # ===== END EXECUTE


})
