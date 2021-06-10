library(shiny)
library(plotly)
library(shinycssloaders)

server <- shiny::shinyServer(function(input, output, session) {
  session$onSessionEnded(shiny::stopApp)


  # ===== WEBAPI

  webApi <- callModule(webApiServer, 'webApiMain')
  cohortReactive <- callModule(extractCohortsServer, 'cohortExtract',
                               webApi = webApi)

  # ===== END WEBAPI

  # ===== INSTALLER
  callModule(installServer, 'installPatientLevelPrediction',
             package = 'PatientLevelPrediction')
  callModule(installServer, 'installHydra',
             package = 'Hydra')
  callModule(installServer, 'installSkeletonPredictionStudy',
             package = 'SkeletonPredictionStudy')
  callModule(installServer, 'installSkeletonPredictionValidationStudy',
             package = 'SkeletonPredictionValidationStudy')

  # ===== END INSTALLER


  # ===== MODEL DEVELOPMENT

  analysisList <- callModule(analysisServer, 'analysisDev')

  # Cohorts
  targetList <- callModule(cohortServer, 'targetDev', cohortReactive)
  outcomeList <- callModule(cohortServer, 'outcomeDev', cohortReactive)

  # Models
  modelList <- callModule(modelServer, 'modelDev')

  # Covariates
  covList <- callModule(covariateServer, 'covariateDev', cohortReactive)

  # Population
  popList <- callModule(populationServer, 'populationDev')

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
             analysisList= analysisList)

  # =====





  # ===== VALIDATION

  analysisValList <- callModule(analysisServer, 'analysisVal')

  # Cohorts
  targetValList <- callModule(cohortServer, 'targetVal', cohortReactive)
  outcomeValList <- callModule(cohortServer, 'outcomeVal', cohortReactive)

  # Population
  popValList <- callModule(populationServer, 'populationVal')

  # Models
  ##modelValList <- callModule(modelValidationServer, 'modelVal')

  jsonForValStudy <- callModule(jsonServer, 'jsonVal',
                             analysisList= analysisValList,
                             targetList = targetValList,
                             outcomeList = outcomeValList,
                             modelList = modelValList,
                             popList = popValList,
                             webApi = webApi)

  callModule(downloadServer, 'downloadVal',
             jsonForStudy = jsonForValStudy,
             analysisList= analysisValList)

  # =====






  # ===== EXECUTE
  callModule(executeServer, 'executePrediction')

  # ===== END EXECUTE


})
