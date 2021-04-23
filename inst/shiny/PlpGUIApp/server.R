library(shiny)
library(plotly)
library(shinycssloaders)

server <- shiny::shinyServer(function(input, output, session) {
  session$onSessionEnded(shiny::stopApp)


  # ===== WEBAPI

  webApi <- shiny::reactiveVal('')
  cohortReactive <- shiny::reactiveVal(data.frame())
  baseUrlCheck <- shiny::reactiveVal('WebAPI: ')
  output$baseUrlCheck <- shiny::renderText(baseUrlCheck())

  shiny::observeEvent(input$connectwebApi, {

    if(!is.null(input$baseUrl)){

      url <- paste0(input$baseUrl, "/info")
      response <- tryCatch({httr::GET(url)},
                           error = function(e){return(NULL)})
      if(!is.null(response)){
        baseUrlCheck('WebAPI: (Connected)')
        showNotification(paste("WebAPI connection works..."), duration = 0, type = 'message')
        webApi(input$baseUrl)
        cohorts <- ROhdsiWebApi::getCohortDefinitionsMetaData(webApi())[,c('id', 'name')]
        cohortReactive(as.data.frame(cohorts))
      } else{
        baseUrlCheck('WebAPI:')
        showNotification(paste("WebAPI input did not connect"), duration = 0, type = 'error')
        webApi('')
        cohortReactive(data.frame())
      }
    }
  })

  # ===== END WEBAPI

  # ===== INSTALLER
  callModule(installServer, 'installPatientLevelPrediction',
             package = c('PatientLevelPrediction','Hydra'))

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
  ## add module to restrict O/P, T/M, T/C pairs
  OPList <- callModule(restrictionServer, 'restrictionPODev',
                       options1Names = outcomeList()$Name,
                       options1Vals = outcomeList()$Id,
                       options2Names = names(popList()),
                       options2Vals = 1:length(popList()),
                       colnameValues = c('outcomeId', 'populationSettingId'))

  MCList <- callModule(restrictionServer, 'restrictionMCDev',
                       options1Names = paste(1:length(modelList()),
                                             unlist(lapply(modelList(), function(x) x$name)),
                                             sep = '-'),
                       options1Vals = 1:length(modelList()),
                       options2Names = 1:length(covList()),#names(covList()),
                       options2Vals = 1:length(covList()),
                       colnameValues = c('modelSettingId', 'covariateSettingId'))

  TCList <- callModule(restrictionServer, 'restrictionTCDev',
                       options1Names = targetList()$Name,
                       options1Vals = targetList()$Id,
                       options2Names = 1:length(covList()),#names(covList()),
                       options2Vals = 1:length(covList()),
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

  # ===== EXECUTE
  callModule(executeServer, 'executePrediction')

  # ===== END EXECUTE


})
