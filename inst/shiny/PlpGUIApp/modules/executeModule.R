executeViewer <- function(id, label = "Development") {
  ns <- shiny::NS(id)
 shiny::fluidRow(
   shinydashboard::box('Install',status = 'info',
                       shiny::actionButton(ns('installStudyViewer'), label = 'Click To Install A Study'),
                       shiny::h5("Click the button above to be able to select a study you created using the 'Design' editor or a prediction study found at 'https://github.com/ohdsi-studies'")
   ),

   shinydashboard::box('Execute', status = 'success',
                       shiny::actionButton(ns('executeStudyViewer'), label = 'Click To Execute A Study'),
                       shiny::h5("Click the button above to pick a study you have installed and execute it (develop a model and/or validate a model)")
   )

 )
}

executeServer <- function(input, output, session) {

  studyPackage <- shiny::reactiveVal(NULL)

  output$selectOhdsiStudy <- shiny::renderUI(
    shiny::selectInput(inputId = session$ns('githubPackage'),
                       label = 'Pick ohdsi study:',
                       choices = getOhdsiStudies())
  )

  output$selectLibrary <- shiny::renderUI(
    shiny::selectInput(inputId = session$ns('library'),
                       label = 'Pick library:',
                       choices = getlibraries(),
                       selected = studyPackage())
  )

  shiny::observeEvent(input$installStudyViewer, {
    shiny::showModal(installStudyModal(session$ns))
  })
  shiny::observeEvent(input$executeStudyViewer, {
    shiny::showModal(executeStudyModal(session$ns))
  })

  shiny::observeEvent(input$installStudyGithub, {
    shiny::removeModal()
      # install from github
    tryCatch({
      install.packages(setdiff('devtools', rownames(installed.packages())))
      devtools::install_github(input$githubPackage, upgrade = "never")},
      error = function(e){shiny::showNotification(paste0('Installing study error: ', e), duration = 5, type = 'error')})
  })

  shiny::observeEvent(input$installStudyLoc, {
    shiny::removeModal()
      # build package
      if(dir.exists(input$packageLoc)){
        shiny::showNotification('Building study', duration = 5, type = 'info')
        pkgbuild::build(input$packageLoc)
      } else{
        shiny::showNotification('No package at location input', duration = 5, type = 'error')
      }
  })


  shiny::observeEvent(input$executeStudy, {
    shiny::removeModal()
    studyPackage(input$library)

    if(studyPackage() != ''){
    runStudy(studyPackage = studyPackage(),
                         outputFolder = input$outputFolder,
                         dbms = input$dbms,
                         user = input$user,
                         pw = input$pw,
                         server = input$server,
                         port = input$port,
                         cdmDatabaseSchema = input$cdmDatabaseSchema,
                         cdmDatabaseName = input$cdmDatabaseName ,
                         cohortDatabaseSchema = input$cohortDatabaseSchema,
                         oracleTempSchema = input$oracleTempSchema,
                         cohortTable = input$cohortTable,
                         createProtocol = input$createProtocol,
                         runDiagnostic = input$runDiagnostic,
                         viewDiagnostic = input$viewDiagnostic,
                         createCohorts = input$createCohorts,
                         runAnalyses = input$runAnalyses,
                         createResultsDoc = input$createResultsDoc ,
                         packageResults = input$packageResults,
                         createValidationPackage = input$createValidationPackage,
                         minCellCount= 5
    )
    }

  })

}

getOhdsiStudies <- function(){
  req <- httr::GET("https://api.github.com/orgs/ohdsi-studies/repos")
  ohdsiStudies <- unlist(lapply(content(req), function(x) x$full_name), use.names = F)
  # could grep on description as well
  predictions <- union(grep('predict', tolower(ohdsiStudies)),grep('valida', tolower(ohdsiStudies)))
  return(ohdsiStudies[predictions])
}

getlibraries <- function(){
  res <- installed.packages()
  libraries <- res[,1]
  potentialStudies <- lapply(1:length(libraries),
                             function(i) length(grep('PatientLevelPrediction', res[i,'Imports'])>0)
  )
  return(names(libraries)[unlist(potentialStudies) == 1])
}

runStudy <- function(studyPackage,
                     outputFolder,
                     dbms,
                     user,
                     pw,
                     server,
                     port,
                     cdmDatabaseSchema,
                     cdmDatabaseName,
                     cohortDatabaseSchema,
                     oracleTempSchema,
                     cohortTable,
                     createProtocol = F,
                     runDiagnostic = F,
                     viewDiagnostic = F,
                     createCohorts = T,
                     runAnalyses = T,
                     createResultsDoc = F,
                     packageResults = F,
                     createValidationPackage = F,
                     minCellCount= 5
                     ){

  library(studyPackage)
  connectionDetails <- DatabaseConnector::createConnectionDetails(dbms = dbms,
                                                                  server = server,
                                                                  user = user,
                                                                  password = pw,
                                                                  port = port)

  executeArgs <- list(connectionDetails = connectionDetails,
          cdmDatabaseSchema = cdmDatabaseSchema,
          cdmDatabaseName = cdmDatabaseName,
          cohortDatabaseSchema = cohortDatabaseSchema,
          cohortTable = cohortTable,
          oracleTempSchema = oracleTempSchema,
          outputFolder = outputFolder,
          createProtocol = createProtocol,
          runDiagnostic = runDiagnostic,
          viewDiagnostic = viewDiagnostic,
          createCohorts = createCohorts,
          runAnalyses = runAnalyses,
          createResultsDoc = createResultsDoc,
          packageResults = packageResults,
          createValidationPackage = createValidationPackage,
          minCellCount = minCellCount)

  do.call(what = 'execute', args = executeArgs,
          envir = as.environment(paste0("package:",studyPackage)))
}


installStudyModal <- function(ns) {
  shiny::modalDialog(

    shiny::h1('Installation'),

    shinydashboard::box(title = paste0('Build or download a prediction study:'), width = '60%',

                        # select file and build R project
                        shiny::textInput(inputId = ns('packageLoc'),
                                         label = 'Location of Rproject:', value = ''),
                        shiny::actionButton(ns('installStudyLoc'), 'Install Study From File'),
                        # select a github prediction study and install package
                        shiny::uiOutput(ns('selectOhdsiStudy')),
                        shiny::actionButton(ns('installStudyGithub'), 'Install Study From Github')
    ),

    footer = shiny::tagList(
      shiny::modalButton("Cancel")
    )
  )
}


executeStudyModal <- function(ns) {
  shiny::modalDialog(

    shiny::h1('Execution'),

    shinydashboard::box(title = paste0('Execute the study:'), width = '60%',

                        # select file and build R project
                        shiny::uiOutput(ns('selectLibrary')),
                        shiny::textInput(ns('dbms'), label = 'Database management system:', placeholder = 'e.g., redshift'),
                        shiny::textInput(ns('user'), label = 'Username for connection:'),
                        shiny::passwordInput(ns('pw'), label = 'Password for connection:'),
                        shiny::passwordInput(ns('server'), label = 'Server:'),
                        shiny::textInput(ns('port'), label = 'Port:'),

                        shiny::textInput(ns('cdmDatabaseSchema'), label = 'Database Schema for CDM:', placeholder = 'e.g., myCdm.dbo'),
                        shiny::textInput(ns('cdmDatabaseName'), label = 'Database Name (friendly name for sharing):', placeholder = 'e.g., myCdm'),
                        shiny::textInput(ns('cohortDatabaseSchema'), label = 'Database Schema for saving results (need write access):', placeholder = 'e.g., scratch.dbo'),
                        shiny::textInput(ns('cohortTable'), label = 'Name of table to saving cohort results:', placeholder = 'e.g., theBestStudyTable'),
                        shiny::textInput(ns('oracleTempSchema'), label = 'The temp schema (only for oracle users):', value = NULL, placeholder = 'e.g., NULL'),
                        shiny::textInput(ns('outputFolder'), label = 'Location to save results:', value = file.path(getwd(),'plpStudy'), placeholder = 'e.g., C:/myresults'),
                        shiny::checkboxInput(ns('createProtocol'), label = 'Create a study protocol', value = F),
                        shiny::checkboxInput(ns('runDiagnostic'), label = 'Run a diagnostic to inspect data before running models', value = F),
                        shiny::checkboxInput(ns('createCohorts'), label = 'Create the study cohorts', value = T),
                        shiny::checkboxInput(ns('runAnalyses'), label = 'Run the development and/or validation analysis', value = T),
                        shiny::checkboxInput(ns('createResultsDoc'), label = 'Create a word document with the results', value = F),
                        shiny::checkboxInput(ns('packageResults'), label = 'Remove sensitive data and package results for sharing', value = F),
                        shiny::checkboxInput(ns('createValidationPackage'), label = 'Create a validation package for the developed models', value = F),
                        shiny::numericInput(ns('minCellCount'), label = 'Min cell count for a result to be displayed:', value = 5, min = 1, max = 1000)
    ),

    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns('executeStudy'), label = 'Run Study')
    )
  )
}
