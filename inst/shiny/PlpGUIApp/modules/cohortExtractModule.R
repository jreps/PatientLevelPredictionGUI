extractCohortsViewer <- function(id) {
  ns <- shiny::NS(id)

  shiny::div(id = ns(id),
             shiny::actionButton(inputId = ns('extractCohorts'),
                                 label = 'Load Cohorts')

  )
}

extractCohortsServer <- function(input, output, session, webApi) {

  cohortReactive <- shiny::reactiveVal(data.frame())

  shiny::observeEvent(input$extractCohorts, {
    if(webApi() != ''){
      cohorts <- ROhdsiWebApi::getCohortDefinitionsMetaData(webApi())[,c('id', 'name')]
      cohortReactive(as.data.frame(cohorts))
      showNotification(paste("Cohort Extracted"), duration = 0, type = 'message')

    } else{
      cohortReactive(data.frame())
      showNotification(paste("Cohort Extraction Failed As No WebApi Connection"), duration = 0, type = 'error')
    }
  })

  return(cohortReactive)
}
