populationViewer <- function(id, label = "Population") {
  ns <- shiny::NS(id)

  shinydashboard::box(title = "Population:", width = '60%',

                      # add table with existing models

                      shiny::dataTableOutput(ns('populationTable')),

                      # add a covariate
                      shiny::textInput(inputId = ns('populationName'),
                                       label = 'New Population Name:'),
                      shiny::actionButton(inputId = ns('populationAdd'),
                                          label = 'Add New Population')

  )
}

populationServer <- #function(id) {
  #moduleServer(
  #id,
  function(input, output, session) {

    # create the list of population settings
    popList <- shiny::reactiveVal()
    selectedRow <- shiny::reactiveVal()


    shiny::observeEvent(input$populationAdd, {
      if(!is.null(input$populationName) && input$populationName!="" && !input$populationName%in%names(popList())){
        oldList <- popList()
        i <- length(oldList)
        oldList[[i+1]] <- list(binary = T,
                               includeAllOutcomes = F,
                               firstExposureOnly = T,
                               washoutPeriod = 365,
                               removeSubjectsWithPriorOutcome = T,
                               priorOutcomeLookback = 9999,
                               requireTimeAtRisk = F,
                               minTimeAtRisk = 1,
                               riskWindowStart = 1,
                               startAnchor = 'cohort start',
                               riskWindowEnd = 365,
                               endAnchor = 'cohort start')
        names(oldList)[i+1] <- input$populationName
        popList(oldList)

        # set selector
        selectedRow(i+1)

        #open editor module:
        shiny::showModal(populationModal(session$ns, popList()[[i+1]] ))

      }
    })

    # create function for edit/delete buttoms in table:
    shinyInput <- function(FUN, len, id, ...) {
      inputs <- character(len)
      for (i in seq_len(len)) {
        inputs[i] <- as.character(FUN(paste0(id, i), ...))
      }
      inputs
    }

    # display the covariates
    output$populationTable <- shiny::renderDataTable({
      if(length(popList())>0){
        data.frame(Name = names(popList()),
                   Settings = getPopSettings(popList()),
                   Edit= shinyInput(actionButton, length(names(popList())), 'pbutton_', label = "Edit", onclick = sprintf("Shiny.onInputChange('%s', this.id, {priority: \"event\"})", session$ns("select_button"))  ),
                   stringsAsFactors = FALSE,
                   row.names = 1:length(names(popList())),
                   Remove = shinyInput(actionButton, length(names(popList())), 'pbutton_', label = "Remove", onclick = sprintf("Shiny.onInputChange('%s', this.id, {priority: \"event\"})", session$ns("delete_button"))  )

        )} else{
          NULL
        }
    }, escape = FALSE)


    # UPDATE POP
    #=================
    # Show modal when button is clicked.
    shiny::observeEvent(input$select_button, {
      selectedRow(as.numeric(strsplit(input$select_button, "_")[[1]][2]))
      oldList <- popList()[[selectedRow()]]
      shiny::showModal(populationModal(session$ns, oldList))
    })
    # When button is pressed, update covariate
    # update the covariate with new settings
    shiny::observeEvent(input$addPops, {
      indVal <-selectedRow()
      oldList <- popList()
      oldList[[indVal]] <- list(binary = input$binary,
                                includeAllOutcomes = input$includeAllOutcomes,
                                firstExposureOnly = input$firstExposureOnly,
                                washoutPeriod = input$washoutPeriod,
                                removeSubjectsWithPriorOutcome = input$removeSubjectsWithPriorOutcome,
                                priorOutcomeLookback = input$priorOutcomeLookback,
                                requireTimeAtRisk = input$requireTimeAtRisk,
                                minTimeAtRisk = input$minTimeAtRisk,
                                riskWindowStart = input$riskWindowStart,
                                startAnchor = input$startAnchor,
                                riskWindowEnd = input$riskWindowEnd,
                                endAnchor = input$endAnchor)
      popList(oldList)
      shiny::removeModal()
    })
    #=================

    # DELETE POP
    #=================
    # if clicked popup asks for confirmation and then deletes covariate setting
    shiny::observeEvent(input$delete_button, {
      selectedRow(as.numeric(strsplit(input$delete_button, "_")[[1]][2]))
      shiny::showModal(populationModalDelete(session$ns))
    })
    shiny::observeEvent(input$deletePops, {
      indVal <- selectedRow()
      oldList <- popList()
      oldList[[indVal]] <- NULL
      popList(oldList)
      shiny::removeModal()
    })
    #=================


    return(popList)
  }


populationModal <- function(ns, settings = list()) {
  shiny::modalDialog(

    shiny::checkboxInput(inputId = ns('binary'),label = 'Binary', value = ifelse(!is.null(settings$binary),settings$binary, T)),
    shiny::checkboxInput(inputId = ns('includeAllOutcomes'),label = 'Include all outcomes', value = ifelse(!is.null(settings$includeAllOutcomes),settings$includeAllOutcomes,F)),
    shiny::checkboxInput(inputId = ns('firstExposureOnly'),label = 'First exposure only', value = ifelse(!is.null(settings$firstExposureOnly),settings$firstExposureOnly,T)),
    shiny::numericInput(inputId = ns('washoutPeriod'), label = 'Minimum prior observation', value = ifelse(!is.null(settings$washoutPeriod),settings$washoutPeriod,365), min = 0),
    shiny::checkboxInput(inputId = ns('removeSubjectsWithPriorOutcome'), label = 'Remove subjects with prior outcome', value = ifelse(!is.null(settings$removeSubjectsWithPriorOutcome),settings$removeSubjectsWithPriorOutcome,T)),
    shiny::conditionalPanel(ns = ns,
                            condition = "input.removeSubjectsWithPriorOutcome == T",
                            shiny::numericInput(inputId = ns('priorOutcomeLookback'), label = 'Prior outcome lookbackn', value = ifelse(!is.null(settings$priorOutcomeLookback),settings$priorOutcomeLookback,9999), min = 0)
    ),
    shiny::checkboxInput(inputId = ns('requireTimeAtRisk'),label = 'Require TAR', value = ifelse(!is.null(settings$requireTimeAtRisk),settings$requireTimeAtRisk,T)),
    shiny::conditionalPanel(ns = ns,
                            condition = "input.requireTimeAtRisk == T",
                            shiny::numericInput(inputId = ns('minTimeAtRisk'), label = 'Minimum time at risk', value = ifelse(!is.null(settings$minTimeAtRisk),settings$minTimeAtRisk,1), min = 0)
    ),
    shiny::numericInput(inputId = ns('riskWindowStart'), label = 'TAR start', value = ifelse(!is.null(settings$riskWindowStart),settings$riskWindowStart,1), min = 0),
    shiny::selectInput(inputId = ns('startAnchor'), label = 'TAR start relative to', choices = c('cohort start', 'cohort end'), selected = ifelse(!is.null(settings$startAnchor),settings$startAnchor,'cohort start')),

    shiny::numericInput(inputId = ns('riskWindowEnd'), label = 'TAR end', value = ifelse(!is.null(settings$riskWindowEnd),settings$riskWindowEnd,1), min = 0),
    shiny::selectInput(inputId = ns('endAnchor'), label = 'TAR end relative to', choices = c('cohort start', 'cohort end'), selected = ifelse(!is.null(settings$endAnchor),settings$endAnchor,'cohort start')),


    footer = shiny::tagList(
      shiny::actionButton(ns('addPops'), 'Update covariate')
    )
  )
}

populationModalDelete <- function(ns) {
  shiny::modalDialog(

    # select the type of covariate to add to the covarite Name
    shiny::h2('Delete Population?'),

    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns('deletePops'), 'Yes')
    )
  )
}


getPopSettings <- function(popList){

  unlist(lapply(popList, function(x) paste(names(unlist(x)), unlist(x), collapse='; ', sep=': ')))

}
