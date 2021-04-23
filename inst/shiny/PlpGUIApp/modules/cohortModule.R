cohortViewer <- function(id, labelv = "cohorts") {
  ns <- shiny::NS(id)
  shinydashboard::box(title = labelv, width = '60%',

                      # add table with existing models

                      shiny::dataTableOutput(ns('cohortTable')),

                      shiny::uiOutput(ns('cohortSelect')),

                      shiny::actionButton(inputId = ns('addCohort'),
                                          label = paste0('Add ', labelv))
  )
}

cohortServer <- #function(id) {
  #moduleServer(
  #id,
  function(input, output, session,
           cohortReactive) {

    # create the dropdown
    output$cohortSelect = shiny::renderUI({

      if(nrow(cohortReactive())>0){
        cohortVar <- as.list(cohortReactive()$id)
        names(cohortVar) <- paste0(cohortReactive()$id, ': ', cohortReactive()$name)

        shiny::selectizeInput(inputId = session$ns('cohort'),
                              label = 'Cohorts',
                              choices = cohortVar)
      }

    })

    cohortList <- shiny::reactiveVal(data.frame())

    shiny::observeEvent(input$addCohort, {

      if(nrow(cohortReactive())>0){
        ind <- cohortReactive()$id == input$cohort
        oldList <- cohortList()

        if(!cohortReactive()$id[ind] %in% oldList$Id){
          oldList <- rbind(oldList,
                           data.frame(Name = cohortReactive()$name[ind],
                                      Id = cohortReactive()$id[ind],
                                      Remove = as.character(actionButton(paste0('button_', cohortReactive()$id[ind]),label = "Delete", onclick = sprintf("Shiny.onInputChange('%s', this.id, {priority: \"event\"})", session$ns("delete_button"))))
                           )
          )
          cohortList(oldList)
        }
      } else{
        shiny::showNotification('Need to connect to a valid webApi to fetch the cohorts', duration = 5, type = 'error')
      }
    })

    output$cohortTable <- shiny::renderDataTable({
      if(nrow(cohortList())>0){
        cohortList()
      } else{
        NULL
      }
    }, escape = FALSE)


    # DELETE COV
    #=================
    selectedRow <- shiny::reactiveVal()
    # if clicked popup asks for confirmation and then deletes covariate setting
    shiny::observeEvent(input$delete_button, {
      selectedRow(as.numeric(strsplit(input$delete_button, "_")[[1]][2]))
      #print(selectedRow())
      shiny::showModal(cohortModalDelete(session$ns))
    })
    shiny::observeEvent(input$deleteCohort, {
      indId <- selectedRow()
      oldList <- cohortList()
      oldList <- oldList[oldList$Id!=indId,]
      cohortList(oldList)
      shiny::removeModal()
    })
    #=================


    return(cohortList)
  }

cohortModalDelete <- function(ns) {
  shiny::modalDialog(
    shiny::h1('Delete Cohort?'),

    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns('deleteCohort'), 'Yes')
    )
  )
}
