cohortViewer <- function(id, labelv = "cohorts") {
  ns <- NS(id)
  shinydashboard::box(title = labelv, width = '60%',
                      
                      # add table with existing models
                      
                      shiny::dataTableOutput(ns('cohortTable')),                 

                      shiny::textInput(inputId = ns('cohortName'), 
                                       label = paste(labelv,'Name:'), 
                                       value = '', 
                                       placeholder = ''
                                      ),
                      shiny::numericInput(inputId = ns('cohortId'), 
                                       label = paste0(labelv, 'Id:'), 
                                       value = 1, 
                                       min = 1, 
                                       max = 100000, 
                                       step = 1
                      ),
                                              
                     shiny::actionButton(inputId = ns('addCohort'), 
                                         label = paste0('Add', labelv))
  )
}

cohortServer <- #function(id) {
  #moduleServer(
  #id,
  function(input, output, session,
           cohortReactive) {
    cohortList <- shiny::reactiveVal(data.frame())
    
    observeEvent(input$addCohort, {
      oldList <- cohortList()
    
      if(!(nrow(oldList)>0 && (input$cohortName%in%oldList$Name || ifelse(!is.null(input$cohortId),input$cohortId,oldList$Id[1])%in%oldList$Id))){
        oldList <- rbind(oldList,
                         data.frame(Name = input$cohortName, 
                                    Id = input$cohortId,  
                                    Remove = as.character(actionButton(paste0('button_', input$cohortId),label = "Edit", onclick = sprintf("Shiny.onInputChange('%s', this.id, {priority: \"event\"})", session$ns("delete_button"))))
                         )
        )}
        cohortList(oldList)
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
    observeEvent(input$delete_button, {
      selectedRow(as.numeric(strsplit(input$delete_button, "_")[[1]][2]))
      print(selectedRow())
      showModal(cohortModalDelete(session$ns))
    })
    observeEvent(input$deleteCohort, {
      indId <- selectedRow()
      oldList <- cohortList()
      oldList <- oldList[oldList$Id!=indId,]
      cohortList(oldList)
      removeModal()
    })
    #=================
    
    
    return(cohortList)
  }

cohortModalDelete <- function(ns) {
  modalDialog(
    shiny::h1('Delete Cohort?'),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton(ns('deleteCohort'), 'Yes')
    )
  )
}