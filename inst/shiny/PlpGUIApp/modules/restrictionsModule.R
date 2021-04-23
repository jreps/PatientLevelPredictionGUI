restrictionViewer <- function(id, label = "Population/Outcome") {
  ns <- shiny::NS(id)

  shinydashboard::box(title = paste(label, "Restrictions:"), width = '60%',

                      shiny::dataTableOutput(ns('restrictionsTable')),
                      shiny::actionButton(inputId = ns('restrictionAddViewer'),
                                          label = 'Add')

  )
}

restrictionServer <- #function(id) {
  #moduleServer(
  #id,
  function(input, output, session,
           options1Names = NULL, options1Vals = NULL,
           options2Names = NULL, options2Vals = NULL,
           colnameValues = c('Setting1', 'Setting2')) {

    # create the list of population settings
    restrictList <- shiny::reactiveVal(c())
    selectedRow <- shiny::reactiveVal()

    shiny::observeEvent(input$restrictionAddViewer, {
        #open editor module:
      if(!is.null(options1Names) && !is.null(options2Names)){
        choices1 <- as.list(options1Vals)
        names(choices1) <- options1Names
        choices2 <- as.list(options2Vals)
        names(choices2) <- options2Names
        shiny::showModal(restrictionAddModule(session$ns, colnameVals = colnameValues,
                                      choices1 = choices1 , choices2 = choices2))
      }
    })

    shiny::observeEvent(input$addRestriction, {
      # add the settings to the data.frame
      oldresult <- restrictList()
      newData <- data.frame( val1 = input$setting1,
                             val2 = input$setting2)
      colnames(newData) <- colnameValues
      newresult <- rbind(oldresult, newData)
      restrictList(newresult)
      shiny::removeModal()
    })

    # create function for edit/delete buttoms in table:
    shinyInput <- function(FUN, len, id, ...) {
      inputs <- character(len)
      for (i in seq_len(len)) {
        inputs[i] <- as.character(FUN(paste0(id, i), ...))
      }
      inputs
    }

    # display the restrictions
    output$restrictionsTable <- shiny::renderDataTable({
      if(length(restrictList())>0){
        result <- data.frame(settings1 = as.character(restrictList()[,1]),
                             settings2 = as.character(restrictList()[,2]),
                             stringsAsFactors = FALSE,
                             row.names = 1:nrow(restrictList()),
                             Remove = shinyInput(actionButton, nrow(restrictList()), 'pbutton_', label = "Remove", onclick = sprintf("Shiny.onInputChange('%s', this.id, {priority: \"event\"})", session$ns("delete_button"))  )
        )
        colnames(result)[1:2] <- colnameValues
        result
        } else{
          NULL
        }
    }, escape = FALSE)



    # DELETE
    #=================
    # if clicked popup asks for confirmation and then deletes covariate setting
    shiny::observeEvent(input$delete_button, {
      selectedRow(as.numeric(strsplit(input$delete_button, "_")[[1]][2]))
      shiny::showModal(restrictionModalDelete(session$ns))
    })
    shiny::observeEvent(input$deleteRestriction, {
      indVal <- selectedRow()
      oldList <- restrictList()
      oldList <- oldList[-indVal,]
      restrictList(oldList)
      shiny::removeModal()
    })
    #=================


    return(restrictList)
  }


restrictionAddModule <- function(ns, colnameVals, choices1, choices2) {
  shiny::modalDialog(

    shiny::selectInput(inputId = ns('setting1'), label = colnameVals[1], choices = choices1, selected = 1),
    shiny::selectInput(inputId = ns('setting2'), label = colnameVals[2], choices = choices2, selected = 1),

    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns('addRestriction'), 'Add')
    )
  )
}

restrictionModalDelete <- function(ns) {
  shiny::modalDialog(

    # select the type of covariate to add to the covarite Name
    shiny::h2('Delete Population?'),

    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns('deleteRestriction'), 'Yes')
    )
  )
}
