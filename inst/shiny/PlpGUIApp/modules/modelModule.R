modelViewer <- function(id, label = "Model") {
  ns <- shiny::NS(id)

  shinydashboard::box(title = "Model:", width = '60%',

                      # add table with existing models

                      shiny::dataTableOutput(ns('modelTable')),

                      # add a model
                      shiny::selectInput(inputId = ns('modelSelect'), label = 'Model:',
                                         choices = c('LassoLogisticRegression', 'GradientBoostingMachine', 'RandomForest'),
                                         multiple = F),
                      shiny::actionButton(inputId = ns('modelAdd'),
                                          label = 'Add Model')

  )
}

modelServer <- #function(id) {
  #moduleServer(
  #id,
  function(input, output, session) {

    # create the list of population settings
    modelList <- shiny::reactiveVal(list())
    # create an variable to show which index is being updated
    selectedRow <- shiny::reactiveVal()

    shiny::observeEvent(input$modelAdd, {

      # add a new model to the list
      oldList <- modelList()
      i <- length(oldList)
      oldList[[i+1]] <- list(name = input$modelSelect,
                             settings = list())
      modelList(oldList)

      # set the row selector
      selectedRow(i+1)

      # now show editor
      type <- getType(input$modelSelect)
      shiny::showModal(do.call(paste0('modelModal',type),
                        list(ns = session$ns, model = list())))

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
    output$modelTable <- shiny::renderDataTable({
      if(length(modelList())>0){
        data.frame(Model = unlist(lapply(modelList(), function(x) x$name)),
                   Setting = getSetting(modelList()),
                   Edit= shinyInput(actionButton, length(modelList()), 'pbutton_', label = "Edit", onclick = sprintf("Shiny.onInputChange('%s', this.id, {priority: \"event\"})", session$ns("select_button"))  ),
                   stringsAsFactors = FALSE,
                   row.names = 1:length(modelList()),
                   Remove = shinyInput(actionButton, length(modelList()), 'pbutton_', label = "Remove", onclick = sprintf("Shiny.onInputChange('%s', this.id, {priority: \"event\"})", session$ns("delete_button"))  )

        )} else{
          NULL
        }
    }, escape = FALSE)

    # UPDATE POP
    #=================
    # Show modal when button is clicked.
    shiny::observeEvent(input$select_button, {
      selectedRow(as.numeric(strsplit(input$select_button, "_")[[1]][2]))
      oldList <- modelList()[[selectedRow()]]

      type <- getType(oldList$name)
      shiny::showModal(do.call(paste0('modelModal',type), list(ns = session$ns, model = oldList)))

    })
    # When button is pressed, update covariate
    # update the covariate with new settings
    shiny::observeEvent(input$addLR, {
      indVal <- selectedRow()
      oldList <- modelList()
      oldList[[indVal]] <- list( name = 'LassoLogisticRegression',
                                 settings = list(variance = input$LRvariance,
                                                 threads = input$LRthreads,
                                                 seed = input$LRseed)
      )
      modelList(oldList)
      shiny::removeModal()
    })

    shiny::observeEvent(input$addGBM, {
      indVal <-selectedRow()
      oldList <- modelList()
      oldList[[indVal]] <- list( name = 'GradientBoostingMachine',
                                 settings = list(ntrees = input$GBMntrees,
                                                 nthread = input$GBMnthread,
                                                 earlyStopRound = input$GBMearlyStopRound,
                                                 maxDepth = input$GBMmaxDepth,
                                                 minRows = input$GBMminRows,
                                                 learnRate = input$GBMlearnRate,
                                                 seed = input$GBMseed
                                 ))
      modelList(oldList)
      shiny::removeModal()
    })
    #=================

    # DELETE
    #=================
    # if clicked popup asks for confirmation and then deletes covariate setting
    shiny::observeEvent(input$delete_button, {
      selectedRow(as.numeric(strsplit(input$delete_button, "_")[[1]][2]))
      shiny::showModal(modelModalDelete(session$ns))
    })
    shiny::observeEvent(input$deleteModel, {
      indVal <- selectedRow()
      oldList <- modelList()
      oldList[[indVal]] <- NULL
      modelList(oldList)
      shiny::removeModal()
    })
    #=================


    return(modelList)
  }




modelModalGBM <- function(ns, model = list()) {
  modalDialog(

    shiny::textInput(inputId = ns('GBMntrees'),
                     label = 'N trees:',
                     value = ifelse(!is.null(model$settings$ntrees),model$settings$ntrees, '50,500'),
                     placeholder = ifelse(!is.null(model$settings$ntrees),model$settings$ntrees, '50,500')
    ),

    shiny::textInput(inputId = ns('GBMmaxDepth'),
                     label = 'Max Depth:',
                     value = ifelse(!is.null(model$settings$maxDepth),model$settings$maxDepth, '2,4,7,10'),
                     placeholder = ifelse(!is.null(model$settings$maxDepth),model$settings$maxDepth, '2,4,7,10')
    ),
    shiny::textInput(inputId = ns('GBMearlyStopRound'),
                     label = 'Early Stop Round:',
                     value = ifelse(!is.null(model$settings$earlyStopRound),model$settings$earlyStopRound,'10'),
                     placeholder = ifelse(!is.null(model$settings$earlyStopRound),model$settings$earlyStopRound,'10')
    ),
    shiny::textInput(inputId = ns('GBMminRows'),
                     label = 'Min Rows:',
                     value = ifelse(!is.null(model$settings$minRows),model$settings$minRows, '5,10,100'),
                     placeholder = ifelse(!is.null(model$settings$minRows),model$settings$minRows, '5,10,100')
    ),
    shiny::textInput(inputId = ns('GBMlearnRate'),
                     label = 'Learn Rate:',
                     value = ifelse(!is.null(model$settings$learnRate),model$settings$learnRate, '0.1'),
                     placeholder = ifelse(!is.null(model$settings$learnRate),model$settings$learnRate, '0.1')
    ),
    shiny::numericInput(inputId = ns('GBMseed'),
                        label = 'Seed:',
                        value = ifelse(!is.null(model$settings$seed),model$settings$seed,111),
                        min = 1,
                        max = 100000,
                        step = 1),
    shiny::numericInput(inputId = ns('GBMnthread'),
                        label = 'Ntread:',
                        value = ifelse(!is.null(model$settings$nthread),model$settings$nthread,-1),
                        min = -1,
                        max = 100,
                        step = 1),

    footer = shiny::tagList(
      shiny::actionButton(ns('addGBM'), 'Update GBM')
    )

  )}

modelModalLR <- function(ns, model = list()) {
  shiny::modalDialog(
    shiny::numericInput(inputId = ns('LRvariance'),
                        label = 'Variance:',
                        value = ifelse(!is.null(model$settings$variance),model$settings$variance, 0.001),
                        min = 0,
                        max = 1000,
                        step = 0.001),
    shiny::numericInput(inputId = ns('LRseed'),
                        label = 'Seed:',
                        value = ifelse(!is.null(model$settings$seed),model$settings$seed,111),
                        min = 1,
                        max = 100000,
                        step = 1),
    shiny::numericInput(inputId = ns('LRthreads'),
                        label = 'Treads:',
                        value = ifelse(!is.null(model$settings$nthread),model$settings$threads,-1),
                        min = -1,
                        max = 100,
                        step = 1),

    footer = shiny::tagList(
      shiny::actionButton(ns('addLR'), 'Update LR')
    )
  )
}

modelModalDelete <- function(ns) {
  shiny::modalDialog(

    # select the type of covariate to add to the covarite Name
    shiny::h2('Delete Model?'),

    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns('deleteModel'), 'Yes')
    )
  )
}


getType <- function(modelSelect){
  type <- 'GBM'
  if(modelSelect == 'LassoLogisticRegression'){
    type <- 'LR'
  }
  if(modelSelect == 'RandomForest'){
    type <- 'RF'
  }

  return(type)
}

getSetting <- function(modelList){

  unlist(lapply(modelList, function(x) paste0(rbind(names(unlist(x$settings)), unlist(x$settings)), collapse=':', sep=' ')))

}
