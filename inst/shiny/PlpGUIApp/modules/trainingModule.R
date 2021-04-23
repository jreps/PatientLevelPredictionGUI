trainViewer <- function(id, label = "Model") {
  ns <- shiny::NS(id)
  shinydashboard::box(title = "Execution Settings:", width = '60%',
                      shiny::numericInput(inputId = ns('minCovariateFraction'),
                                          label = 'Min Covariate Fraction:',
                                          value = 0.001, min = 0, max = 1, step = 0.0001),
                      shiny::checkboxInput(inputId = ns('normalizeData'),
                                           label = 'Normalize Data:', value = F),
                      shiny::selectInput(inputId = ns('testSplit'), label = 'Test Split Type:',
                                         choices = c('stratified','time','subject'),
                                         multiple = F),
                      shiny::numericInput(inputId = ns('testFraction'),
                                          label = 'Fraction of data to use as test set:',
                                          value = 0.25, min = 0, max = 1, step = 0.01),
                      shiny::textInput(inputId = ns('splitSeed'),
                                       label = 'Split Seed:' ,
                                       placeholder = '123'),
                      shiny::numericInput(inputId = ns('nfold'),
                                          label = 'CV nfold:',
                                          value = 3, min = 1, max = 100, step = 1)
  )
}

trainServer <- #function(id) {
  #moduleServer(
  #id,
  function(input, output, session) {
    executeList <- shiny::reactiveVal(value = list())
    shiny::observeEvent(input$minCovariateFraction, {
      oldList <- executeList()
      oldList$minCovariateFraction <- input$minCovariateFraction
      executeList(oldList)
    })
    shiny::observeEvent(input$normalizeData, {
      oldList <- executeList()
      oldList$normalizeData <- input$normalizeData
      executeList(oldList)
    })
    shiny::observeEvent(input$testSplit, {
      oldList <- executeList()
      oldList$testSplit <- input$testSplit
      executeList(oldList)
    })
    shiny::observeEvent(input$testFraction, {
      oldList <- executeList()
      oldList$testFraction <- input$testFraction
      executeList(oldList)
    })
    shiny::observeEvent(input$splitSeed, {
      oldList <- executeList()
      oldList$splitSeed <- input$splitSeed
      executeList(oldList)
    })
    shiny::observeEvent(input$nfold, {
      oldList <- executeList()
      oldList$nfold <- input$nfold
      executeList(oldList)
    })

    return(executeList)
  }
#)

#}
