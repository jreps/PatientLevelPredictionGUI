analysisViewer <- function(id, label = "Model") {
  ns <- shiny::NS(id)

  shinydashboard::box(title = "Analysis Settings:", width = '60%',

                      shiny::textInput(inputId = ns('packageName'),
                                       label = 'Package Name:' ,
                                       placeholder = 'Example'),

                      shiny::textAreaInput(inputId = ns('packageDescription'),
                                           label = 'Package Description:' ,
                                           placeholder = 'an example of the skeleton'),

                      shiny::textInput(inputId = ns('createdBy'),
                                       label = 'Created by:' ,
                                       placeholder = 'your name'),

                      shiny::textInput(inputId = ns('organizationName'),
                                       label = 'Organization name:' ,
                                       placeholder = 'your organization')

  )
}

analysisServer <- #function(id) {
  #moduleServer(
  #id,
  function(input, output, session) {
    analysisList <- shiny::reactiveVal(value = list())
    shiny::observeEvent(input$packageName, {
      oldList <- analysisList()
      oldList$packageName <- input$packageName
      analysisList(oldList)
    })
    shiny::observeEvent(input$packageDescription, {
      oldList <- analysisList()
      oldList$packageDescription <- input$packageDescription
      analysisList(oldList)
    })
    shiny::observeEvent(input$createdBy, {
      oldList <- analysisList()
      oldList$createdBy <- input$createdBy
      analysisList(oldList)
    })
    shiny::observeEvent(input$organizationName, {
      oldList <- analysisList()
      oldList$organizationName <- input$organizationName
      analysisList(oldList)
    })

    return(analysisList)
  }
#)

#}
