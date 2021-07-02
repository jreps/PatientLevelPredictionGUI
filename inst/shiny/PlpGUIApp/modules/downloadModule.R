downloadViewer <- function(id, label = "download") {
  ns <- shiny::NS(id)

  shinydashboard::box(title = "Download:", width = '60%',

                      # check webApi
                      shinyDirectoryInput::directoryInput(ns('outputPackageLocation'),
                                                          label = 'select a directory to save study package:'),
                      shiny::actionButton(inputId = ns('hydrate'),
                                          label = 'Download')

  )
}

downloadServer <- #function(id) {
  #moduleServer(
  #id,
  function(input, output, session,
           jsonForStudy, analysisList) {

    shiny::observeEvent(input$hydrate, {
      if(jsonForStudy() != ''){
        createPackage(jsonForStudy(),
                      outputPackageLocation = file.path(readDirectoryInput(session, 'outputPackageLocation'), analysisList()$packageName),
                      outputJsonLocation = NULL,
                      jsonName = 'predictionAnalysisList.json')
      } else{

      }

    })




    shiny::observeEvent(
      ignoreNULL = TRUE,
      eventExpr = {
        input$outputPackageLocation
      },
      handlerExpr = {
        if (input$outputPackageLocation > 0) {
          # condition prevents handler execution on initial app launch

          # launch the directory selection dialog with initial path read from the widget
          path = choose.dir(default = readDirectoryInput(session, 'outputPackageLocation'))

          # update the widget value
          updateDirectoryInput(session, 'outputPackageLocation', value = path)
        }
      }
    )

  }


# generic - calls Hydra for json specification
createPackage <- function(jsonForStudy,
                          outputPackageLocation = 'D:/testing/package',
                          outputJsonLocation = NULL,
                          jsonName = 'predictionAnalysisList.json'){

  # save json
  if(is.null(outputJsonLocation)){
    outputJsonLocation <- PatientLevelPrediction:::createTempModelLoc()
  }
  if(!dir.exists(outputJsonLocation)){
    dir.create(outputJsonLocation, recursive = T)
  }
  write(jsonForStudy, file=file.path(outputJsonLocation, jsonName))

  specifications <- Hydra::loadSpecifications(file.path(outputJsonLocation, jsonName))

  if(dir.exists(outputPackageLocation)){
    shiny::showNotification("Location Exists - pick a different outputPackageLocation")
  } else {
    Hydra::hydrate(specifications = specifications, outputFolder = outputPackageLocation)
  }
}

