appDir <- system.file("shiny", "PlpGUIApp", package = "PatientLevelPredictionGUI")
source(file.path(appDir,"helpers","codeToPopSkeletons.R"))

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
           jsonForStudy, analysisList,
           baseUrl) {

    shiny::observeEvent(input$hydrate, {
      if(jsonForStudy() != ''){
        createPackage(jsonForStudy = jsonForStudy(),
                      outputPackageLocation = file.path(readDirectoryInput(session, 'outputPackageLocation')),
                      packageName = analysisList()$packageName,
                      skeletonType = jsonForStudy()$skeletonType,
                      jsonName = 'predictionAnalysisList.json',
                      baseUrl = baseUrl())
      } else{
        shiny::showNotification("Create json settings first and retry")
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
                          outputPackageLocation = 'D:/testing',
                          packageName = 'testpackage',
                          skeletonType = 'SkeletonPredictionStudy',
                          jsonName = 'predictionAnalysisList.json',
                          baseUrl ){

  if(dir.exists(file.path(outputPackageLocation, packageName) )){
    shiny::showNotification("Location Exists - pick a different outputPackageLocation or packageName")
  } else {
    dir.create(file.path(outputPackageLocation, packageName), recursive = T)
    #Hydra::hydrate(specifications = specifications, outputFolder = outputPackageLocation)
    createPackage <- tryCatch({downLoadSkeleton(outputFolder = outputPackageLocation,
                                                packageName = packageName,
                                                skeletonType = skeletonType)#'SkeletonPredictionValidationStudy')
    }, error = function(e){shiny::showNotification(e); return(NULL)})

    if(!is.null(createPackage)){
      createPackage <- tryCatch({replaceName(packageLocation = file.path(outputPackageLocation,  packageName),
                                             packageName = packageName,
                                             skeletonType = skeletonType)},
                                error = function(e){shiny::showNotification(e); return(NULL)})
      if(!is.null(createPackage)){
        createPackage <- tryCatch({saveAnalysisJson(packageLocation = file.path(outputPackageLocation,  packageName),
                                                    analysisList = jsonForStudy)},
                                  error = function(e){shiny::showNotification(e); return(NULL)})

        if(!is.null(createPackage)){
          createPackage <- tryCatch({saveCohorts(packageLocation = file.path(outputPackageLocation,  packageName),
                                                 analysisList = jsonForStudy,
                                                 baseUrl = baseUrl)},
                                    error = function(e){shiny::showNotification(e); return(NULL)})

          if(!is.null(createPackage)){
            shiny::showNotification("Skeleton package created")
          } else{
            shiny::showNotification("Saving cohort issue")
          }

        }}}

  }
}

