appDir <- system.file("shiny", "PlpGUIApp", package = "PatientLevelPredictionGUI")
source(file.path(appDir,"helpers","HelperCheckInputs.R"))
source(file.path(appDir,"helpers","HelperFormatInput.R"))
source(file.path(appDir,"helpers","createDevelopmentStudyJson.R"))

jsonViewer <- function(id, label = "Json") {
  ns <- shiny::NS(id)

  shinydashboard::box(title = "Json:", width = '60%',

                      # check webApi
                      shiny::actionButton(inputId = ns('jsonGenerate'),
                                          label = 'View Json'),
                      shiny::htmlOutput(ns('jsonCode'))

  )
}

jsonServer <- #function(id) {
  #moduleServer(
  #id,
  function(input, output, session,
           type = 'development',
           analysisList,
           targetList,
           outcomeList,
           modelList,
           covList,
           popList,
           OPList,
           MCList,
           TCList,
           executeList,
           webApi) {


    jsonForStudy <- shiny::reactiveVal('')
    createStudyJsonList <- shiny::reactiveVal(list())
    valid <- shiny::reactiveVal('')

    shiny::observeEvent(input$jsonGenerate, {
      if(type == 'development'){
      valid(checkPredictionSkeleton(analysisList(),
                                    targetList(),
                                    outcomeList(),
                                    modelList(),
                                    covList(),
                                    popList(),
                                    OPList(),
                                    MCList(),
                                    TCList(),
                                    executeList()))
      }else{
        valid(checkValidationSkeleton(analysisList(),
                                      modelList()))
      }

      if(valid() == '' && type == 'development'){
        createStudyJsonList(formatSettings(analysisList(),
                                                      targetList(),
                                                      outcomeList(),
                                                      modelList(),
                                                      covList(),
                                                      popList(),
                                                      OPList(),
                                                      MCList(),
                                                      TCList(),
                                                      executeList()
        ))
      } else if(valid() == '' && type == 'validation'){
        settings <- formatValidationSettings(analysisList(),
                                 modelList())
        createStudyJsonList(settings)
      }
      else{
        shiny::showNotification(valid(), duration = 5, type = 'error')
      }

      if(length(createStudyJsonList())>0 && webApi() != ''){
        json <- createJson(type, createStudyJsonList(), webApi() )
        jsonForStudy(json)
        output$jsonCode <- shiny::renderUI({shiny::HTML(gsub('\n','', gsub("/n", "<br/>",
                                                                           RJSONIO::toJSON(jsonForStudy(), digits = 23)
        ))
        )
        })
      } else{
        jsonForStudy('')
        if(webApi() == ''){
          shiny::showNotification('webApi not set', duration = 5, type = 'error')
        } else {
          shiny::showNotification('Incorrect settings for JSON', duration = 5, type = 'error')
        }
      }

    })

    return(jsonForStudy)

  }


createJson <- function(type,createStudyJsonList, webApi){

  # this write
  if(type == 'development'){
    createStudyJsonList$webApi <- webApi
    jsonForStudy <- do.call('createDevelopmentStudyJson', createStudyJsonList)
  }
  if(type == 'validation'){
    jsonForStudy <- createStudyJsonList
  }

  return(jsonForStudy)
}
