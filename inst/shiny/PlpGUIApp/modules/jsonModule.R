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
    createDevelopmentStudyJsonList <- shiny::reactiveVal(list())
    valid <- shiny::reactiveVal('')

    shiny::observeEvent(input$jsonGenerate, {
      valid(checkPredictionSkeleton(analysisList(),
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

      if(valid() == ''){
        createDevelopmentStudyJsonList(formatSettings(analysisList(),
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
      } else{
        shiny::showNotification(valid(), duration = 5, type = 'error')
      }

      if(length(createDevelopmentStudyJsonList())>0 && webApi() != ''){
        jsonForStudy(createJson(createDevelopmentStudyJsonList(), webApi() ))
        ##output$jsonCode <- shiny::renderPrint(js)
        output$jsonCode <- shiny::renderUI({shiny::HTML(gsub('\n','', gsub("/n", "<br/>",
                                                                           jsonForStudy()
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
