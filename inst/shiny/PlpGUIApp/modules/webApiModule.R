webApiViewer <- function(id) {
  ns <- shiny::NS(id)

  #shinydashboard::box(title = "WebApi:", width = '90%', solidHeader = T,
                      shiny::div(id = ns(id),

                      shiny::textInput(inputId = ns('baseUrl'),
                                       label = shiny::textOutput(ns('baseUrlCheck')) ,
                                       placeholder = 'http://api.ohdsi.org:8080/WebAPI',
                                       value = 'http://api.ohdsi.org:8080/WebAPI'),

                      shiny::checkboxInput(inputId = ns('authorizeWebApi'),
                                           label = 'requires authorization',
                                           value = FALSE),

                      shiny::conditionalPanel(condition = "input.authorizeWebApi", ns = ns,
                                             shiny::selectInput(inputId = ns("authMethod"),
                                                                label = "authMethod",
                                                                multiple = F,
                                                                choices = list(db='db',
                                                                               ad= 'ad',
                                                                               windows = 'windows'),
                                                                selected = 'Development'),
                                             shiny::textInput(inputId = ns('webApiUsername'),
                                                              label = 'webApiUsername' ,
                                                              placeholder = 'username'),
                                             shiny::passwordInput(inputId = ns('webApiPassword'),
                                                                  label = 'webApiPassword',
                                                                  value = 'password')

                      ),

                      shiny::actionButton(inputId = ns('connectwebApi'),
                                          label = 'Connect to webApi')

  )
}

webApiServer <- function(input, output, session) {

    webApi <- shiny::reactiveVal('')
    baseUrlCheck <- shiny::reactiveVal('WebAPI: ')
    output$baseUrlCheck <- shiny::renderText(baseUrlCheck())

    shiny::observeEvent(input$connectwebApi, {

      if(!is.null(input$baseUrl)){

        # check connection
        if(input$authorizeWebApi){
          response <- tryCatch({ROhdsiWebApi::authorizeWebApi(baseUrl = input$baseUrl,
                                                              authMethod = input$authMethod,
                                                              webApiUsername = input$webApiUsername,
                                                              webApiPassword = input$webApiPassword
                                                                )},
                               error = function(e){return('fail')})
          if(!is.null(response)){
            response <- NULL
          }else{
            response <- 'connect'
          }

        } else{
          url <- paste0(input$baseUrl, "/info")
          response <- tryCatch({httr::GET(url)},
                               error = function(e){return(NULL)})
        }


        if(!is.null(response)){
          baseUrlCheck('WebAPI: (Connected)')
          showNotification(paste("WebAPI connection works..."), duration = 0, type = 'message')
          webApi(input$baseUrl)
        } else{
          baseUrlCheck('WebAPI:')
          showNotification(paste("WebAPI input did not connect"), duration = 0, type = 'error')
          webApi('')
        }
      }else{
        showNotification(paste("Missing WebAPI"), duration = 0, type = 'error')
      }
    })

    return(webApi)
  }



