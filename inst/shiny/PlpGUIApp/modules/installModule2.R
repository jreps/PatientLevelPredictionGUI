installViewer <- function(id, label = "Package") {
  ns <- shiny::NS(id)

  shinydashboard::box(title = paste0('R configuration:'), width = '60%',
                      shiny::uiOutput(ns('packageSelect')),
                      shiny::h3('Install status:'),
                      shiny::dataTableOutput(ns('installMain')),
                      shiny::h3('Dependencies:'),
                      shiny::dataTableOutput(ns('installDeps'))
  )
}

installServer <- function(input, output, session, package) {

  output$packageSelect <- renderUI({
    shiny::selectInput(inputId = session$ns('package'), label = 'Package:',
                       choices = package, selected = 1)
  })

  installMain <- shiny::reactiveVal(data.frame())
  installDeps <- shiny::reactiveVal(data.frame())
  packageOfInt <- shiny::reactiveVal(package[1])

  shiny::observeEvent(input$package, {
    info <- installInfo(package = input$package)
    installMain(info$main)
    installDeps(info$dependencies)

    output$installMain  <- shiny::renderDataTable({
      if(nrow(installMain())>0){

        result <- installMain()
        result <- data.frame(package = result$package,
                             versionAvailable = result$versionAvailable,
                             versionInsalled = result$versionInsalled,
                             Edit = as.character(shiny::actionButton(inputId = paste0('button_',input$package), label = "Install/Update", onclick = sprintf("Shiny.onInputChange('%s', this.id, {priority: \"event\"})", session$ns("install_button"))  )),
                             stringsAsFactors = FALSE,
                             row.names = 1 )

      } else{
        NULL
      }
    }, escape = FALSE)

    if( nrow(installDeps()) >0){
      output$installDeps <- shiny::renderDataTable(installDeps())
    }

    #install.packages('ResourceSelection')
  }
  )


  # install/update
  shiny::observeEvent(input$install_button, {
    pkg <- strsplit(input$install_button, "_")[[1]][2]
    packageOfInt(pkg)
    shiny::showModal(installPackageModal(session$ns, pkg))
  })


  shiny::observeEvent(input$install, {
    # check devtools and install if needed
    install.packages(setdiff('devtools', rownames(installed.packages())))

    # install package
    shiny::showNotification(paste0('Starting to install ',packageOfInt(), ' - modal will close when package install is complete'), duration = 5)
    devtools::install_github(repo = paste0('ohdsi/',packageOfInt()), upgrade = "never")
    shiny::showNotification(paste0('Installed ',packageOfInt()), duration = 5)

    # update the info
    info <- installInfo(package = packageOfInt())
    installMain(info$main)
    installDeps(info$dependencies)

    shiny::removeModal()

  })

}



installPackageModal <- function(ns, pkg) {
  shiny::modalDialog(

    # select the type of covariate to add to the covarite Name
    shiny::h1(paste0('Do you want to install/update package: ',pkg, '?')),

    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns('install'), 'Install/Update')
    )
  )
}
