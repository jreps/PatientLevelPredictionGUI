appdir <- file.path("/Users/jreps/Documents/PatientLevelPredictionGUI/inst","shiny", "PlpGUIApp")
source(file.path(appdir,"helpers","HelperCheckPackageDep.R"))

installViewer <- function(id, label = "Package") {
  ns <- shiny::NS(id)
  shiny::uiOutput(ns("placeholder"))
}

installServer <- function(input, output, session, package) {

  info <- shiny::reactiveVal(installInfo(package = package))

  output$placeholder = shiny::renderUI({
    shinydashboard::box(title = package,
                        status = getStatus(info()$main),
                        solidHeader = T,
                        shiny::textOutput(session$ns('availableVersion')),
                        shiny::textOutput(session$ns('installedVersion')),
                        shiny::actionButton(inputId = session$ns('install_button'), label = 'Install'),
                        shiny::actionButton(inputId = session$ns('View'), label = 'View Details')
        )
  })

  output$availableVersion <- shiny::renderText(paste('Latest Version:', info()$main$versionAvailable))
  output$installedVersion <- shiny::renderText(paste('Installed Version:', info()$main$versionInsalled))

  output$installDeps <- shiny::renderDataTable(info()$dependencies)


  # install/update
  shiny::observeEvent(input$install_button, {
    shiny::showModal(installPackageModal(session$ns, package))
  })

  shiny::observeEvent(input$install, {
    # check devtools and install if needed
    install.packages(setdiff('devtools', rownames(installed.packages())))

    # install package
    shiny::showNotification(paste0('Starting to install ',package, ' - modal will close when package install is complete'), duration = 5)
    devtools::install_github(repo = paste0('ohdsi/',package), upgrade = "never")

    status <- ifelse(length(tryCatch({as.character(packageVersion(package))},error = function(e){return(NULL)}))==1, 'success','warning')

    if(status == 'success'){
      shiny::showNotification(paste0('Installed ',package), duration = 5)
    }else{
      shiny::showNotification(paste0('Installation Issue with ',package), duration = 5)
    }

    # update the info
    info(installInfo(package = package))

    shiny::removeModal()

  })


  # launch modal to view details:

  # install/update
  shiny::observeEvent(input$View, {
    shiny::showModal(viewPackageModal(session$ns, package))
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


viewPackageModal <- function(ns, pkg) {
  shiny::modalDialog(

    shinydashboard::box(title = paste0(pkg,' Dependencies:'), width = '60%',
                        shiny::dataTableOutput(ns('installDeps'))
    ),

    footer = shiny::tagList(
      shiny::modalButton("Cancel")
    )
  )
}


getStatus <- function(x){
  if(x$versionInsalled == 'Not installed'){
    return('danger')
  } else if(x$versionInsalled == x$versionAvailable){
    return('success')
  } else{
    return('warning')
  }
}
