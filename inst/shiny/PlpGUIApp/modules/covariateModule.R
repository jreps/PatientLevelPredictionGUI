covariateViewer <- function(id, label = "Covariate") {
  ns <- shiny::NS(id)

  shinydashboard::box(title = "Covariates:", width = '60%',

                      # add table with existing models

                      shiny::dataTableOutput(ns('covariateTable')),

                      # add a covariate
                      shiny::textInput(inputId = ns('covariateName'),
                                       label = 'New Covariate Name:'),
                      shiny::actionButton(inputId = ns('addCovariate'),
                                          label = 'Add New Covariate')

  )
}

covariateServer <- #function(id) {
  #moduleServer(
  #id,
  function(input, output, session,
           cohortReactive) {

    # create the list of covariates
    covList <- shiny::reactiveVal(list())
    selectedRow <- shiny::reactiveVal(0)
    covIndex <- shiny::reactiveVal(0)

    shiny::observeEvent(input$addCovariate, {
      if(!is.null(input$covariateName) && input$covariateName!="" && !input$covariateName%in%names(covList())){
        oldList <- covList()
        i <- length(oldList)
        oldList[[i+1]] <- list()
        names(oldList)[i+1] <- input$covariateName
        covList(oldList)

        # update selector
        selectedRow(i+1)
        covIndex(1)

        # display editor
        shiny::showModal(covariateModuleViewer(session$ns))
      }
    })

    shiny::observeEvent(input$addNewCovariate, {
        oldList <- covList()
        i <- selectedRow()
        covInd <- length(oldList[[i]])
        oldList[[i]][[covInd+1]] <- list()
        covList(oldList)

        # update selector
        covIndex(covInd+1)

        # display editor
        shiny::showModal(covariateModuleSelector(session$ns))

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
    output$covariateTable <- shiny::renderDataTable({
      if(length(covList())>0){
        data.frame(Name = names(covList()),
                   #View = shinyInput(actionButton, length(names(covList())), 'button_', label = "View", onclick = sprintf("Shiny.onInputChange('%s', this.id, {priority: \"event\"})", session$ns("view_button"))  ),
                   Edit = shinyInput(actionButton, length(names(covList())), 'button_', label = "Edit", onclick = sprintf("Shiny.onInputChange('%s', this.id, {priority: \"event\"})", session$ns("select_button"))  ),
                   stringsAsFactors = FALSE,
                   row.names = 1:length(names(covList())),
                   Remove = shinyInput(actionButton, length(names(covList())), 'button_', label = "Remove", onclick = sprintf("Shiny.onInputChange('%s', this.id, {priority: \"event\"})", session$ns("delete_button"))  )

        )} else{
          NULL
        }
    }, escape = FALSE)

    #=================

    # ADD COV
    #=================
    # Show modal when button is clicked.
    observeEvent(input$select_button, {
      selectedRow(as.numeric(strsplit(input$select_button, "_")[[1]][2]))

      typeNames <- lapply(covList()[[as.numeric(strsplit(input$select_button, "_")[[1]][2])]], function(x) x$fnct)#add
      if(length(typeNames)==0){
        covariateList <- NULL
      } else{
        covariateList <- as.list(1:length(typeNames))
        names(covariateList) <- unlist(typeNames)
      }

      shiny::showModal(covariateModuleViewer(session$ns, covariateList))
    })

    # When button is pressed, update covariate
    # update the covariate with new settings
    shiny::observeEvent(input$selectCovariateType, {

      # open up the correct covariate module
      shiny::removeModal()

      # get settings
      indVal <-selectedRow()
      oldList <- covList()

      if(input$covariateType!='' && !is.null(input$covariateType)){
        shiny::showModal(do.call(paste0('covariateModule',input$covariateType),
                                 list(ns = session$ns, settings = oldList[[indVal]][[covIndex()]],
                                      cohortReactive = cohortReactive())))
      }
    })


    shiny::observeEvent(input$updateStandard, {
      indVal <-selectedRow()
      oldList <- covList()
      i <- covIndex()
      oldList[[indVal]][[i]] <- list(fnct = 'Standard',
                                     settings = list(useDemographicsGender = input$useDemographicsGender,
                                     useDemographicsAge = input$useDemographicsAge,
                                     useDemographicsAgeGroup = input$useDemographicsAgeGroup,
                                     useDemographicsRace = input$useDemographicsRace,
                                     useDemographicsEthnicity = input$useDemographicsEthnicity,
                                     useDemographicsIndexYear = input$useDemographicsIndexYear,
                                     useDemographicsIndexMonth = input$useDemographicsIndexMonth,
                                     useDemographicsPriorObservationTime = input$useDemographicsPriorObservationTime,
                                     useDemographicsPostObservationTime = input$useDemographicsPostObservationTime,
                                     useDemographicsTimeInCohort = input$useDemographicsTimeInCohort,
                                     useDemographicsIndexYearMonth = F,
                                     useConditionOccurrenceAnyTimePrior = FALSE,
                                     useConditionOccurrenceLongTerm = FALSE,
                                     useConditionOccurrenceMediumTerm = FALSE,
                                     useConditionOccurrenceShortTerm = FALSE,
                                     useConditionOccurrencePrimaryInpatientAnyTimePrior = FALSE,
                                     useConditionOccurrencePrimaryInpatientLongTerm = FALSE,
                                     useConditionOccurrencePrimaryInpatientMediumTerm = FALSE,
                                     useConditionOccurrencePrimaryInpatientShortTerm = FALSE,
                                     useConditionEraAnyTimePrior = FALSE,
                                     useConditionEraLongTerm = FALSE,
                                     useConditionEraMediumTerm = FALSE,
                                     useConditionEraShortTerm = FALSE,
                                     useConditionEraOverlapping = FALSE,
                                     useConditionEraStartLongTerm = FALSE,
                                     useConditionEraStartMediumTerm = FALSE,
                                     useConditionEraStartShortTerm = FALSE,
                                     useConditionGroupEraAnyTimePrior = FALSE,
                                     useConditionGroupEraLongTerm = FALSE,
                                     useConditionGroupEraMediumTerm = FALSE,
                                     useConditionGroupEraShortTerm = FALSE,
                                     useConditionGroupEraOverlapping = FALSE,
                                     useConditionGroupEraStartLongTerm = FALSE,
                                     useConditionGroupEraStartMediumTerm = FALSE,
                                     useConditionGroupEraStartShortTerm = FALSE,
                                     useDrugExposureAnyTimePrior = FALSE,
                                     useDrugExposureLongTerm = FALSE,
                                     useDrugExposureMediumTerm = FALSE,
                                     useDrugExposureShortTerm = FALSE,
                                     useDrugEraAnyTimePrior = FALSE,
                                     useDrugEraLongTerm = FALSE,
                                     useDrugEraMediumTerm = FALSE,
                                     useDrugEraShortTerm = FALSE,
                                     useDrugEraOverlapping = FALSE,
                                     useDrugEraStartLongTerm = FALSE,
                                     useDrugEraStartMediumTerm = FALSE,
                                     useDrugEraStartShortTerm = FALSE,
                                     useDrugGroupEraAnyTimePrior = FALSE,
                                     useDrugGroupEraLongTerm = FALSE,
                                     useDrugGroupEraMediumTerm = FALSE,
                                     useDrugGroupEraShortTerm = FALSE,
                                     useDrugGroupEraOverlapping = FALSE,
                                     useDrugGroupEraStartLongTerm = FALSE,
                                     useDrugGroupEraStartMediumTerm = FALSE,
                                     useDrugGroupEraStartShortTerm = FALSE,
                                     useProcedureOccurrenceAnyTimePrior = FALSE,
                                     useProcedureOccurrenceLongTerm = FALSE,
                                     useProcedureOccurrenceMediumTerm = FALSE,
                                     useProcedureOccurrenceShortTerm = FALSE,
                                     useDeviceExposureAnyTimePrior = FALSE,
                                     useDeviceExposureLongTerm = FALSE,
                                     useDeviceExposureMediumTerm = FALSE,
                                     useDeviceExposureShortTerm = FALSE,
                                     useMeasurementAnyTimePrior = FALSE,
                                     useMeasurementLongTerm = FALSE,
                                     useMeasurementMediumTerm = FALSE,
                                     useMeasurementShortTerm = FALSE,
                                     useMeasurementValueAnyTimePrior = FALSE,
                                     useMeasurementValueLongTerm = FALSE,
                                     useMeasurementValueMediumTerm = FALSE,
                                     useMeasurementValueShortTerm = FALSE,
                                     useMeasurementRangeGroupAnyTimePrior = FALSE,
                                     useMeasurementRangeGroupLongTerm = FALSE,
                                     useMeasurementRangeGroupMediumTerm = FALSE,
                                     useMeasurementRangeGroupShortTerm = FALSE,
                                     useObservationAnyTimePrior = FALSE,
                                     useObservationLongTerm = FALSE,
                                     useObservationMediumTerm = FALSE,
                                     useObservationShortTerm = FALSE,
                                     useCharlsonIndex = FALSE,
                                     useDcsi = FALSE,
                                     useChads2 = FALSE,
                                     useChads2Vasc = FALSE,
                                     useHfrs = FALSE,
                                     useDistinctConditionCountLongTerm = FALSE,
                                     useDistinctConditionCountMediumTerm = FALSE,
                                     useDistinctConditionCountShortTerm = FALSE,
                                     useDistinctIngredientCountLongTerm = FALSE,
                                     useDistinctIngredientCountMediumTerm = FALSE,
                                     useDistinctIngredientCountShortTerm = FALSE,
                                     useDistinctProcedureCountLongTerm = FALSE,
                                     useDistinctProcedureCountMediumTerm = FALSE,
                                     useDistinctProcedureCountShortTerm = FALSE,
                                     useDistinctMeasurementCountLongTerm = FALSE,
                                     useDistinctMeasurementCountMediumTerm = FALSE,
                                     useDistinctMeasurementCountShortTerm = FALSE,
                                     useDistinctObservationCountLongTerm = FALSE,
                                     useDistinctObservationCountMediumTerm = FALSE,
                                     useDistinctObservationCountShortTerm = FALSE,
                                     useVisitCountLongTerm = FALSE,
                                     useVisitCountMediumTerm = FALSE,
                                     useVisitCountShortTerm = FALSE,
                                     useVisitConceptCountLongTerm = FALSE,
                                     useVisitConceptCountMediumTerm = FALSE,
                                     useVisitConceptCountShortTerm = FALSE,
                                     longTermStartDays = input$longTermStartDays,
                                     mediumTermStartDays = input$mediumTermStartDays,
                                     shortTermStartDays = input$shortTermStartDays,
                                     endDays = input$endDays,
                                     includedCovariateConceptIds = input$includedCovariateConceptIds,
                                     addDescendantsToInclude = input$addDescendantsToInclude,
                                     excludedCovariateConceptIds = input$excludedCovariateConceptIds,
                                     addDescendantsToExclude = input$addDescendantsToExclude,
                                     includedCovariateIds = input$includedCovariateIds))
      covList(oldList)
      shiny::removeModal()
    })

    shiny::observeEvent(input$deleteStandard, {
      indVal <-selectedRow()
      oldList <- covList()
      i <- covIndex()
      if(i>1){
        oldList[[indVal]][[i]] <- NULL
        covIndex(i-1)
      } else{
        oldList[[indVal]] <- list()
        covIndex(0)
      }
      covList(oldList)
      shiny::removeModal()
    })

    shiny::observeEvent(input$updateCohort, {

      if(nrow(cohortReactive())>0){
        indVal <- selectedRow()
        oldList <- covList()
        i <- covIndex()
        oldList[[indVal]][[i]] <- list(fnct = 'Cohort', # createCohortCovariateSettings
                                       settings = list(cohortCovcovariateName =  cohortReactive()$name[cohortReactive()$id== input$cohortCovcohortId],
                                                       cohortCovcohortId = input$cohortCovcohortId,
                                                       covariateId = as.double(input$cohortCovcohortId)*1000+as.double(input$cohortCovanalysisId),
                                                       cohortCovstartDay= input$cohortCovstartDay,
                                                       cohortCovendDay= input$cohortCovendDay,
                                                       cohortCovcount=input$cohortCovcount,
                                                       cohortCovageInteraction = input$cohortCovageInteraction,
                                                       cohortCovlnAgeInteraction= input$cohortCovlnAgeInteraction,
                                                       cohortCovanalysisId = input$cohortCovanalysisId)
        )
        covList(oldList)
      } else{
        shiny::showNotification('Need to connect to a valid webApi to fetch the cohorts', duration = 5, type = 'error')
      }
      shiny::removeModal()
    })

    shiny::observeEvent(input$deleteCohort, {
      oldList <- covList()
      indVal <- selectedRow()
      i <- covIndex()
      if(i>1){
        oldList[[indVal]][[i]] <- NULL
        covIndex(i-1)
      } else{
        oldList[[indVal]] <- list()
        covIndex(0)
      }
      covList(oldList)
      shiny::removeModal()
    })


    # update the settings
    shiny::observeEvent(input$updateCovariate, {
      shiny::removeModal()

      indVal <-selectedRow()
      oldList <- covList()
      i <- as.double(input$covariateId)
      covIndex(i)
      result <- oldList[[indVal]][[i]]
      type <- result$fnct

      if(type !='' && !is.null(type)){
        shiny::showModal(do.call(paste0('covariateModule',type),
                                 list(ns = session$ns, settings = result,
                                      cohortReactive = cohortReactive())))
      }

    })

    #=================

    # DELETE full COV
    #=================
    # if clicked popup asks for confirmation and then deletes covariate setting
    shiny::observeEvent(input$delete_button, {
      selectedRow(as.numeric(strsplit(input$delete_button, "_")[[1]][2]))
      shiny::showModal(covariateModalDelete(session$ns))
    })
    shiny::observeEvent(input$deleteCovariateSetting, {
      indVal <- selectedRow()
      oldList <- covList()
      oldList[[indVal]] <- NULL
      covList(oldList)
      shiny::removeModal()
    })
    #=================


    return(covList)
  }


covariateModuleViewer <- function(ns,covariateList = NULL) {
  shiny::modalDialog(

    # select the type of covariate to add to the covarite Name
    shiny::h3('Select Existing Covariate Setting:'),
    shiny::selectInput(inputId = ns('covariateId'),
                       label = 'Covariate Type:',
                       choices = covariateList),
    shiny::actionButton(ns('updateCovariate'), 'Edit Selected Covariate'),
    shiny::h3('Or Add New Covariate Setting:'),
    shiny::actionButton(ns('addNewCovariate'), 'Add New Covariate Setting')

  )
}

covariateModuleSelector <- function(ns, types = c('Standard', 'Cohort', 'Measurement', 'Age')) {
  shiny::modalDialog(

    # select the type of covariate to add to the covarite Name
    shiny::selectInput(inputId = ns('covariateType'), label = 'Covariate Type:',
                       choices = types
    ),

    footer = shiny::tagList(
      #modalButton("Cancel"),
      shiny::actionButton(ns('selectCovariateType'), 'Select Covariate')
    )
  )
}


covariateModuleStandard <- function(ns, settings, ...) { # settings is list(fnct, settings)
  shiny::modalDialog(

    shiny::fluidRow(
      shiny::column(width = 2,
             shiny::numericInput(ns("endDays"), "endDays:", value = 0 )
      ),
      shiny::column(width = 2,
             shiny::numericInput(ns("longTermStartDays"), "long Term Start Days:", value = -365 )
      ),
      shiny::column(width = 3,
             shiny::numericInput(ns("mediumTermStartDays"), "medium Term Start Days:", value = -180 )
      ),
      shiny::column(width = 2,
             shiny::numericInput(ns("shortTermStartDays"), "short Term Start Days:", value = -30 )
      )
    ),

    shiny::fluidRow(
      shiny::column(width = 2,
             shiny::checkboxInput(ns("useDemographicsGender"), "Use Gender:", value = F )
      ),
      shiny::column(width = 2,
             shiny::checkboxInput(ns("useDemographicsAge"), "Use Age (year):", value = F )
      ),
      shiny::column(width = 2,
             shiny::checkboxInput(ns("useDemographicsAgeGroup"), "Use Age (5-year bins):", value = F )
      ),
      shiny::column(width = 2,
             shiny::checkboxInput(ns("useDemographicsRace"), "Use Race:", value = F )
      ),
      shiny::column(width = 2,
             shiny::checkboxInput(ns("useDemographicsEthnicity"), "Use Ethnicity:", value = F )
      )
    ),
    shiny::fluidRow(
      shiny::column(width = 2,
             shiny::checkboxInput(ns("useDemographicsIndexYear"), "Use Index Year:", value = F )
      ),
      shiny::column(width = 2,
             shiny::checkboxInput(ns("useDemographicsIndexMonth"), "Use Index Month:", value = F )
      ),
      shiny::column(width = 2,
             shiny::checkboxInput(ns("useDemographicsPriorObservationTime"), "Use total observed time before index:", value = F )
      ),
      shiny::column(width = 2,
             shiny::checkboxInput(ns("useDemographicsPostObservationTime"), "Use total observed time after index:", value = F )
      ),
      shiny::column(width = 2,
             shiny::checkboxInput(ns("useDemographicsTimeInCohort"), "Use time in cohort:", value = F )
      )
    ),

    shiny::fluidRow(
      shiny::column(width = 6,
             shiny::textInput(ns("includedCovariateConceptIds"), "Included Covariate Concept Ids:", value = '' )
      ),
      shiny::column(width = 3,
             shiny::checkboxInput(ns("addDescendantsToInclude"), "Add Descendants To Included :", value = F )
      )),
    shiny::fluidRow(
      shiny::column(width = 6,
             shiny::textInput(ns("excludedCovariateConceptIds"), "Excluded Covariate Concept Ids:", value = '' )
      ),
      shiny::column(width = 3,
             shiny::checkboxInput(ns("addDescendantsToExclude"), "Add Descendants To Exclude:", value = F )
      )
    ),
    shiny::fluidRow(
      shiny::column(width = 9,
             shiny::textInput(ns("includedCovariateIds"), "Included Covariate Ids:", value = '' )
      )
    ),

    footer = shiny::tagList(
      shiny::actionButton(ns('deleteStandard'), 'Delete'),
      shiny::actionButton(ns('updateStandard'), 'Update Standard')
    )
  )
}

covariateModuleCohort <- function(ns, settings, cohortReactive, ...) { # settings is list(fnct, settings)

  if(nrow(cohortReactive)>0){
    choices <- as.list(cohortReactive$id)
    names(choices ) <- cohortReactive$name
  } else {
    choices <- NULL
  }
  selectedId <- ifelse(is.null(settings$settings$cohortCovcohortId), 1, settings$settings$cohortCovcohortId)

  shiny::modalDialog(
    shiny::selectInput(inputId = ns('cohortCovcohortId'), label = 'Select covariate cohort: ',
                       choices = choices, selected = selectedId),
    #shiny::textInput(inputId = ns('cohortCovName'), label = 'Covariate name:'),
    shiny::numericInput(inputId = ns('cohortCovanalysisId'), label = 'analysisId (between 400 and 500): ', min=400, max = 500,
                        value = ifelse(is.null(settings$settings$cohortCovanalysisId), 456, settings$settings$cohortCovanalysisId)),
    shiny::numericInput(inputId = ns('cohortCovstartDay'), label = 'startDay: ',
                        value = ifelse(is.null(settings$settings$cohortCovstartDay), -9999, settings$settings$cohortCovstartDay)),
    shiny::numericInput(inputId = ns('cohortCovendDay'), label = 'endDay: ',
                        value = ifelse(is.null(settings$settings$cohortCovendDay), 0, settings$settings$cohortCovendDay)),
    shiny::checkboxInput(inputId = ns('cohortCovcount'), label = 'count:', value = ifelse(is.null(settings$settings$cohortCovcount),FALSE,settings$settings$cohortCovcount)),
    shiny::checkboxInput(inputId = ns('cohortCovageInteraction'), label = 'ageInteraction:', value = ifelse(is.null(settings$settings$cohortCovageInteraction),FALSE,settings$settings$cohortCovageInteraction)),
    shiny::checkboxInput(inputId = ns('cohortCovlnAgeInteraction'), label = 'lnAgeInteraction:', value = ifelse(is.null(settings$settings$cohortCovlnAgeInteraction),FALSE,settings$settings$cohortCovlnAgeInteraction)),

    footer = shiny::tagList(
      shiny::actionButton(ns('deleteCohort'), 'Delete'),
      shiny::actionButton(ns('updateCohort'), 'Update Cohort')
    )
  )
}

covariateModalDelete <- function(ns) {
  shiny::modalDialog(

    # select the type of covariate to add to the covarite Name
    shiny::h1('Delete Covariate?'),

    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns('deleteCovariateSetting'), 'Yes')
    )
  )
}
