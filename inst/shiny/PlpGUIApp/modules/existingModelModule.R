existingModelViewer <- function(id, label = "Model") {
  ns <- shiny::NS(id)

  shinydashboard::box(title = "Existing Model:", width = '60%',

                      # add table with existing models (model id with link to view, type, number variables and remove option)
                      shiny::dataTableOutput(ns('modelTable')),

                      # add a model
                      shiny::actionButton(inputId = ns('addModel'),
                                          label = 'Add Model')

  )
}

existingModelServer <- #function(id) {
  #moduleServer(
  #id,
  function(input, output, session,
           cohortReactive, webApi) {

    modelList <- shiny::reactiveVal(list()) # list of all models
    selectedRow <- shiny::reactiveVal()  # ind of model
    predictorRow <- shiny::reactiveVal()  # ind of predictor


    # MODEL STUFF

    output$modelTable <- shiny::renderDataTable({
      if(length(modelList())>0){
        data.frame(Model = unlist(lapply(modelList(), function(x) x$name)),
                   Type = unlist(lapply(modelList(), function(x) x$attr_predictionType)),
                   cohortId = unlist(lapply(modelList(), function(x) x$cohortId)),
                   outcomeId = unlist(lapply(modelList(), function(x) x$outcomeId)),
                   Predictors = unlist(lapply(modelList(), function(x) length(x$model$coefficients))),
                   Edit= shinyInput(shiny::actionButton, length(modelList()), 'pbutton_', label = "Edit", onclick = sprintf("Shiny.onInputChange('%s', this.id, {priority: \"event\"})", session$ns("select_button"))  ),
                   stringsAsFactors = FALSE,
                   row.names = 1:length(modelList()),
                   Remove = shinyInput(shiny::actionButton, length(modelList()), 'pbutton_', label = "Remove", onclick = sprintf("Shiny.onInputChange('%s', this.id, {priority: \"event\"})", session$ns("delete_button"))  )

        )} else{
          NULL
        }
    }, escape = FALSE)

    # this adds a new model
    #@@@@@@@@@@@@@@@@@@@@
    shiny::observeEvent(input$addModel, {
      # add to modelList
      # add a new model to the list
      oldList <- modelList()
      i <- length(oldList)
      oldList[[i+1]] <- list(modelId = i+1,
                             name = paste0('model ', i+1),
                             cohortId = 0,
                             cohort_json = NULL,
                             outcomeId = 0,
                             outcome_json = NULL,
                             populationSettings = PatientLevelPrediction::createStudyPopulationSettings(),
                             covariateSettings = list(),
                             new = TRUE,
                             attr_predictionType = 'binary',
                             attr_type = 'nonPlpGlm',
                             model = list(
                               coefficients = list(),
                               finalMapping = 'function(x){return(1/(1+exp(-x)))}',
                               offset = NULL,
                               baselineHazard = NULL))
      modelList(oldList)

      # set the row selector
      selectedRow(i+1)
      # now show editor
      shiny::showModal(viewModelModal(ns = session$ns, model =
                                        modelList()[[selectedRow()]],
                                      cohorts = cohortReactive))
    })


    # this update the model settings
    #@@@@@@@@@@@@@@@@@@@@
    shiny::observeEvent(input$updateModel, {
      oldList <- modelList()

      model <- oldList[[selectedRow()]]$model
      model$finalMapping <- input$finalMapping
      model$offset <- input$offset
      model$baselineHazard <- input$baselineHazard

      oldList[[selectedRow()]] <- list(modelId = selectedRow(),
                                       name = input$name,
                             new = FALSE,
                             attr_predictionType = input$attr_predictionType,
                             attr_type = input$attr_type,
                             cohortId = input$cohortId,
                             cohort_json = ROhdsiWebApi::getCohortDefinition(as.numeric(input$cohortId), webApi()),
                             outcomeId = input$outcomeId,
                             outcome_json = ROhdsiWebApi::getCohortDefinition(as.numeric(input$outcomeId), webApi()),
                             populationSettings = oldList[[selectedRow()]]$populationSettings, # add
                             covariateSettings = list(),
                             model = model)
      modelList(oldList)

      # now show editor
      shiny::removeModal()
    })

    shiny::observeEvent(input$deleteModel, {
      indVal <- selectedRow()
      oldList <- modelList()
      oldList[[indVal]] <- NULL
      modelList(oldList)
      shiny::removeModal()
    })

    # edit existing model setting
    shiny::observeEvent(input$select_button, {
      selectedRow(as.numeric(strsplit(input$select_button, "_")[[1]][2]))

      # now show editor
      shiny::showModal(viewModelModal(ns = session$ns,
                                      model = modelList()[[selectedRow()]],
                                      cohorts = cohortReactive))
    })


    # if clicked popup asks for confirmation and then deletes covariate setting
    shiny::observeEvent(input$delete_button, {
      selectedRow(as.numeric(strsplit(input$delete_button, "_")[[1]][2]))
      shiny::showModal(modelModalDelete(session$ns))
    })

    #=================




    # COVARIATES STUFF

    # display the covariates
    output$predictorTable <- shiny::renderDataTable({
      if(length(modelList()[[selectedRow()]]$model$coefficients)!=0){

        data.frame(name = unlist(lapply(modelList()[[selectedRow()]]$model$coefficients, function(x) x$covariateName)),
                   covariateId = unlist(lapply(modelList()[[selectedRow()]]$model$coefficients, function(x) x$covariateId)),
                   points = unlist(lapply(modelList()[[selectedRow()]]$model$coefficients, function(x) x$points)),
                   offset = unlist(lapply(modelList()[[selectedRow()]]$model$coefficients, function(x) x$offset)),
                   power = unlist(lapply(modelList()[[selectedRow()]]$model$coefficients, function(x) x$power)),
                   Edit= shinyInput(shiny::actionButton, length(modelList()[[selectedRow()]]$model$coefficients), 'pbutton_', label = "Edit", onclick = sprintf("Shiny.onInputChange('%s', this.id, {priority: \"event\"})", session$ns("select_predictor"))  ),
                   stringsAsFactors = FALSE,
                   row.names = 1:length(modelList()[[selectedRow()]]$model$coefficients),
                   Remove = shinyInput(shiny::actionButton, length(modelList()[[selectedRow()]]$model$coefficients), 'pbutton_', label = "Remove", onclick = sprintf("Shiny.onInputChange('%s', this.id, {priority: \"event\"})", session$ns("delete_predictor"))  )

        )} else{
          NULL
        }
    }, escape = FALSE)

    # edit existing model setting
    shiny::observeEvent(input$select_predictor, {

      predId <- as.numeric(strsplit(input$select_predictor, "_")[[1]][2])
      predictorRow(predId)

      # now show editor
      shiny::showModal(viewPredictorModal(ns = session$ns,
                                          cohortReactive,
                                          predictor = modelList()[[selectedRow()]]$model$coefficients[[predictorRow()]]))
    })


    # DELETE
    #=================
    # if clicked popup asks for confirmation and then deletes covariate setting
    shiny::observeEvent(input$delete_predictor, {

      predId <- as.numeric(strsplit(input$delete_predictor, "_")[[1]][2])
      predictorRow(predId)

      shiny::showModal(predictorModalDelete(session$ns))
    })

   # code to view predictors in selected model + remove them

    # add predictors
    shiny::observeEvent(input$addPredictor, {

      # update the current values in the model settings then open a new predictor module
      oldList <- modelList()

      model <- oldList[[selectedRow()]]$model
      model$finalMapping <- input$finalMapping
      model$offset <- input$offset
      model$baselineHazard <- input$baselineHazard
      oldList[[selectedRow()]] <- list(modelId = selectedRow(),
                                       name = input$name,
                                       new = FALSE,
                                       attr_predictionType = input$attr_predictionType,
                                       attr_type = input$attr_type,
                                       cohortId = input$cohortId,
                                       cohort_json = ROhdsiWebApi::getCohortDefinition(as.numeric(input$cohortId), webApi()),
                                       outcomeId = input$outcomeId,
                                       outcome_json = ROhdsiWebApi::getCohortDefinition(as.numeric(input$outcomeId), webApi()),
                                       populationSettings = oldList[[selectedRow()]]$populationSettings, # add
                                       covariateSettings = list(),
                                       model = model)

      # add new predictor
      i <- length(oldList[[selectedRow()]]$model$coefficients) + 1
      predictorRow(i)
      oldList[[selectedRow()]]$model$coefficients[[predictorRow()]] <- newPredictor()
      modelList(oldList)
      shiny::showModal(viewPredictorModal(ns = session$ns,
                                          cohortReactive,
                                          predictor = modelList()[[selectedRow()]]$model$coefficients[[predictorRow()]]))
    })

    shiny::observeEvent(input$updatePredictor, {
      # code to add the predcitor to the current model
      newCoeff <- list(covariateId = as.double(as.character(input$atlasId))*1000+input$analysisId,
                       covariateName = input$covariateName,
                       cohortId = input$atlasId,
                       cohortjson = as.character(getCohort(as.double(as.character(input$atlasId)), webApi())),
                   points = input$cohortPoints,
                   offset = input$cohortOffset,
                   power = input$cohortPower,
                   analysisId = input$analysisId,
                   startDay = input$startDay,
                   endDay = input$endDay,
                   count = input$count,
                   ageInteraction = input$ageInteraction,
                   lnAgeInteraction = input$lnAgeInteraction)

      # update reactive var
      mdl <- modelList()
      mdl[[selectedRow()]]$model$coefficients[[predictorRow()]] <- newCoeff
      modelList(mdl)

      ##shiny::removeModal()
      # now show editor
      shiny::showModal(viewModelModal(ns = session$ns,
                                      model = modelList()[[selectedRow()]],
                                      cohorts = cohortReactive))

    })

    shiny::observeEvent(input$deletePredictor, {
      oldList <- modelList()
      oldList[[selectedRow()]]$model$coefficients[[predictorRow()]] <- NULL
      modelList(oldList)
      shiny::removeModal()
    })




    # update the pop settings in the model
    shiny::observeEvent(input$populationSettings, {

      # save current settings in model:
      oldList <- modelList()
      model <- oldList[[selectedRow()]]$model
      model$finalMapping <- input$finalMapping
      model$offset <- input$offset
      model$baselineHazard <- input$baselineHazard
      oldList[[selectedRow()]] <- list(modelId = selectedRow(),
                                       name = input$name,
                                       new = FALSE,
                                       attr_predictionType = input$attr_predictionType,
                                       attr_type = input$attr_type,
                                       cohortId = input$cohortId,
                                       cohort_json = ROhdsiWebApi::getCohortDefinition(as.numeric(input$cohortId), webApi()),
                                       outcomeId = input$outcomeId,
                                       outcome_json = ROhdsiWebApi::getCohortDefinition(as.numeric(input$outcomeId), webApi()),
                                       populationSettings = oldList[[selectedRow()]]$populationSettings, # add
                                       covariateSettings = list(),
                                       model = model)
      modelList(oldList)

      # open pop module
      shiny::showModal(viewPopulationModal(ns = session$ns,
                                           populationSettings = modelList()[[selectedRow()]]$populationSettings
                                      )
                       )

    })

    shiny::observeEvent(input$updatePopulationSettings, {
      oldList <- modelList()
      populationSettings = list(binary = oldList[[selectedRow()]]$attr_type,
                                riskWindowStart = input$riskWindowStart,
                                startAnchor = input$startAnchor,
                                riskWindowEnd = input$riskWindowEnd,
                                endAnchor = input$endAnchor,
                                requireTimeAtRisk = input$requireTimeAtRisk,
                                minTimeAtRisk = input$minTimeAtRisk,
                                removeSubjectsWithPriorOutcome = input$removeSubjectsWithPriorOutcome,
                                priorOutcomeLookback = input$priorOutcomeLookback,
                                firstExposureOnly = input$firstExposureOnly,
                                washoutPeriod = input$washoutPeriod
                                )
      oldList[[selectedRow()]]$populationSettings <- populationSettings

      modelList(oldList)

      shiny::showModal(viewModelModal(ns = session$ns,
                                      model = modelList()[[selectedRow()]],
                                      cohorts = cohortReactive))
    })


    return(modelList)
  }


# START MODALS
# =========================

viewModelModal<- function(ns, model, cohorts) {
  choices <- cohorts()$id
  names(choices) <- paste0(cohorts()$id, ':', cohorts()$name)

  options <- shinyWidgets::pickerOptions(liveSearch = T, showTick = T)

  modalDialog(

    shiny::textInput(inputId = ns('name'),
                     label = 'Model name:',
                     value = ifelse(!is.null(model$name), model$name, 'model name')),

    shinyWidgets::pickerInput(inputId = ns('cohortId'),
                              label = 'Select target cohort: ',
                              choices = choices,
                              selected = model$cohortId,
                              multiple = F,
                              options = options),

    shinyWidgets::pickerInput(inputId = ns('outcomeId'),
                              label = 'Select outcome: ',
                              choices = choices,
                              selected = model$outcomeId,
                              multiple = F,
                              options = options),

    # button to select pop
    shiny::actionButton(inputId = ns('populationSettings'), label = 'Edit Population Settings'),

    shiny::selectInput(inputId = ns('attr_predictionType'),
                       label = 'Model type:',
                       choices = list(binary = 'binary',
                                      survival = 'survival'),
                       selected = ifelse(!is.null(model$attr_predictionType), model$attr_predictionType, 'binary')
                       ),

    shiny::selectInput(inputId = ns('attr_type'),
                       label = 'PLP function:',
                       choices = list(nonPlpGlm = 'nonPlpGlm'),
                       selected = ifelse(!is.null(model$attr_type), model$attr_type, 'nonPlpGlm')
    ),

    shiny::textInput(inputId = ns('finalMapping'),
                     label = 'Final Mapping:',
                     value = ifelse(!is.null(model$model$finalMapping), model$model$finalMapping, 'f(x){return(1/(1+exp(-x)))}'),
                     placeholder = ifelse(!is.null(model$model$finalMapping), model$model$finalMapping, 'f(x){return(1/(1+exp(-x)))}')
                      ),

    shiny::numericInput(inputId = ns('offset'),
                        label = 'Offset:',
                        value = ifelse(!is.null(model$model$offset),model$model$offset, 0),
                        min = -10000000,
                        max = 10000000,
                        step = 0.001),
    shiny::numericInput(inputId = ns('baselineHazard'),
                        label = 'Baseline Hazard:',
                        value = ifelse(!is.null(model$model$baselineHazard),model$model$baselineHazard,0.1),
                        min = 0,
                        max = 1,
                        step = 0.001),

   shiny::dataTableOutput(ns('predictorTable')),
   shiny::actionButton(ns('addPredictor'), 'Add Predictor'),

    footer = shiny::tagList(
      shiny::actionButton(ns('updateModel'), ifelse(model$new, 'Add existing model', 'Update existing model'))
    )

  )}


viewPredictorModal<- function(ns, cohorts, predictor) {

  choices <- cohorts()$id
  names(choices) <- paste0(cohorts()$id, ':', cohorts()$name)

  options <- shinyWidgets::pickerOptions(liveSearch = T, showTick = T)

  modalDialog(
    # select type
    #shiny::selectInput(inputId = ns('predictorType'),
    #                   label = 'Type',
    #                   choices = list(standard = 'standard', cohort = 'cohort'),
    #                   selected = 'standard')

    shiny::textInput(inputId = ns('covariateName'),
                     label = 'Name: ', value = ifelse(is.null(predictor$covariateName), '', predictor$covariateName)),

    shinyWidgets::pickerInput(inputId = ns('atlasId'),
                              label = 'Select covariate cohort: ',
                              choices = choices,
                              selected = predictor$cohortId,
                              multiple = F,
                              options = options),

    shiny::numericInput(inputId = ns('analysisId'), label = 'analysisId (between 400 and 500): ', min=400, max = 500,
                        value = ifelse(is.null(predictor$analysisId), 457, predictor$analysisId)),
    shiny::numericInput(inputId = ns('startDay'), label = 'startDay: ',
                        value = ifelse(is.null(predictor$startDay), -9999, predictor$startDay)),
    shiny::numericInput(inputId = ns('endDay'), label = 'endDay: ',
                        value = ifelse(is.null(predictor$endDay), 0,  predictor$endDay)),
    shiny::checkboxInput(inputId = ns('count'), label = 'count:', value = ifelse(is.null(predictor$count),FALSE,predictor$count)),
    shiny::checkboxInput(inputId = ns('ageInteraction'), label = 'ageInteraction:', value = ifelse(is.null(predictor$ageInteraction),FALSE,predictor$ageInteraction)),
    shiny::checkboxInput(inputId = ns('lnAgeInteraction'), label = 'lnAgeInteraction:', value = ifelse(is.null(predictor$lnAgeInteraction),FALSE,predictor$lnAgeInteraction)),

    # add points/offset/power
    shiny::numericInput(inputId = ns('cohortPoints'), label = 'Points: ',
                        value = ifelse(is.null(predictor$points), 0, predictor$points)),

   shiny::numericInput(inputId = ns('cohortOffset'), label = 'Offset: ',
                       value = ifelse(is.null(predictor$offset), 0, predictor$offset)),

   shiny::numericInput(inputId = ns('cohortPower'), label = 'Power: ',
                       value = ifelse(is.null(predictor$power), 0, predictor$power)),

    footer = shiny::tagList(
      shiny::actionButton(ns('updatePredictor'), 'Update')
    )

  )}

modelModalDelete <- function(ns) {
  shiny::modalDialog(

    # select the type of covariate to add to the covarite Name
    shiny::h2('Delete Model?'),

    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns('deleteModel'), 'Yes')
    )
  )
}

predictorModalDelete <- function(ns) {
  shiny::modalDialog(

    shiny::h2('Delete Predictor?'),

    footer = shiny::tagList(
      shiny::modalButton("Cancel"),
      shiny::actionButton(ns('deletePredictor'), 'Yes')
    )
  )
}


viewPopulationModal<- function(ns, populationSettings) {

  modalDialog(

    shiny::numericInput(inputId = ns('riskWindowStart'),
                     label = 'riskWindowStart:',
                     value = ifelse(!is.null(populationSettings$riskWindowStart), populationSettings$riskWindowStart, 1)),
    shiny::selectInput(inputId = ns('startAnchor') , label = 'startAnchor', choices = list('cohort start','cohort end'), selected = populationSettings$startAnchor),

    shiny::numericInput(inputId = ns('riskWindowEnd'),
                        label = 'riskWindowEnd:',
                        value = ifelse(!is.null(populationSettings$riskWindowEnd), populationSettings$riskWindowEnd, 365)),
    shiny::selectInput(inputId = ns('endAnchor') , label = 'endAnchor', choices = list('cohort start','cohort end'), selected = populationSettings$endAnchor),

    shiny::checkboxInput(inputId = ns('requireTimeAtRisk'), label = 'requireTimeAtRisk', value = populationSettings$requireTimeAtRisk),
    shiny::numericInput(inputId = ns('minTimeAtRisk'),
                        label = 'minTimeAtRisk:',
                        value = ifelse(!is.null(populationSettings$minTimeAtRisk), populationSettings$minTimeAtRisk, 0)),
    shiny::checkboxInput(inputId = ns('removeSubjectsWithPriorOutcome'), label = 'removeSubjectsWithPriorOutcome', value = populationSettings$removeSubjectsWithPriorOutcome),
    shiny::numericInput(inputId = ns('priorOutcomeLookback'),
                        label = 'priorOutcomeLookback:',
                        value = ifelse(!is.null(populationSettings$priorOutcomeLookback), populationSettings$priorOutcomeLookback, -9999)),

    shiny::checkboxInput(inputId = ns('firstExposureOnly'), label = 'firstExposureOnly', value = populationSettings$firstExposureOnly),
    shiny::numericInput(inputId = ns('washoutPeriod'),
                        label = 'washoutPeriod:',
                        value = ifelse(!is.null(populationSettings$washoutPeriod), populationSettings$washoutPeriod, -9999)),


    footer = shiny::tagList(
      shiny::actionButton(ns('updatePopulationSettings'),  'Update')
    )

  )}

# END MODALS
# =========================

# helpers

# create function for edit/delete buttoms in table:
shinyInput <- function(FUN, len, id, ...) {
  inputs <- character(len)
  for (i in seq_len(len)) {
    inputs[i] <- as.character(FUN(paste0(id, i), ...))
  }
  inputs
}

getCohort <- function(atlasId, webApi){
  cohort <- ROhdsiWebApi::getCohortDefinition(atlasId, webApi)
  return(cohort)
}


newPredictor <- function(covariateId = 0,
                        covariateName = '',
                        cohortId = 0,
                        cohortjson = '',
                        points = 0,
                        offset = 0,
                        power = 1,
                        analysisId = 467,
                        startDay = -9999,
                        endDay = 0,
                        count = F,
                        ageInteraction = F,
                        lnAgeInteraction = F){

  list(covariateId = covariateId,
       covariateName = covariateName,
       cohortId = cohortId,
       cohortjson = cohortjson,
       points = points,
       offset = offset,
       power = power,
       analysisId = analysisId,
       startDay = startDay,
       endDay = endDay,
       count = count,
       ageInteraction = ageInteraction,
       lnAgeInteraction = lnAgeInteraction)
}
