formatSettings <- function(analysisList,
                                    targetList,
                                    outcomeList,
                                    modelList,
                                    covList,
                                    popList,
                                    OPList,
                                    MCList,
                                    TCList,
                                    executeList){


  populationSettings <- formatPopList(popList)

  modelList <- formatModelList(modelList)

  covariateSettings <- formatCovariateList(covList)

  resrictOutcomePops <- NULL
  if(length(OPList)>0){
    resrictOutcomePops <- OPList
  }

  resrictModelCovs <- NULL
  if(length(MCList)>0){
    resrictModelCovs  <- MCList
  }

  resrictTargetCovs <- NULL
  if(length(TCList)>0){
    resrictTargetCovs  <- TCList
  }


  executionSettings <- list(minCovariateFraction = executeList$minCovariateFraction,
                            normalizeData = executeList$normalizeData,
                            testSplit = executeList$testSplit,
                            testFraction = executeList$testFraction,
                            splitSeed = executeList$splitSeed,
                            nfold = executeList$nfold)

  createDevelopmentStudyJsonList <- list(packageName = analysisList$packageName,
                                     packageDescription = analysisList$packageDescription,
                                     createdBy = analysisList$createBy,
                                     organizationName = analysisList$organizationName,
                                     targets = data.frame(targetId = targetList$Id,
                                                          cohortId = targetList$Id,
                                                          targetName = targetList$Name),
                                     outcomes = data.frame(outcomeId = outcomeList$Id,
                                                           cohortId = outcomeList$Id,
                                                           outcomeName = outcomeList$Name),
                                     populationSettings = populationSettings,
                                     modelList = modelList,
                                     covariateSettings = covariateSettings,
                                     resrictOutcomePops = resrictOutcomePops,
                                     resrictModelCovs = resrictModelCovs,
                                     resrictTargetCovs = resrictTargetCovs,
                                     executionSettings = executionSettings
                                     )

}

formatValidationSettings <- function(analysisList,
                                     modelList){

  #cohortDefitions
  cohortDefinitions <- getCohortDefinitions(modelList)

  #models
  # - name, settings, cohort_id, outcome_id, populationSettings, covariateSettings (fnct, settings),
  #   attr_predictionType, attr_type
  for(i in 1:length(modelList)){
    modelList[[i]] <- modelList[[i]][!names(modelList[[i]]) %in% c('new','cohort_json','cohort_json','outcome_json')]
    modelList[[i]]$populationSettings <- modelList[[i]]$populationSettings
    modelList[[i]]$covariateSettings <- createCovariateList(modelList[[i]])
    modelList[[i]]$settings <- list()
    modelList[[i]]$model <- list(formatValidationModel(modelList[[i]]$model))
  }


  validationStudyJsonList <- list(skeletonVersion = 'v1.0.1',
                                        skeletonType = "SkeletonPredictionValidationStudy",
                                        packageName = analysisList$packageName,
                                        packageDescription = analysisList$packageDescription,
                                        createdBy = analysisList$createBy,
                                        createdDate = as.character(Sys.Date()),
                                        organizationName = analysisList$organizationName,
                                        models = modelList,
                                        cohortDefinitions = cohortDefinitions)

  return(validationStudyJsonList)
}

getCohortDefinitions <- function(modelList){

  targets <- unique(lapply(1:length(modelList), function(i) modelList[[i]]$cohort_json))
  outcomes <- unique(lapply(1:length(modelList), function(i) modelList[[i]]$outcome_json))

  covariates <- list()
  for(i in 1:length(modelList)){
    if(length(modelList[[i]]$model$coefficients)>0){
      for(j in 1:length(modelList[[i]]$model$coefficients)){
        covariates[[length(covariates)+1]] <- modelList[[i]]$model$coefficients[[j]]$cohortjson
      }
    }
  }

  cohortDefinitions <- unique(c(targets, outcomes, covariates))

  return(cohortDefinitions)
}

createCovariateList <- function(x){

  if(length(x$model$coefficients)==0){
    return(NULL)
  }

  covSet <- list()
  length(covSet) <- length(x$model$coefficients)
  for(j in 1:length(x$model$coefficients)){
    settings <- x$model$coefficients[[j]]
    settings<- settings[!names(settings)%in%c('points','power','offset','cohortjson')]

    covSet[[j]] <- list(fnct = 'createCohortCovariateSettings',
                        settings = settings
    )
  }
  return(covSet)
}

formatValidationModel <- function(model){

  if(length(model$coefficients)==0){
    return(NULL)
  }

  coef <- lapply(1:length(model$coefficients), function(i){unlist(model$coefficients[[i]][c('covariateId','covariateName', 'points', 'offset', 'power')])})
  model$coefficients <- do.call('rbind', coef)

  return(model)
}



# format strToVect

strToVect <- function(x, sepVal=','){
  if(class(x)=="character"){
    if(length(grep(sepVal,x))>0){
      x <- as.double(strsplit(x, sepVal)[[1]])
      return(x)
    }
  }

  return(x)
}

# works correctly!
formatModelList <- function(mList){
  names <- unlist(lapply(mList, function(x) x$name))
  settings <- lapply(mList, function(x) x$settings)
  for(i in 1:length(settings)){
    settings[[i]] <- lapply(settings[[i]], strToVect)
  }
  names <- paste0(names,'Settings')

  result <- list()
  length(result) <- length(names)
  for( i in 1:length(names)){
    temp <- list(settings[[i]])
    names(temp) <- names[i]
    result[[i]] <- temp
  }
  #settings
  return(result)
}

# this is formatting correctly!
formatPopList <- function(popList){
  result <- lapply(popList, function(x) do.call('createStudyPopulationSettings', x, envir = environment(PatientLevelPrediction::createStudyPopulationSettings)))
  names(result) <- NULL

  return(result)
}

# need to remove names
formatCovariateList <- function(covList){

  for(i in 1:length(covList)){
    for(j in 1:length(covList[[i]])){

      covList[[i]][[j]]$settings <- modifySettings(covList[[i]][[j]])
      covList[[i]][[j]]$fnct <- modifyFnct(covList[[i]][[j]]$fnct)
    }
  }
  names(covList) <- NULL
  #return(list(covList))
  return(covList)
}

modifyFnct <- function(name){
  if(name == 'Cohort'){
    return('createCohortCovariateSettings')
  }
  if(name == 'Standard'){
    return('createCovariateSettings')
  }
  if(name == 'Age'){
    return('createAgeCovariateSettings')
  }
  if(name == 'Measurement'){
    return('createMeasurementCovariateSettings')
  }
  if(name == 'MeasurementCohort'){
    return('createMeasurementCohortCovariateSettings')
  }

}

modifySettings <- function(listV){
  fnct <- listV$fnct
  settings <- listV$settings

  if(fnct == 'Cohort'){
    names(settings) <- gsub('cohortCov', '', names(settings))
  }
  if(fnct =='Standard'){
    if(settings$includedCovariateIds == ''){
      settings$includedCovariateIds <- c()
    } else{
      settings$includedCovariateIds <- c(strToVect(settings$includedCovariateIds))
    }
    if(settings$excludedCovariateConceptIds == ''){
      settings$excludedCovariateConceptIds <- c()
      settings$addDescendantsToExclude <- NULL
    } else{
      settings$excludedCovariateConceptIds <- c(strToVect(settings$excludedCovariateConceptIds))
    }
    if(settings$includedCovariateConceptIds == ''){
      settings$includedCovariateConceptIds <- c()
      settings$addDescendantsToInclude <- NULL
    } else{
      settings$includedCovariateConceptIds <- c(strToVect(settings$includedCovariateConceptIds))
    }
    settings <- do.call(FeatureExtraction::createCovariateSettings, settings)
  }
  if(fnct == 'Age'){
    names(settings) <- gsub('ageCov', '', names(settings))
  }
  if(fnct == 'Measurement'){
    names(settings) <- gsub('measureCov', '', names(settings))
    if(!is.null(settings$conceptSet)){
      settings$conceptSet <- strToVect(settings$conceptSet)
    }
  }
  if(fnct == 'MeasurementCohort'){
    names(settings) <- gsub('measurementCohortCov', '', names(settings))
  }

  return(settings)
}
