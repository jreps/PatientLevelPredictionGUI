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


createJson <- function(createDevelopmentStudyJsonList, webApi){

  createDevelopmentStudyJsonList$webApi <- webApi

  # this write
  jsonForStudy <- do.call('createDevelopmentStudyJson', createDevelopmentStudyJsonList)

  return(jsonForStudy)
}

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

# works correctly!
formatModelList <- function(mList){
  names <- unlist(lapply(mList, function(x) x$name))
  settings <- lapply(mList, function(x) x$settings)
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
    settings <- do.call(FeatureExtraction::createCovariateSettings, settings)
  }
  if(fnct == 'Age'){
    names(settings) <- gsub('ageCov', '', names(settings))
  }
  if(fnct == 'Measurement'){
    names(settings) <- gsub('measurementCov', '', names(settings))
  }
  if(fnct == 'MeasurementCohort'){
    names(settings) <- gsub('measurementCohortCov', '', names(settings))
  }

  return(settings)
}
