checkPredictionSkeleton <- function(analysisList,
                                    targetList,
                                    outcomeList,
                                    modelList,
                                    covList,
                                    popList,
                                    OPList,
                                    MCList,
                                    TCList,
                                    executeList){

  msg <- checkShinyApp(analysisList, 'packageName', isNotNull = T, type = 'analysisList')
  msg <- addMessage(msg,checkShinyApp(analysisList, 'packageDescription', isNotNull = F, type = 'analysisList'))
  msg <- addMessage(msg,checkShinyApp(analysisList, 'createdBy', isNotNull = F, type = 'analysisList'))
  msg <- addMessage(msg,checkShinyApp(analysisList, 'organizationName', isNotNull = F, type = 'analysisList'))

  # dats.frame with: Name, Id
  print(targetList)
  msg <- addMessage(msg,checkShinyApp(targetList, c('Name','Id'), isNotNull = T, type = c('targetList')))
  msg <- addMessage(msg,checkShinyApp(outcomeList, c('Name','Id'), isNotNull = T, type = c('outcomeList')))

  msg <- addMessage(msg,ifelse(length(modelList)==0,' -- No Models specified',''))
  msg <- addMessage(msg,ifelse(length(covList)==0,' -- No Covariate settings specified',''))
  msg <- addMessage(msg,ifelse(length(popList)==0,' -- No Population Settings specified',''))

  msg <- addMessage(msg,checkShinyApp(executeList, 'minCovariateFraction', isNotNull = T, type = 'executeList'))
  msg <- addMessage(msg,checkShinyApp(executeList, 'normalizeData', isNotNull = F, type = 'executeList'))
  msg <- addMessage(msg,checkShinyApp(executeList, 'testSplit', isNotNull = F, type = 'executeList'))
  msg <- addMessage(msg,checkShinyApp(executeList, 'testFraction', isNotNull = F, type = 'executeList'))
  msg <- addMessage(msg,checkShinyApp(executeList, 'splitSeed', isNotNull = F, type = 'executeList'))
  msg <- addMessage(msg,checkShinyApp(executeList, 'nfold', isNotNull = F, type = 'executeList'))

  return(msg)
}


checkValidationSkeleton <- function(analysisList,
                                    modelList){

  msg <- checkShinyApp(analysisList, 'packageName', isNotNull = T, type = 'analysisList')
  msg <- addMessage(msg,checkShinyApp(analysisList, 'packageDescription', isNotNull = F, type = 'analysisList'))
  msg <- addMessage(msg,checkShinyApp(analysisList, 'createdBy', isNotNull = F, type = 'analysisList'))
  msg <- addMessage(msg,checkShinyApp(analysisList, 'organizationName', isNotNull = F, type = 'analysisList'))

  msg <- addMessage(msg,ifelse(length(modelList)==0,' -- No Models specified',''))

  return(msg)
}


checkShinyApp <- function(object, columnNames, isNotNull, type){

  for(i in 1:length(columnNames)){
    if(!columnNames[i]%in%names(object)){
      return(paste(' -- Missing',columnNames[i],'in',type))
    }

    if(is.na(object[columnNames[i]]) && isNotNull){
      return(paste(' -- Cannot be NULL:',columnNames[i]))
    }

    #if(!type %in% class(object[columnNames[i]])){
    #  return(paste('Incorrect type:',columnNames[i]))
    #}

  }

  return('')
}

addMessage <- function(newmsg, oldmsg){
  paste0(newmsg,oldmsg)
}
