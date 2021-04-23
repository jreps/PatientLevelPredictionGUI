getPlpPackageDeps <- function(package = 'PatientLevelPrediction',
                              branch = 'master'){
descPlp <- read.table(curl::curl(paste0('https://raw.githubusercontent.com/OHDSI/',package,'/',branch,'/DESCRIPTION')),sep = '?')
descPlp <- sapply(descPlp, FUN = as.character)

if(length(grep('Depends:',descPlp))>0 && length(grep('Imports:',descPlp))>0 && length(grep('Suggests:',descPlp))>0){
  depPk <- descPlp[(grep('Depends:',descPlp) + 1): (grep('Imports:',descPlp)-1)]
  importPk <- descPlp[(grep('Imports:',descPlp)+1):(grep('Suggests:',descPlp)-1)]
  suggestPk <- descPlp[(grep('Suggests:',descPlp)+1):ifelse(length(grep("Remotes:",descPlp))>0, grep("Remotes:",descPlp)-1, grep("NeedsCompilation:",descPlp) - 1)]

  plpPk <- rbind(data.frame(name = depPk, type = rep('Dependencies', length(depPk))),
                 data.frame(name = importPk, type = rep('Imports', length(importPk))),
                 data.frame(name = suggestPk, type = rep('Suggests', length(suggestPk)))
  )
}

if(length(grep('Depends:',descPlp))==0 && length(grep('Imports:',descPlp))>0 && length(grep('Suggests:',descPlp))>0){
  importPk <- descPlp[(grep('Imports:',descPlp)+1):(grep('Suggests:',descPlp)-1)]
  suggestPk <- descPlp[(grep('Suggests:',descPlp)+1):ifelse(length(grep("Remotes:",descPlp))>0, grep("Remotes:",descPlp)-1, grep("NeedsCompilation:",descPlp) - 1)]

  plpPk <- rbind(data.frame(name = importPk, type = rep('Imports', length(importPk))),
                 data.frame(name = suggestPk, type = rep('Suggests', length(suggestPk)))
  )
}

plpPk$name <- gsub('\t','', gsub(' ', '', gsub(',','', plpPk$name)))

names <- sapply(plpPk$name, function(x) strsplit(x, '\\(')[[1]][1])
version <- gsub('\\)','',sapply(plpPk$name, function(x) strsplit(x, '\\(')[[1]][2]))

plpPk$name <- names
plpPk$version <- version

return(plpPk)
}

getInstalledPk <- function(plpPk){
plpPk$versionInstalled <- unlist(lapply(1:length(plpPk$name), function(i){tryCatch({as.character(packageVersion(plpPk$name[i]))},
                                                                                      error = function(e){return('Not installed')}
)}))

rVersion <- R.Version()
plpPk$versionInstalled[plpPk$name=='R'] <- paste0(rVersion$major, '.', rVersion$minor)

return(plpPk)
}

versionCheck <- function(required, installed){
  if(is.na(required)){
    if(installed != 'Not installed'){
      return('Pass')
    }
  }

  if(!is.na(required)){

    if(length(grep('>=', required))>0){
      if(installed >= gsub('>=','', required)){
        return('Pass')
      } else{
        return('Fail')
      }
    }
    if(length(grep('>', required))>0){
      if(installed > gsub('>','', required)){
        return('Pass')
      } else{
        return('Fail')
      }
    }

  }
  return('Fail')
}


installInfo <- function(package = 'PatientLevelPrediction',
                        branch = 'master'){

  descPlp <- read.table(curl::curl(paste0('https://raw.githubusercontent.com/OHDSI/',package,'/',branch,'/DESCRIPTION')),sep = '?')
  descPlp <- sapply(descPlp, FUN = as.character)

  mainInfo <- data.frame(package = package,
                         versionAvailable = gsub(' ','', gsub('Version:','',descPlp[grep('Version:', descPlp )])),
                         versionInsalled = tryCatch({as.character(packageVersion(package))},
                                                    error = function(e){return('Not installed')}))

  # run to get deps for PLP master
  plpPk <- getPlpPackageDeps(package = package, branch = branch)
  plpPk <- getInstalledPk(plpPk)
  plpPk$check <- apply(plpPk, 1, function(x){versionCheck(x['version'], x['versionInstalled'])})

  return(list(main = mainInfo,
              dependencies = plpPk))
}
