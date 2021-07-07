openGUI <- function(testing = F){
  appDir <- system.file("shiny", "PlpGUIApp", package = "PatientLevelPredictionGUI")
  if(testing){
    appDir <- file.path(getwd(), 'inst',"shiny", "PlpGUIApp")
  }
  shiny::runApp(appDir)
}
