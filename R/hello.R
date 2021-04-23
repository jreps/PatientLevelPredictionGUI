openGUI <- function(testing = F){
  PatientLevelPrediction:::ensure_installed("shiny")
  PatientLevelPrediction:::ensure_installed("shinydashboard")
  PatientLevelPrediction:::ensure_installed("shinycssloaders")
  PatientLevelPrediction:::ensure_installed("DT")
  PatientLevelPrediction:::ensure_installed("htmlwidgets")
  PatientLevelPrediction:::ensure_installed("shinyWidgets")
  PatientLevelPrediction:::ensure_installed("plotly")

  appDir <- system.file("shiny", "PlpGUIApp", package = "PatientLevelPredictionGUI")
  if(testing){
    appDir <- file.path(getwd(), 'inst',"shiny", "PlpGUIApp")
  }
  shiny::runApp(appDir)
}
