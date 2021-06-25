# @file Ui.R
#
# Copyright 2020 Observational Health Data Sciences and Informatics
#
# This file is part of PatientLevelPrediction
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#     http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.

library(shiny)
library(plotly)
library(shinycssloaders)
library(shinydashboard)
library(shinyWidgets)
library(shinyDirectoryInput) #devtools::install_github('wleepang/shiny-directory-input')
library(httr)

##appdir <- system.file("shiny", "PlpGUIApp", package = "PatientLevelPredictionGUI")
appdir <- file.path("/Users/jreps/Documents/github/PatientLevelPredictionGUI/inst","shiny", "PlpGUIApp")

source(file.path(appdir,"modules","webApiModule.R"))
source(file.path(appdir,"modules","cohortExtractModule.R"))

source(file.path(appdir,"modules","installModule2.R"))

source(file.path(appdir,"modules","cohortModule.R"))
source(file.path(appdir,"modules","modelModule.R"))
source(file.path(appdir,"modules","covariateModule.R"))
source(file.path(appdir,"modules","populationModule.R"))
source(file.path(appdir,"modules","analysisModule.R"))
source(file.path(appdir,"modules","trainingModule.R"))
source(file.path(appdir,"modules","restrictionsModule.R"))
source(file.path(appdir,"modules","jsonModule.R"))
source(file.path(appdir,"modules","downloadModule.R"))

source(file.path(appdir,"modules","executeModule.R"))

source(file.path(appdir,"helpers","HelperCheckInputs.R"))
source(file.path(appdir,"helpers","HelperFormatInput.R"))
source(file.path(appdir,"helpers","createDevelopmentStudyJson.R"))



source(file.path(appdir,"modules","existingModelModule.R"))

actionButton <- shiny::actionButton

addInfo <- function(item, infoId) {
  infoTag <- tags$small(class = "badge pull-right action-button",
                        style = "padding: 1px 6px 2px 6px; background-color: steelblue;",
                        type = "button",
                        id = infoId,
                        "i")
  item$children[[1]]$children <- append(item$children[[1]]$children, list(infoTag))
  return(item)
}

ui <- shinydashboard::dashboardPage(skin = 'black',

                                    shinydashboard::dashboardHeader(title = "PLP-GUI",

                                                                    tags$li(div(img(src = 'logo.png',
                                                                                    title = "OHDSI PLP", height = "40px", width = "40px"),
                                                                                style = "padding-top:0px; padding-bottom:0px;"),
                                                                            class = "dropdown")


                                    ),

                                    shinydashboard::dashboardSidebar(
                                      shinydashboard::sidebarMenu(id ='menu',
                                                                  addInfo(shinydashboard::menuItem("Install", tabName = "Install", icon = shiny::icon("wrench")), "InstallInfo"),
                                                                  addInfo(shinydashboard::menuItem("Design", tabName = "Design", icon = shiny::icon("pencil-ruler")), "DesignInfo"),
                                                                  addInfo(shinydashboard::menuItem("Execute", tabName = "Execute", icon = shiny::icon("running")), "ExecuteInfo"),
                                                                  addInfo(shinydashboard::menuItem("View", tabName = "View", icon = shiny::icon("eye")), "ViewInfo"),
                                                                  addInfo(shinydashboard::menuItem("Help", tabName = "Help", icon = shiny::icon("info")), "HelpInfo")
                                      ),

                                      # scroller performanace - make conditional
                                      conditionalPanel(condition = "input.menu=='Design'",
                                                       shiny::selectInput(inputId = "designType",
                                                                          label = "Type",
                                                                          multiple = F,
                                                                          choices = c('Development','Validation'),
                                                                          selected = 'Development'),

                                                       webApiViewer("webApiMain"),
                                                       extractCohortsViewer("cohortExtract")



                                      )




                                    ), # end sidebar

                                    shinydashboard::dashboardBody(
                                      shinydashboard::tabItems(

                                        # help tab
                                        shinydashboard::tabItem(tabName = "Install",
                                                                installViewer("installPatientLevelPrediction", "PatientLevelPrediction"),
                                                                installViewer("installHydra", "Hydra"),
                                                                installViewer("installSkeletonPredictionStudy", "SkeletonPredictionStudy"),
                                                                installViewer("installSkeletonPredictionValidationStudy", "SkeletonPredictionValidationStudy")


                                        ),

                                        # help tab
                                        shinydashboard::tabItem(tabName = "Help",
                                                                shiny::h2("Information"),
                                                                shiny::p("Click on a row to explore the results for that model.  When you wish to explore a different model, then select the new result row and the tabs will be updated."),
                                                                shiny::a("Demo Video", href = 'https://youtu.be/StpV40yl1UE', target='_blank')
                                        ),

                                        # First tab content
                                        shinydashboard::tabItem(tabName = "Design",


                                                                conditionalPanel(condition = "input.designType=='Development'",


                                                                                 tabsetPanel(type = "tabs",

                                                                                             tabPanel("Settings",


                                                                                                      analysisViewer("analysisDev", "Analysis Settings"),

                                                                                                      cohortViewer("targetDev", "Target"),
                                                                                                      cohortViewer("outcomeDev", "Outcome"),

                                                                                                      modelViewer("modelDev", "Model Settings"),

                                                                                                      covariateViewer("covariateDev", "Covariate Settings"),

                                                                                                      populationViewer("populationDev", "Population Settings"),

                                                                                                      restrictionViewer("restrictionPODev", "Population/Outcome"),
                                                                                                      restrictionViewer("restrictionMCDev", "Model/Covariate"),
                                                                                                      restrictionViewer("restrictionTCDev", "Target/Covariate"),

                                                                                                      trainViewer("trainDev", "Training Settings")
                                                                                             ),

                                                                                             tabPanel("Json",
                                                                                                      jsonViewer("jsonDev", "json development")
                                                                                             ),

                                                                                             tabPanel("Download",
                                                                                                      downloadViewer("downloadDev", "download development")
                                                                                             )


                                                                                 )


                                                                ),
                                                                conditionalPanel(condition = "input.designType=='Validation'",
                                                                                 tabsetPanel(type = "tabs",

                                                                                             tabPanel("Settings",


                                                                                                      analysisViewer("analysisVal", "Analysis Settings"),

                                                                                                      cohortViewer("targetVal", "Target"),
                                                                                                      cohortViewer("outcomeVal", "Outcome"),

                                                                                                      populationViewer("populationVal", "Population Settings"),

                                                                                                      existingModelViewer("existingModel", "Model Settings")


                                                                                             ),

                                                                                             tabPanel("Json",
                                                                                                      jsonViewer("jsonVal", "json development")
                                                                                             ),

                                                                                             tabPanel("Download",
                                                                                                      downloadViewer("downloadVal", "download development")
                                                                                             )


                                                                                 )
                                                                )
                                        ),

                                        shinydashboard::tabItem(tabName = "Execute",
                                                                executeViewer("executePrediction", "Prediction")

                                        ),

                                        shinydashboard::tabItem(tabName = "View",
                                                                'Coming Soon'

                                        )


                                      )
                                    )
)
