##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : March 2023, 20th
# Description : Application de visualisation des données (Trajectoires sur carrefour par vue aérienne)
##---------------------------------------------

# Chargement des librairies et codes
library(shiny)
library(shinycssloaders)
source("./ui/welcomePage.R")
source("./ui/generalPage.R")
source("./ui/detailledStudyPage.R")
source("./server/loadingAll.R")
source("./server/detailledStudyServer.R")
source("./server/generalServer.R")


# Define UI for app 
ui <-
  navbarPage(
    "Sécurité routière d'un carrefour",
    welcomePage(),
    generalPage(input, output),
    detailledStudy(input,output)
  )

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  loadAll(input,output)

  generalServer(input,output)

  detailledStudyServer(input,output)
}

# Run app
shinyApp(ui = ui, server = server)