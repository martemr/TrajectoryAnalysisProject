library(shiny)
library(shinycssloaders)
#library(ggplot2)
source("./ui/welcomePage.R")
source("./ui/generalPage.R")
source("./ui/etudeDetaillePage.R")
source("./server/loadingAll.R")
source("./server/detailledStudyServer.R")
source("./server/generalServer.R")


# Define UI for app 
ui <-
  navbarPage(
    "Sécurité routière d'un carrefour",
    welcomePage(),
    generalPage(input, output),
    etudeDetaille(input,output)
  )

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  if(!exists("initDone")) loadAll(input,output)

  generalManagement(input,output)

  detailledStudyServer(input,output)
}

shinyApp(ui = ui, server = server)