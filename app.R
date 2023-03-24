library(shiny)

# Define UI for app 
ui <- 
  navbarPage("Critère de sécurité routière",
             tabPanel("Informations générales",
                      fluidRow(
                        # Selections des paramètres LEFT
                        #sidebarPanel(
                        
                        # Dossier initial
                        column(3,
                               textInput(
                                 "dosinit",
                                 label = h4("Dossier contenant les données"),
                                 value = "./data/"
                               ), ),
                        
                        column(3, 
                          # Localisation selection
                          selectInput(
                            "LocalisationId",
                            label = h4("Localisation à afficher"),
                            choices = list(
                              "Localisation 1" = 1,
                              "Localisation 2" = 2,
                              "Localisation 3" = 3,
                              "Localisation 4" = 4
                            ),
                            selected = 1
                          ),
                        ),
                        
                        column(3,
                          sliderInput(
                            "DistanceMin",
                            label = h4("Distance minimum de filtrage"),
                            min = 0,
                            max = 100,
                            value = 22
                          ),
                        ),
                        
                        column(3,
                          checkboxGroupInput(
                            "checkGroup",
                            label = h4("Données à afficher"),
                            choices = list(
                              "Zone d'étude" = 1,
                              "Trajectoires" = 2,
                              "Comptages" = 3
                            ),
                            selected = 1
                          ),
                        ),
                      ),
                      
                      fluidRow(column(8,offset=2,
                        dataTableOutput("infoArray"))
                        ),
                      
                      fluidRow(
                        column(8,offset=2,
                               plotOutput("zoneEtude"))
                      ),
                      fluidRow(
                        column(8,offset=2,
                               plotOutput("trajectoires"))
                      ),
                      
                      fluidRow(column(8,offset=2,
                                      dataTableOutput("countArray"))
                      ),
             ),
             tabPanel("Etude de véhicules ")
  )




# Define server logic required to draw a histogram ----
server <- function(input, output) {
  # Définition des variables étudiés par les widgets
  
  source("~/TrajectoryAnalysisProject/code/00_init.r", echo=FALSE)
  source("~/TrajectoryAnalysisProject/code/01_plotUtils.R", echo=FALSE)
  
  output$infoArray <- renderDataTable({
    LocId <- input$LocalisationId
    recordingMeta[locationId==LocId,
                             .("ID"=recordingId,
                               "Jour d'enregistrement"=weekday,
                               'Heure de début'=paste(startTime,"h00",sep=""),
                               'Durée (min)'=paste(round(duration/60,0),"min"),
                               'Usagers observés'=numVehicles)] 
  })
  
  output$countArray <- renderDataTable({
    LocId <- input$LocalisationId
    if("3" %in% input$checkGroup){
        totalTime = unlist(sum(recordingMeta[locationId==LocId,'duration']))/3600
        data.table("Type"=c("Véhicules", "Piétons", "Cyclistes", "Total"),
                   "Comptage" = c(
                     paste(round((n_distinct(trajectoriesDataset[class %in% c('car','truck_bus'), 'trackId'])/ totalTime), 0), 'usagers/heure'),
                     paste(round((n_distinct(trajectoriesDataset[class == 'pedestrian'          , 'trackId'])/ totalTime), 0), 'usagers/heure'),
                     paste(round((n_distinct(trajectoriesDataset[class == 'bicycle'             , 'trackId'])/ totalTime), 0), 'usagers/heure'),
                     paste(round((n_distinct(trajectoriesDataset$trackId)/totalTime),0),'usagers/heure')))
        }
  })

  output$zoneEtude <-
    renderPlot(height = 500, width = 800, {
      if("1" %in% input$checkGroup) {
        LocId <- input$LocalisationId
        initPlotImage(LocId) # Initialisation de l'image
        drawEmptyPlot("Zone d'étude")
      }
    })

  output$trajectoires <-
    renderPlot(height = 500, width = 800, {
      if ("2" %in% input$checkGroup) {
        dosinit <- input$dosinit
        LocId <- input$LocalisationId
        initPlotImage(LocId) # Initialisation de l'image
        loadData(dosinit,LocId)
        trajectoriesDataset <<- cleanDataset(input$DistanceMin)
        par(mfrow = c(2, 2))
        drawTrajectories(FALSE)
      }
    })
}



shinyApp(ui = ui, server = server)

