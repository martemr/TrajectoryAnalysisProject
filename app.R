library(shiny)
library(shinycssloaders)
#library(ggplot2)

# Define UI for app 
ui <- 
  navbarPage("Critère de sécurité routière",
             tabPanel("Acceuil",
                      textOutput("results"),
                      img(src='logoCerema.png', align = "center"),
                      fluidRow( align="center",
                        titlePanel(h1("Définition d'un critère global de sécurité routiere par carrefour",align="center")),
                        h3("Présentation"),
                        p("Pour cet exemple d'étude, nous utilisons un jeu de données Allemand, le dataset inD (",
                          a("https://www.ind-dataset.com"),"), datant de 2020."),
                        p("Il comprend 32 enregistrements filmé par des drônes au dessus de 4 carrefours différents. Chaque enregistrement est ",
                          strong("déjà pré-traité"),
                          " : Il y a 4 usagers différents : piétons, voitures, bus/camion, vélos. Les vidéos sont découpés en 25 image par secondes (frames). Chaque usager est détecté sur l'image. Le jeu de donnée est donc un ensemble de lignes (plusieurs millions) où chaque ligne est un usager et les variables qui le décrivent (type, position, vitesse, etc.) situé dans une image, elle même situé dans l'ensemble de vidéo."),
                        p("Le jeu de donnée ne nous donne pas accès aux vidéos originales."),
                      ),
                      fluidRow(align='center',
                               h3("Données d'étude"),
                               withSpinner(textOutput("waitingText"), type=1)),
                      fluidRow(align = 'center',
                               img(src='plaquette.png', align = "center"), ), 
                      fluidRow(align='center',
                               withSpinner(textOutput("null"), type=1)),
                      
                      
             ), 
             
             tabPanel("Informations générales",
                      # CONDITIONS
                      sidebarPanel(
                        #plotOutput("ceremaImage")
                        h3("Parametres d'étude"),
                        # textInput(
                        #   "dosinit",
                        #   label = h4("Dossier contenant les données"),
                        #   value = "./data/"
                        # ),
                        
                        selectInput("LocalisationId", 
                                    label = h4("Localisation à afficher"),
                                    choices = list(
                                      "Localisation 1" = 1,
                                      "Localisation 2" = 2,
                                      "Localisation 3" = 3,
                                      "Localisation 4" = 4
                                    )),
                        
                        # sliderInput(
                        #   "DistanceMin",
                        #   label = h4("Distance minimum de filtrage"),
                        #   min = 0,
                        #   max = 100,
                        #   value = 22
                        # ),
                        
                        checkboxGroupInput(
                          "dataToPrint",
                          label = h4("Données à afficher"),
                          choices = list(
                            "Information sur les enregistrements" = 4,
                            "Zone d'étude" = 1,
                            "Trajectoires" = 2,
                            "Comptages" = 3
                          ),
                          selected = 1
                        ),
                      ),
                      
                      # DONNEES CONDITIONNELLES
                      mainPanel(
                        conditionalPanel(
                          "input.dataToPrint.includes('4')",
                          dataTableOutput("infoArray")
                        ),
                        
                        conditionalPanel(align="center",
                          "input.dataToPrint.includes('1')",
                          plotOutput("zoneEtude", height = "auto")
                        ),
                      
                        conditionalPanel(align="center",
                          "input.dataToPrint.includes('2')",
                          withSpinner(plotOutput("trajectoires", height=800),type=1),
                        ),
                        
                        conditionalPanel(
                          "input.dataToPrint.includes('3')",
                          withSpinner(dataTableOutput("countArray"),type=1),
                          plotOutput("countChart", height = "auto")
                        ),
                      )
             ),
             
             
             tabPanel("Etude de véhicules ", 
                      # CONDITIONS
                      sidebarPanel(
                        h3("Parametres d'étude"),
                        selectInput("classId", 
                                    label = h4("Véhicule étudié"),
                                    choices = list(
                                      "Voitures"    = 'car',
                                      "Piétons"     = 'pedestrian',
                                      "Cyclistes"   = 'bicycle',
                                      "Camions/Bus" = 'truck_bus'
                                    )),
                        checkboxGroupInput(
                          "classParams",
                          label = h4("Données à afficher"),
                          choices = list(
                            "Trajectoires" = 1,
                            "Groupes par trajectoire" = 2,
                            "Flux" = 3,
                            "Circulation sur la chausée" = 4
                          ),
                          selected = 1
                        ),
                      ),
                      
                      mainPanel(
                        conditionalPanel(
                          "input.classParams.includes('1')",
                          withSpinner(plotOutput("classTrajectoire", height=800), type=1),
                        ),
                        
                        conditionalPanel(
                          "input.classParams.includes('2')",
                          withSpinner(plotOutput("clusterTrajectoire", height=800), type=1),
                        ),
                        
                        conditionalPanel(
                          "input.classParams.includes('3')",
                          withSpinner(plotOutput("flowDiagram", height=800), type=1), 
                        ),
                        
                        # conditionalPanel(
                        #   "input.classParams.includes('4')",
                        #   plotOutput("onRoadPlot", height=800)
                        # ),
                      )
             ),
             tabPanel("Interractions ", 
                      # # CONDITIONS
                      # 
                      # sidebarPanel(
                      #   selectInput("trackIdSelected", 
                      #               label = h4("Véhicule étudié"),
                      #               choices = list(unique(trajectoriesDataset$trackId)))
                      # ),
                      # mainPanel(
                      #   plotOutput("interractionsPlot", height=800), 
                      # ),
                      
                      )
  )




# Define server logic required to draw a histogram ----
server <- function(input, output) {
  # Définition des variables étudiés par les widgets
  print("Initialisation des fonctions")
  source("~/TrajectoryAnalysisProject/code/00_init.r", echo=FALSE)
  source("~/TrajectoryAnalysisProject/code/01_plotUtils.R", echo=FALSE)
  source("~/TrajectoryAnalysisProject/code/02_visualisationUtils.R", echo=FALSE)
  source("~/TrajectoryAnalysisProject/code/clustering/04_origine-destinationClustering.r", echo=FALSE)
  print(" → Fonctions initialisés")
  
  #############################
  # 0 : Page présentation
  #############################
  output$ceremaImage <- renderImage({
    filename <- '/images/logoCerema.png'
    list(src = paste(getwd(),filename, sep=""),
         contentType = '', alt="cerema")
  }, deleteFile = FALSE)
  
  output$plaquetteImage <- renderImage({
    list(src = paste(getwd(),"/images/plaquette.png", sep=""),
         contentType = '', alt="cerema")
  }, deleteFile = FALSE)

  # Chargement des données
  output$waitingText <- renderText({
    start_time <- Sys.time()
    dosinit <<- "./data/"
    loadData(dosinit)
    cleanDataset()
    end_time <- Sys.time()
    paste(nrow(tracks), "points chargés en", round(end_time-start_time,2), "secondes")
  })
  
  output$null <- renderText({
    LocId <- input$LocalisationId
    createAllClustersOfLocalisation(LocId, tracksMeta)
    ""
  })
  
  
    
  #############################
  # 1: Infos générales
  #############################
  output$infoArray <- renderDataTable({
    LocId <- input$LocalisationId
    recordingMeta[locationId==LocId,
                             .("ID"=recordingId,
                               "Jour d'enregistrement"=weekday,
                               'Heure de début'=paste(startTime,"h00",sep=""),
                               'Durée (min)'=paste(round(duration,0),"min"),
                               'Usagers observés'=numVehicles)]
    })
  
  output$countArray <- renderDataTable({
    LocId <- input$LocalisationId
    totalTime = unlist(sum(recordingMeta[locationId==LocId,'duration']))/60
    data.table("Type"=c("Véhicules", "Piétons", "Cyclistes", "Total"),
               "Comptage" = c(paste(round((n_distinct(trajectoriesDataset[locationId==LocId & class %in% c('car','truck_bus'), 'trackId'])/ totalTime), 0), 'usagers/heure'),
                              paste(round((n_distinct(trajectoriesDataset[locationId==LocId & class == 'pedestrian'          , 'trackId'])/ totalTime), 0), 'usagers/heure'),
                              paste(round((n_distinct(trajectoriesDataset[locationId==LocId & class == 'bicycle'             , 'trackId'])/ totalTime), 0), 'usagers/heure'),
                              paste(round((n_distinct(trajectoriesDataset[locationId==LocId,trackId])/totalTime),0),'usagers/heure')))
    })
  
  output$countChart <- renderPlot({
    LocId <- input$LocalisationId
    drawPieChart(LocId)
    }, width = 900, height=600)

  output$zoneEtude <- renderPlot({
    LocId <- input$LocalisationId
    initPlotImage(LocId, dosinit) # Initialisation de l'image
    drawEmptyPlot("Zone d'étude")
  }, width = 900, height = 600)

  output$trajectoires <-
    renderCachedPlot({
      LocId <- input$LocalisationId
      #par(mfrow = c(2, 2))
      drawTrajectories(LocId, dosinit, AllTrajectoriesOnOneGraph = TRUE)
    }, sizePolicy = sizeGrowthRatio(width= 900, height = 600, growthRate = 1),
    cacheKeyExpr = {list(input$LocalisationId)})
  
  #############################
  # 2: Etude de véhicule
  #############################
  output$classTrajectoire <-
    renderCachedPlot({
      print("Chargement trajectoire")
      LocId <- input$LocalisationId
      class <- input$classId
      drawTrajectories(LocId,dosinit, AllTrajectoriesOnOneGraph = TRUE, StudiedClass = class)
      print(" → done")
    },
    cacheKeyExpr = {list(input$classId, input$LocalisationId)})
  
  output$clusterTrajectoire <- renderCachedPlot({
    print("Chargement clusters")
    LocId <- input$LocalisationId
    class <-input$classId
    drawClusters(class,LocId, clusters, clusterMeta, AllTrajectoriesOnOneGraph = TRUE)
    print(" → done")
  },
  cacheKeyExpr = {list(input$classId, input$LocalisationId)})
  
  output$flowDiagram <- renderCachedPlot({
    print("Chargement diagramme de flux")
    LocId <- input$LocalisationId
    class <-input$classId
    flowDiagram(selectedClass=class,LocationId=LocId,clusterMeta, WithAnnotations=FALSE, allFlowOnOneGraph=TRUE)
    print(" → done")
  },
  cacheKeyExpr = {list(input$classId, input$LocalisationId)})
  
  # output$onRoadPlot <- renderCachedPlot({
  #   print("Chargement ")
  #   LocId <- input$LocalisationId
  #   class <-input$classId
  #   drawOnRoad(trajectoriesDataset, class=class)
  #   print(" → done")
  # },
  # cacheKeyExpr = {list(input$classId, input$LocalisationId)})
  # 
  
  
  
  # output$interractionPlot <- renderPlot(
  #   tId <- input$trackIdSelected
  #   plotInterraction(tId)
  # )
  
}



shinyApp(ui = ui, server = server)

