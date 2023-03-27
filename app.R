library(shiny)

# Define UI for app 
ui <- 
  navbarPage("Critère de sécurité routière",
             tabPanel("Présentation",
                      fluidRow(
                        plotOutput("ceremaImage", height = 150),
                        titlePanel(h1("Définition d'un critère global de sécurité routiere par carrefour",align="center")),
                        p("Pour cet exemple d'étude, nous utilisons un jeu de données Allemand, le dataset inD (",
                          a("https://www.ind-dataset.com"),", datant de 2020."),
                        p("Il comprend 32 enregistrements filmé par des drônes au dessus de 4 carrefours différents. Chaque enregistrement est ",
                          strong("déjà pré-traité"),
                          " : Il y a 4 usagers différents : piétons, voitures, bus/camion, vélos. Les vidéos sont découpés en 25 image par secondes (frames). Chaque usager est détecté sur l'image. Le jeu de donnée est donc un ensemble de lignes (plusieurs millions) où chaque ligne est un usager et les variables qui le décrivent (type, position, vitesse, etc.) situé dans une image, elle même situé dans l'ensemble de vidéo."),
                        p("Le jeu de donnée ne nous donne pas accès aux vidéos originales."),
                      ),
                      fluidRow(align = 'center',
                               plotOutput("plaquetteImage", height = 150), ), 
             ), 
             
             tabPanel("Informations générales",
                      # CONDITIONS
                      sidebarPanel(
                        h3("Parametres d'étude"),
                        textInput(
                          "dosinit",
                          label = h4("Dossier contenant les données"),
                          value = "./data/"
                        ),
                        
                        selectInput("LocalisationId", 
                                    label = h4("Localisation à afficher"),
                                    choices = list(
                                      "Localisation 1" = 1,
                                      "Localisation 2" = 2,
                                      "Localisation 3" = 3,
                                      "Localisation 4" = 4
                                    )),
                        
                        sliderInput(
                          "DistanceMin",
                          label = h4("Distance minimum de filtrage"),
                          min = 0,
                          max = 100,
                          value = 22
                        ),
                        
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
                        
                        conditionalPanel(
                          "input.dataToPrint.includes('1')",
                          plotOutput("zoneEtude")
                        ),
                      
                        conditionalPanel(
                          "input.dataToPrint.includes('2')",
                          plotOutput("trajectoires")
                        ),
                        
                        conditionalPanel(
                          "input.dataToPrint.includes('3')",
                          dataTableOutput("countArray")
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
                      ),
                      
                      mainPanel(
                        plotOutput("classTrajectoire"),
                        plotOutput("clusterTrajectoire")
                      )
             ),
  )




# Define server logic required to draw a histogram ----
server <- function(input, output) {
  # Définition des variables étudiés par les widgets
  print("Initialisation des fonctions")
  source("~/TrajectoryAnalysisProject/code/00_init.r", echo=FALSE)
  source("~/TrajectoryAnalysisProject/code/01_plotUtils.R", echo=FALSE)
  source("~/TrajectoryAnalysisProject/code/clustering/04_origine-destinationClustering.r", echo=FALSE)
  print(" → Fonctions initialisés")
  
  # 0 : Page présentation
  output$ceremaImage <- renderImage({
    filename <- '/images/logoCerema.png'
    list(src = paste(getwd(),filename, sep=""),
         contentType = '', alt="cerema")
  }, deleteFile = FALSE)
  
  output$plaquetteImage <- renderImage({
    list(src = paste(getwd(),"/images/plaquette.png", sep=""),
         contentType = '', alt="cerema")
  }, deleteFile = FALSE)
  
  #test <- reactiveVal()

  # 1: Infos générales
  output$infoArray <- renderDataTable({
    LocId <- input$LocalisationId
    dosinit <- input$dosinit
    loadData(dosinit, LocId)
    recordingMeta[locationId==LocId,
                             .("ID"=recordingId,
                               "Jour d'enregistrement"=weekday,
                               'Heure de début'=paste(startTime,"h00",sep=""),
                               'Durée (min)'=paste(round(duration/60,0),"min"),
                               'Usagers observés'=numVehicles)]
  })
  
  output$countArray <- renderDataTable({
    LocId <- input$LocalisationId
    totalTime = unlist(sum(recordingMeta[locationId==LocId,'duration']))/60
    data.table("Type"=c("Véhicules", "Piétons", "Cyclistes", "Total"),
               "Comptage" = c(
                 paste(round((n_distinct(trajectoriesDataset[class %in% c('car','truck_bus'), 'trackId'])/ totalTime), 0), 'usagers/heure'),
                 paste(round((n_distinct(trajectoriesDataset[class == 'pedestrian'          , 'trackId'])/ totalTime), 0), 'usagers/heure'),
                 paste(round((n_distinct(trajectoriesDataset[class == 'bicycle'             , 'trackId'])/ totalTime), 0), 'usagers/heure'),
                 paste(round((n_distinct(trajectoriesDataset$trackId)/totalTime),0),'usagers/heure')))
    })

  output$zoneEtude <-
    renderPlot({
        print("Chargement zone d'étude")
        dosinit <- input$dosinit
        LocId <- input$LocalisationId
        loadData(dosinit, LocId)
        trajectoriesDataset <<- cleanDataset(input$DistanceMin)
        initPlotImage(LocId, dosinit) # Initialisation de l'image
        drawEmptyPlot("Zone d'étude")
        print(" → done")
    })

  output$trajectoires <-
    renderCachedPlot({
      dosinit <- input$dosinit
      LocId <- input$LocalisationId
      par(mfrow = c(2, 2))
      drawTrajectories(FALSE)
    }, 
    cacheKeyExpr = {list(input$LocalisationId)})
  
  
  # 2: Etude de véhicule
  output$classTrajectoire <-
    renderCachedPlot({
      dosinit <- input$dosinit
      LocId <- input$LocalisationId
      loadData(dosinit, LocId)
      trajectoriesDataset <<- cleanDataset(input$DistanceMin)
      class <-input$classId
      drawTrajectories(AllTrajectoriesOnOneGraph = TRUE, StudiedClass = class)
    },
    cacheKeyExpr = {list(input$classId, input$LocalisationId)})
  
  output$clusterTrajectoire <- renderCachedPlot({
    LocId <- input$LocalisationId
    class <-input$classId
    clusters     <<- createClusters(tracksMeta, ClusteringClass = class)
    clusterMeta <<- getClusterMeta(tracksMeta, clusters)
    drawClusters(clusters, clusterMeta, AllTrajectoriesOnOneGraph = TRUE)
  },
  cacheKeyExpr = {list(input$classId, input$LocalisationId)})
}



shinyApp(ui = ui, server = server)

