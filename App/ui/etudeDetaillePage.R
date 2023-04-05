etudeDetaille <- function(input, output){
  tabPanel("Etude détaillée",
    # CONDITIONS
    sidebarPanel(
      h3("Type d'étude"),
      
      selectInput("studyType",label="",
        choices = list(
          "Classe" = 1,
          "Fréquentation" = 2,
          "Interractions" = 3
        )
      ),
  
      # Type d'étude = CLASSE
      conditionalPanel("input.studyType.includes('1')",
        h3("Parametres d'étude"),
        # Véhicule
        selectInput("classId",
          label = h4("Véhicule étudié"),
          choices = list(
            "Voitures"    = 'car',
            "Piétons"     = 'pedestrian',
            "Cyclistes"   = 'bicycle',
            "Camions/Bus" = 'truck_bus'
          ),
        ),
        # Courbes à afficher
        checkboxGroupInput("classParams",
          label = h4("Données à afficher"),
          choices = list(
            "Trajectoires" = 1,
            "Groupes de trajectoire" = 2,
            "Flux" = 3,
            "Circulation sur la chausée" = 4, 
            "Vitesse" = 5,
            "Zone d'attente" = 6,
            "Inclure annotations" = 7
          ),
          selected = 1
        ),
      ),
  
      # Fréquentation
      conditionalPanel("input.studyType.includes('2')",
        h3("Parametres d'étude"),

        checkboxGroupInput(
          "countClass",
          label = h4("Comptage"),
          choices = list(
            "Voitures"    = 'car',
            "Piétons"     = 'pedestrian',
            "Cyclistes"   = 'bicycle',
            "Camions/Bus" = 'truck_bus'
          ),
        ),

        checkboxGroupInput(
          "timeCompare",
          label = h4("Horaires à comparer"),
          choices = list(
            "1"   = 1,
            "1"  = 2,
            "1" = 3,
            "1" = 4
          ),
          selected = 1
        ),
      ),
  
  
      conditionalPanel("input.studyType.includes('3')",
                       h3("Parametres d'étude"),
                       
      )
    ),
  
    
    # MAIN PANEL
    mainPanel(
      # CLASSE
      #  Trajectoires
      conditionalPanel(
        "input.classParams.includes('1') & input.studyType.includes('1')",
        withSpinner(plotOutput("classTrajectoire", height =
                                 800), type = 1)
      ),
      
      #  Groupes de trajectoires
      conditionalPanel(
        "input.classParams.includes('2') & input.studyType.includes('1')",
        withSpinner(plotOutput("clusterTrajectoire", height = 800), type = 1)
      ),
      
      #  Flux
      conditionalPanel(
        "input.classParams.includes('3') & input.studyType.includes('1')",
        withSpinner(plotOutput("flowDiagram", height =
                                 800), type = 1)
      ),
      
      #  Circulations sur la chausée
      conditionalPanel(
        "input.classParams.includes('4') & input.studyType.includes('1')",
        plotOutput("onRoadPlot", height=800)
      ), 
      
      #  Vitesse
      conditionalPanel(
        "input.classParams.includes('5') & input.studyType.includes('1')",
        plotOutput("speedHeatMap", height=800)
      ),
      
      #  Zone d'attente
      conditionalPanel(
        "input.classParams.includes('6') & input.studyType.includes('1')",
        plotOutput("waitingArea", height=800)
      ),
      
      #  Inclure annotations
      
      
      
      
      
      # FREQUENTATION
      #  Comptage
      conditionalPanel(
        "input.classParams.length>0 & input.studyType.includes('2')",
        withSpinner(dataTableOutput("countArray"), type = 1),
        plotOutput("countChart", height = "auto")
        ),
      
      #  Horaires
      conditionalPanel(
        "input.timeCompare.length>0 & input.studyType.includes('2')",
                       plotOutput("timeCompare", height = "auto")
      ),
      
      
      
  

  
      
  

    )
  )
}