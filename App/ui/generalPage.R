generalPage <- function(input,output){
  tabPanel("Général",
    # CONDITIONS
    sidebarPanel(
      # Dossier de données
      h3("Parametres d'étude"),
      textInput("dosinit",
        label = h4("Dossier contenant les données"),
        value = "../data/"
      ),
  
      # Localisation
      selectInput(
        "LocalisationId",
        label = h4("Localisation à afficher"),
        choices = list(
          "Localisation 1" = 1,
          "Localisation 2" = 2,
          "Localisation 3" = 3,
          "Localisation 4" = 4
        )
      ),
      
      # Données à afficher
      checkboxGroupInput(
        "dataToPrintGeneral",
        label = h4("Données à afficher"),
        choices = list(
          "Information sur les enregistrements" = 1,
          "Zone d'étude" = 2,
          "Trajectoires" = 3
        ), selected = 2
      ),
    ),
    
    # DONNEES CONDITIONNELLES
    mainPanel(
      # Informations
      conditionalPanel(
        "input.dataToPrintGeneral.includes('1')",
        dataTableOutput("infoArray")
      ),
      
      # Zone d'étude
      conditionalPanel(
        "input.dataToPrintGeneral.includes('2')",
        print("here"),
        align = "center",
        plotOutput("zoneEtude", height = "auto")
      ),
      
      # Trajectoires
      conditionalPanel(
        align = "center",
        "input.dataToPrintGeneral.includes('3')",
        withSpinner(plotOutput("trajectoires", height = 800), type = 1),
      ),
      

    )
  )
  
  
}
