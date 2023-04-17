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
      
      # DESCRIPTION
      h3("Description"),
      conditionalPanel(
        "!(input.dataToPrintGeneral.includes('1') || input.dataToPrintGeneral.includes('2') || input.dataToPrintGeneral.includes('3'))",
         p('Veuillez selectionner une donnée à afficher.')
      ),
      
      conditionalPanel(
        "input.dataToPrintGeneral.includes('1')",
        h4("Informations"),
        p("Les données que l'on possède sont un ensemble de plusieurs enregistrements. Ce tableau détaille ce que contient chacun des enregistrements de cette localisation.")
      ),
      
      conditionalPanel(
        "input.dataToPrintGeneral.includes('2')",
        h4("Zone d'étude"),
        p("La zone d'étude est la vue aérienne de l'intersection où les données ont été prélevés. Il y a 4 zones différentes.")
      ),
      
      conditionalPanel(
        "input.dataToPrintGeneral.includes('3')",
        h4("Trajectoires"),
        p("Ce graphique est le tracé de toutes les trajectoires vues lors des enregistrements. Chaque type de véhicule à une couleur, chaque ligne est un véhicule.")
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
