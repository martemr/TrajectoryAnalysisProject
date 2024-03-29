##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : March 2023, 20th
# Description : UI de la page "Etude détaillée"
##---------------------------------------------

detailledStudy <- function(input, output) {
  tabPanel("Etude détaillée",
           #==========================================
           # Onglet : CONDITIONS
           #==========================================
           sidebarPanel(h3("Type d'étude"),
                        
                        # Selection du type d'étude
                        selectInput(
                          "studyType",
                          label = "",
                          choices = list("Classe" = 1,
                                         "Fréquentation" = 2,
                                         "Interractions" = 3
                                         )
                        ),
                        
                        #==========================================
                        # Type d'étude : CLASSE
                        #==========================================
                        conditionalPanel("input.studyType.includes('1')",
                                         h3("Parametres d'étude"),
                                         
                                         # Véhicule
                                         selectInput("classId",
                                                     label = h4("Véhicule étudié"),
                                                     choices = list("Voitures"    = 'car',
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
                                                              "Flux" = 3#,
                                                              #"Circulation sur la chausée" = 4,
                                                              #"Vitesse" = 5,
                                                              #"Zone d'attente" = 6,
                                                              #"Inclure annotations" = 7
                                                            ),
                                                           ), 
                                         ), 
                        
                        #==========================================
                        # Type d'étude : FREQUENTATION
                        #==========================================
                        conditionalPanel("input.studyType.includes('2')",
                                         h3("Parametres d'étude"),
                                         
                                         # Classes à compter
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
                                         
                                         # Comparaison des horaires de fréquentation
                                         # checkboxGroupInput(
                                         #   "timeCompare",
                                         #   label = h4("Horaires à comparer"),
                                         #   choices = list(
                                         #     "1"   = 1,
                                         #     "1"  = 2,
                                         #     "1" = 3,
                                         #     "1" = 4
                                         #   ),
                                         #   selected = 1
                                         # ),
                                        ), 
  
                        #==========================================
                        # Type d'étude : INTERACTIONS
                        #==========================================
                        conditionalPanel("input.studyType.includes('3')",
                                         h3("Parametres d'étude"),
                                         
                                         # Selection du recordingId
                                         textInput(
                                           "recordingInput",
                                           label = h4("Choisir l'enregistrement sur lequel prendre les interactions"),
                                           value = 0
                                         ),
                                         
                                         # Selection du trackId
                                         textInput("trackIdInterraction",
                                                   label = h4("track à étudier"),
                                                   value = 7),
                                         
                                         # Liste déroulante
                                         # ch <- list(trajectoriesDataset[,'trackId']),
                                         # names(ch)<-ch,
                                         # selectInput("trackIdInterraction",label="",
                                         #             choices = ch
                                         # )
                                        ), 
                        
                        #==========================================
                        # SOUS ONGLET : DESCRIPTION
                        #==========================================
                        
                        # Etude par Classe
                        conditionalPanel(
                          "input.studyType.includes('1')",
                          h3("Etude par classe"),
                          p(
                            "L'étude par classe est l'étude de l'intersection par type de véhicule. Le but est de visualiser en détail les déplacements des usagers et les éventuelles contraintes que cela peut entrainer."
                          ),
                        ), 
                        
                        # Etude par Fréquentation
                        conditionalPanel(
                          "input.studyType.includes('2')",
                          h3("Etude par fréquentation"),
                          p(
                            "L'étude par fréquentation permet d'évaluer la taille du carrefour et ses fréquentations. On retrouve notamment le comptage des véhicules, l'utilisation de la chausée."
                          ),
                        ),
                        
                        # Etude par Interaction
                        conditionalPanel(
                          "input.studyType.includes('3')",
                          h3("Etude des interractions"),
                          p(
                            "Les interractions au sein d'un carrefour permettent de mieux comprendre les conflits et les conceptions à revoir."
                          ),
                        ),
                        
                        # Partie description
                        h3("Description"),
                        
                        # Message d'attente
                        conditionalPanel("!(input.classParams.length>0)",
                                         p('Veuillez selectionner une donnée à afficher.')),
                        
                        # Description : Trajectoires
                        conditionalPanel(
                          "input.classParams.includes('1') & input.studyType.includes('1')",
                          h4("Trajectoires"),
                          p(
                            "La courbe qui s'affiche est le tracé des trajectoires de la classe sélectionné sur l'intersection précédemment sélectionnée. Toutes les trajectoires sont superposées et une ligne est une trajectoire."
                          )
                        ),
                        
                        # Description : Groupe de trajectoires
                        conditionalPanel(
                          "input.classParams.includes('2') & input.studyType.includes('1')",
                          h4("Groupes de trajectoires"),
                          p(
                            "Les trajectoires affichés sont groupés par couleur. Un groupe est un ensemble de trajectoire ayant des caractéristiques communes : même origine et destination, tracé très similaire, etc."
                          )
                        ),
                        
                        # Description : Flux
                        conditionalPanel(
                          "input.classParams.includes('3') & input.studyType.includes('1')",
                          h4("Flux"),
                          p(
                            "Le tracé des flux regroupe les trajectoires principales en une trajectoire plus visible. La taille des trajectoires est proportionnelle au volume de véhicule par heure. La flèche indique la destination."
                          )
                        ),
                        
                        # Description : Circulation sur la chaussée
                        conditionalPanel(
                          "input.classParams.includes('4') & input.studyType.includes('1')",
                          h4("Circulation sur la chausée"),
                          p(
                            "Il peut être intéressant pour comprendre l'utilisation d'un carrefour de relever où les usagers circulent. La courbe affiché permet de distinger les utilisations de la chaussée des utilisations du trotoir."
                          )
                        ),
                      ), 
  
    
           #==========================================
           # Onglet : MAIN PANEL
           #==========================================
           mainPanel(
             #==========================================
             # Type d'étude : CLASSE
             #==========================================
             # Trajectoires
             conditionalPanel(
               "input.classParams.includes('1') & input.studyType.includes('1')",
               withSpinner(plotOutput("classTrajectoire", height =
                                        800), type = 1)
             ),
             
             # Groupes de trajectoires
             conditionalPanel(
               "input.classParams.includes('2') & input.studyType.includes('1')",
               withSpinner(plotOutput("clusterTrajectoire", height = 800), type = 1)
             ),
             
             # Flux
             conditionalPanel(
               "input.classParams.includes('3') & input.studyType.includes('1')",
               withSpinner(plotOutput("flowDiagram", height = 800), type = 1)
             ),
             
            # #  Circulations sur la chausée
            # conditionalPanel(
            #   "input.classParams.includes('4') & input.studyType.includes('1')",
            #   withSpinner(plotOutput("onRoadPlot", height=800), type=1)
            # ), 
            # 
            # #  Vitesse
            # conditionalPanel(
            #   "input.classParams.includes('5') & input.studyType.includes('1')",
            #   withSpinner(plotOutput("speedHeatMap", height=800), type=1)
            # ),
            # 
            # #  Zone d'attente
            # conditionalPanel(
            #   "input.classParams.includes('6') & input.studyType.includes('1')",
            #   plotOutput("waitingArea", height=800)
            # ),
            # 
            # #  Inclure annotations
      
            #==========================================
            # Type d'étude : FREQUENTATION
            #==========================================
            #  Comptage
            conditionalPanel(
              "input.classParams.length>0 & input.studyType.includes('2')",
              withSpinner(dataTableOutput("countArray"), type = 1),
              plotOutput("countChart", height = "auto")
            ),
            
            # #  Horaires
            # conditionalPanel(
            #   "input.timeCompare.length>0 & input.studyType.includes('2')",
            #                  plotOutput("timeCompare", height = "auto")
            # ),
      
            #==========================================
            # Type d'étude : INTERACTIONS
            #==========================================
            conditionalPanel(
              "input.classParams.length>0 & input.studyType.includes('3')",
              plotOutput("interractionPlot", height = "auto")
            ), 
           )
          )
}