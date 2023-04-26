##---------------------------------------------
# Diagnostic SR via images aériennes
# CEREMA
# Author : Martin Emery
# Date : March 2023, 20th
# Description : UI de la page "Acceuil"
##---------------------------------------------

welcomePage <- function(){
  tabPanel(
    "Accueil",
    
    # Logo Cerema
    fluidRow(
      textOutput("results"),
      img(src = 'logoCerema.png', align = "center")
    ),
    
    # Descripton de l'utilisation
    fluidRow(
      align = "center",
      titlePanel(
        h1("Définition d'un critère global de sécurité routiere par carrefour", align = "center")
      ),
      h3("Présentation"),
      p("Pour cet exemple d'étude, nous utilisons un jeu de données Allemand, le dataset inD (",
        a("https://www.ind-dataset.com"), 
        "), datant de 2020."
      ),
      p("Il comprend 32 enregistrements filmé par des drônes au dessus de 4 carrefours différents. Chaque enregistrement est ",
        strong("déjà pré-traité"),
        " : Il y a 4 usagers différents : piétons, voitures, bus/camion, vélos. Les vidéos sont découpés en 25 image par secondes (frames). Chaque usager est détecté sur l'image. Le jeu de donnée est donc un ensemble de lignes (plusieurs millions) où chaque ligne est un usager et les variables qui le décrivent (type, position, vitesse, etc.) situé dans une image, elle même situé dans l'ensemble de vidéo."
      ),
      p("Le jeu de donnée ne nous donne pas accès aux vidéos originales."),
    ),
    
    # Texte de chargement des données
    fluidRow(
      align = 'center',
      h4("Données d'étude"),
      textOutput("waitingText")
    ),
    
    # Plaquette descritptive du projet
    fluidRow(align = 'center',
             img(src = 'plaquette.png', align = "center"),),
    
    # Appel au chargement en tache de fond des clusters
    fluidRow(align = 'center', textOutput("clustersLoad")),
  )
}
