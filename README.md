# TrajectoryAnalysisProject

## Structure
Le code est concentré dans le dossier code. Les fichiers sont ensuite triés en 3 partie :
- Clustering : Codes permettant de regrouper les trajectoires en groupe
- Analysis : Analyse faites sur les trajectoires (ex: vitesse, interractions, etc.)
- Visualisation : Codes permettant de visualiser les analyses (diagramme des flux, carte de chaleur, etc.)

L'ensemble du code est conçu pour fonctionner avec le fichier **safetyCriteria.rmd**. Ce fichier regroupe les résultats intermédiaires à travers plusieurs zones d'appels au code.
Il est aussi documenté et simple d'utilisation. Il est conseillé d'utiliser ce code.

GIT : 
Le git comporte 2 branches : La main qui est fonctionelle et permet de charger et lire le safetyCriteria. L'autre est la phase d'expérimentation du critère de sécurité en interractif (library shiny).
