![](./App/www/logoCerema.png) 

# Critère de sécurité routière d'un carrefour

Le projet suivant est un projet visant à établir un critère de sécurité routière sur les carrefours à partir de données aériennes. Dans un premier temps il consiste seulement à l'analyse et le traitement de données provenant d'un dataset allemand : InD dataset. Ces données aériennes sont déjà traités et filtrés.

## Structure

Le langage utilisé tous le long du projet est en R. La structure du code est la suivante :\
|- `TrajectoryAnalysisProject/` \
&nbsp;&nbsp; \|-- `README.md`\
&nbsp;&nbsp; \|-- `App/` *Dossier contenant les codes qui permettent de faire fonctionner l'appli* \
&nbsp;&nbsp;&nbsp;&nbsp; \|-- `app.R` ***Code principal de l'application***\
&nbsp;&nbsp;&nbsp;&nbsp; \|-- `server/` *Server de l'appli* \
&nbsp;&nbsp;&nbsp;&nbsp; \|-- `ui/` *Interface de l'appli* \
&nbsp;&nbsp;&nbsp;&nbsp; \|-- `www` *Images de l'appli* \

&nbsp;&nbsp; \|-- `code/` *Dossier contenant tous les codes permetant de traiter et visualiser les données* \
&nbsp;&nbsp;&nbsp;&nbsp; \|-- `00_setup.R`   ***Installation de toutes les librairies utilisés dans les codes*** \
&nbsp;&nbsp;&nbsp;&nbsp; \|-- `01_init.R`      *Fonctions de chargement et nettoyage des données dans l'environnement* \
&nbsp;&nbsp;&nbsp;&nbsp; \|-- `02_plotUtils.R` *Fonctions permettant l'affichage de trajectoires et plans 2D* \
&nbsp;&nbsp;&nbsp;&nbsp; \|-- `03_visualisationUtils.R` *Fonction permettant l'affichage de courbes et graphiques* \
&nbsp;&nbsp;&nbsp;&nbsp; \|-- `analysis/`      *Codes permetant d'analyser les données (selon des paramêtres précis : vitesse, interactions, etc.)* \
&nbsp;&nbsp;&nbsp;&nbsp; \|-- `clustering/`    *Codes permetant de classer les trajectoires (Différentes méthodes disponible)* \
&nbsp;&nbsp;&nbsp;&nbsp; \|-- `prediction/`    *Codes permetant de prédire des trajectoires et établir un risque de colision entre 2 trajectoires* \
&nbsp;&nbsp;&nbsp;&nbsp; \|-- `visualisation/` *Codes permetant d'afficher des graphiques particuliers (flux, cartes de chaleur, etc.)* \

&nbsp;&nbsp; \|-- `data/` ***Dossier dans lequel déposer les données à analyser*** \

&nbsp;&nbsp; \|-- `safetyCriteria.Rmd` *Deprecated depuis 04/2023*


## Source de données
### Données d'exemple
Les données utilisés pour la création de ce modèle sont accessibles au dépôt suivant : https://www.ind-dataset.com/

### Format de données
Afin d'utiliser le code avec d'autres données initiales, il est nécessaire de respecter un format :

- Un fichier `XX_tracks.csv` où *XX* désigne le numéro d'enregistrement. Si il n'existe qu'un seul enregistrement, saisir 00.
&nbsp;&nbsp; \
**Format :** Dataset contenant une ligne par point observé. Le dataset doit contenir les colonnes :
  - "recordingId" : Id de l'enregistrement. Chaque enregistrement doit avoir un Id distinct pour ne pas confondre des trajectoires ayant lieu à des moments différents.
  - "trackId" : Id de la trajectoire. Permet de distinguer les trajectoires entre elles.
  - "frame" : Frame absolue dans la vidéo. Vidéo découpée en frames (=images) par seconde.
  - "trackLifetime" : Frame relative à la trajectoire. Sa valeur est 0 sur la première frame où apparait l'objet et s'incrémente d'un par frame.
  - "heading","width","length" : Informations sur la boite englobante de l'objet.
  - "xCenter","yCenter","xVelocity","yVelocity","xAcceleration","yAcceleration" : Informations sur la position de l'objet et son évolution dans le plan de coordonnées UTM.
  - "lonVelocity","latVelocity","lonAcceleration","latAcceleration" : Informations sur la position de l'objet et son évolution dans le plan latitude/longitude.
  
- Un fichier `XX_tracksMeta.csv` où *XX* désigne le numéro d'enregistrement. Il contient les métadonnées des tracks de cet enregistrement.
&nbsp;&nbsp; \
**Format :** Dataset contenant une ligne par trajectoire observé. Le dataset doit contenir les colonnes :
  - "recordingId" : Id de l'enregistrement. 
  - "trackId" : Id de la trajectoire. 
  - "initialFrame","finalFrame","numFrames" : information sur les frame de départ, arrivée et nombre de frames. Une trajectoire ne doit pas être intérrompue entre 2 frames.
  - "width","length" : Taille de l'objet. 0 pour un cycliste ou piéton. *Non utilisé dans cette application.*
  - "class" : La classe de l'objet parmi : ['pedestrian','bicycle','car','truck_bus']

- Un fichier `XX_recordingMeta.csv` où *XX* désigne le numéro d'enregistrement. Il contient les métadonnées des tracks de cet enregistrement.
&nbsp;&nbsp; \
**Format :** Dataset contenant une ligne par trajectoire observé. Le dataset doit contenir les colonnes :
  - "recordingId" : Id de l'enregistrement.
  - "locationId" : Identifiant de la localisation de l'enregistrement si plusieurs lieu d'enregistrement (exemple : plusieurs caméra). Sinon 1.
  - "frameRate" : Taux de découpe de la vidéo en images (exemple 25fps).
  - "speedLimit", "weekday", "startTime", "duration" : Informations sur l'enregistrement.
  - "numTracks", "numVehicles", "numVRUs" : Informations sur les objets détectés.
  - "latLocation", "lonLocation", "xUtmOrigin", "yUtmOrigin" : Position de l'origine de l'enregistrement.
  - "orthoPxToMeter" : Conversion pixel/mètres.
  
Pour plus de détail sur le format des données voir : https://www.ind-dataset.com/format

## Utilisation

-   Télécharger le dossier complet depuis github en zip ou en clonant le repository.
-   Ouvrir le projet `TrajectoryAnalysisProject.Rproj` sur **R studio**.
-   Exécuter le code `00_setup.R` afin d'installer les librairies nécessaires. *(Cela peut prendre un certain temps)*
-   **Pour utiliser l'application :** Exécuter le code `App/app.r`. L'application devrait s'ouvrir.
-   **Pour exécuter du code hors de l'application :** Exécuter au préalable le fichier `00_init.R`, puis dans un terminal R les fonctions : `loadData()` & `cleanDataset()`. Vous pouvez désormais utiliser chacune des fonctions du code. **Les dépendances entre codes et fonctions sont indiqués en début de fichier. La mention "DEPRECATED" indique que ce code n'est pas fait pour fonctionner dans l'environnement actuel. Il est reste disponible pour consultation.** Il faut exécuter le fichier au préalable pour charger ses fonctions dans l'environnement.
