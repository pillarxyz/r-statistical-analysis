Cet ensemble de donné est collecté en faisant des analyse voix biomédicales de 42 personnes 
atteintes de la maladie de Parkinson à un stade précoce recrutées pour le test de six mois et suivie par
un dispositif de télésurveillance pour la surveillance à distance de la progression des symptômes. 
Les enregistrements ont été automatiquement capturés au domicile du patient. 
--------------Description des variables-----------------------------------
subject - identifiant de chaque patient
age - age de patient
sex - sexe de pactient '0' - homme, '1' - femme
test\_time : Temps écoulé depuis le début de recrutement de patient dans ce test . le nombre présenté c'est le nombre de jours.
motor\_UPDRS - Clinician's motor UPDRS score,
total\_UPDRS - Clinician's total UPDRS score, 
Jitter(%),Jitter(Abs),Jitter:RAP,Jitter:PPQ5,Jitter:DDP - différentes measures de la variation de fréquence
Shimmer,Shimmer(dB),Shimmer:APQ3,Shimmer:APQ5,Shimmer:APQ11,Shimmer:DDA - différentes measures de variation d'amplitude
NHR,HNR -  Deux mesures du ratio de bruit de les composantes tonales de la voix
RPDE - Une mesure de la complexité dynamique non linéaire
DFA - mesure signal fractale
PPE - mesure non linéare de la variation de fréquence 
-------------------------------------
Nombre de variable : 22
Nombre d'observation : 910
-----------------------------------------------
variable dépendant : "total\_UPDRS"  une échelle pour l’évaluation de la maladie de Parkinson 
il permet d’évaluer l’évolution de la maladie et l’impact des différents symptômes sur la qualité de vie de votre proche.
