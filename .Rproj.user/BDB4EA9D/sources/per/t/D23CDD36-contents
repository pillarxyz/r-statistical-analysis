#' --- 
#' title: "Classification"
#' author: "Ayman Lafaz, Ilyas Sekkaoui" 
#' fig_width: 10
#' fig_height: 10
#' --- 


#'introduction : l'objectif de cette partie c'est d'appliquer la méthodes k-means et de CAH, 
#' au tableau des variables quantitatives, mains avant de faire cela on doit rendre 
#'les variables centré réduit car ce dernier permettent d’éviter que les variables à
#'forte variance pèsent indûment sur les résultats .

data<-read.csv("dataParkinson.csv")
data<-data[,-c(1,2,3)]
data_cr<-scale(data,center=T,scale=T)
write.csv(data_cr, "data_centre_reduit.csv")
#'1.Pour differents N on applique le k-means au tableau des variables quantitatives jusqu'a on trouve
#'le N  le plus petit qui verfie que le taux d’inertie expliquée de la classification à N classes 
#'est supérieur à 0.95 pour notre cas est 243
#'et on remarque á partire de k=21 l’adjonction d’un groupe supplémentaire n’augmente pas significativement la part d’inertie expliquée par la partition


N=243
inertie.expl <- rep(0,times=N)
for (k in 2:N){
  clus <- kmeans(data_cr,centers=k,nstart=5)
  inertie.expl[k] <- clus$betweenss/clus$totss
}
inertie.expl
max(inertie.expl)
plot(1:N,inertie.expl,type="b",xlab="Nb. de groupes",ylab="% inertie explique")
#'2.De meme pour diffirents Nc (Nombre de classes á retenir ) on choisit celle qui 
#'verfie la régle (var(I2)/var(I))<0.05 I étant le vecteur de taille N des taux d’inertie expliquée 
#'et I2 étant le vecteur des (N-Nc) dernières valeurs des taux d’inertie expliquée,
#'on trouve que Nc est 60 donc le nombre de classes optimales pour notre cas est 60
Nc=60
var(inertie.expl[Nc:N])/(var(inertie.expl))

#'3.Application de CAH 
library(FactoMineR)
data_cr<-read.csv("data_centre_reduit.csv")

data_cr$X=NULL
Re<-HCPC(data_cr,nb.clust=-1,consol = TRUE)
#'3.1 le rapport de corrélation est une mesure d’association importante entre
#'une variable quantitative et une variable qualitative Alors plus que sa
#'valeur est plus proche de 1 plus que sa variable associée est plus corrélée avec la
#'variable classification.
#'Pour notre cas la variable plus correlée c'est NHR ,Shimmer ,Shimmer dB,Shimmer.APQ5 
#'la majorité des variables ont une correlation moyenne sauf les variables 
#'DFA, motor_UPDRS ,total_UPDRS, test_time qui ont une tréS faible correlation
#'
#'
#'3.2 Pour la premiére classe :
#'La variable qui caractérise cette classe  est la variable dont |v.test| est le plus grand.
#'dans notre cas c'est la variable HNR 
#'Les variables qui ont les p.value les plus faibles, présentent les observations les plus
#'concentrées à l’intérieur des classes,
#'On constate que tout les variables ont une p.value < 0.001 donc 
#'elles ont tous un degré d'importance Hautement significatives.
#'tous les variables sauf la vaiable HNR ont v.test<0 ce qui signfie que les individus  
#'de la classe 1, ont des valeurs des variables inférieures à la moyenne. Dans ce cas 
#'cette classe se caractérise par des valeurs faibles de ses variables.
#'On a les écarts types globales 0.999 = 1 grâce au centrage et à la réduction
#'des données.
#'
#'
#'Pour La deuxiéme classe :
#'La variable qui caractérise cette classe est la variable PPE car elle a le
#'|v.test| le plus grand.
#'On constate que tout les variables sauf test_time ont une p.value < 0.001 donc 
#'elles ont tous un degré d'importance Hautement significatives.
#'la variable PPE a le p.value plus petit donc c’est la variable 
#'qui présent les observations les plus concentrées à l’intérieur de la classe.
#'La variable HNR sa valeurs est inférieures à la moyenne car son v.test <0 
#'les autres variable ont v.test>0 donc leurs valeurs est supérieur á la moyenne.
#'
#'
#'Pour la troisiéme classe :
#'#'La variable qui caractérise cette classe est la variable NHR car elle a le
#'|v.test| le plus grand.
#'On constate que tout les variables sauf test_time ont une p.value < 0.001 donc 
#'elles ont tous un degré d'importance Hautement significatives.
#'la variable PPE a le p.value plus petit donc c’est la variable 
#'qui présent les observations les plus concentrées à l’intérieur de la classe.
#'Les variable HNR, test_time ont des valeurs inférieures à la moyenne car leurs v.test <0 
#'cependant les autres variable ont v.test>0 donc leurs valeurs est supérieur á la moyenne.
Re$desc.var

Re$data.clust

Re$desc.ind

#'3.3 Calcul du taux d'inertie


Iner_inter_av = Re$call$bw.before.consol #'inertie inter avant consol
Iner_inter_ap =Re$call$bw.after.consol  #'inertie inter aprés consol
Re$call$t$within  #'intra
Iner_Total= Re$call$t$within[59] + Re$call$bw.before.consol #'inertie totale
Iner_Total
taux_inertie_av = Iner_inter_av/(Re$call$t$within[59] + Re$call$bw.before.consol)
taux_inertie_av
taux_inertie_ap = Iner_inter_ap/(Re$call$t$within[59] + Re$call$bw.after.consol )
taux_inertie_ap

#'4.comparaison des classifications faites par k-means et CAH 
#'l’algorithme des K-means est un algorithme qui permet de trouver des classes dans des
#'données, qui n’entretiennent jamais de relations hiérarchiques : une classe n’est
#'jamais incluse dans une autre classe. Cet Algorithme fonctionne en précisant le
#'nombre de classes attendues par le calcule des distances Intra-Classe et InterClasse. 
#'Mais cet algorithme ne trouve pas nécessairement la configuration la plus
#'optimale correspondant à la fonction objective minimale . 
#'Cependant, CAH est un algorithme qui fournit une hiérarchie de partitions :
#'arbre contenant l’historique de la classification et permettant de
#'retrouver n-1 partitions, il Crée à chaque étape une partition obtenue en agrégeant
#'2 à 2 les individus les plus proches ainsi il nécessite de fixer une règle pour agréger
#'un individu et un groupe d’individus.de surcroît, il permet de choisir le nombre de
#'classes de façon optimale, grâce à des indicateurs de qualité de la classification en
#'fonction du nombre de classes 

