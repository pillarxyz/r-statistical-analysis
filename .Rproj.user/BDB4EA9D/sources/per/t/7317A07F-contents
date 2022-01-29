#' --- 
#' title: "ACP"
#' author: "Ayman Lafaz, Ilyas Sekkaoui" 
#' fig_width: 10
#' fig_height: 10
#' --- 

#'introduction :
#'Dans cette partie on va appliquer l'ACP dans notre tableau quantitaives ce dernier permet
#'d'identifier les variables qui sont corrélées positivement ou négativement et aussi
#'d'identifier les individus qui se ressemble ou différe 

data<-read.csv("dataParkinson.csv")
data<-data[,-c(1,2,3)]
library(FactoMineR)
#'1.Avant d'application ACP on fait le test de Bartlett qui teste l’homogénéité des variances, 
#'c’est un test qui se fait sur la matrice de corrélation, cette dernière ne doit
#'pas être singulière, et ne doit pas se présenter sous la forme d’une matrice identité
#'Alors ce test permet de signaler le cas où la matrice de corrélation s’écrit sous forme d’une matrice identité.

bartlett.test(data)  #'Test de sphericite de Bartlett
#'puisque p-value < 5% on accepte l’hypothèse que les variables ne sont
#'pas orthogonales, elles sont corrélées.ce qui confirme que la matrice de corrélation
#'diverge significativement de la matrice d’identité.

#'1.Application de l'ACP normé
res<-PCA(data,ncp=5,axes=c(1,2))
attributes(res)

#'On remarque que toutes les variables sont à l’intérieur d’un cercle unitaire, parce que après cette opération,
#'les variables se mettent à l’intérieur d’une sphère, et la projection d’une sphère
#'unitaire dans un plan de deux dimension est un cercle unitaire. de plus la distance
#'de toutes les variables après le centrage et la réduction est 1 donc la projection est
#'nécessairement inférieur à 1.
#'Plus que les variables est proche du cercle plus qu’elles sont bien présentées.
#'Dans notre cas, la majorité des variables sont bien présenté sauf test_time ,DFA,RPDE 
#'qui sont mal présentée par rapport aux autres.

#'2.Le principale avantage de la centration-réduction est de rendre comparables des
#'variables qui ne le seraient pas directement parce qu’elles ont des moyennes et/ou
#'des variances trop différentes.
#'Une autre raison pour appliquer le centrage et la réduction, c’est lorsqu’on tombe
#'face à des variables avec des unités différentes.

#'3.Calcul de l'indice KMO et des MSAi
library(psych)
KMO(cor(data))
#'L’indice KMO de est 0.9 peut être qualifié d’excellent. Il nous
#'indique que les corrélations entre les variables sont de bonne qualité.

#'4.Les valeurs propres, le pourcentage d’inertie 
#'de chaque valeur propre et le cumule des pourcentages d’inertie

res$eig

plot(1:ncol(data),res$eig[,1],type="b",ylab="Valeurs propres",xlab="Composantes",main="graphique des valeurs propres") #'Graphique des valeurs propres
sum(res$eig[,1])
barplot(res$eig[,1],main="Eigenvalues", names.arg=paste("dim",1:nrow(res$eig)))
#'La variation totale est répartie selon 19 valeurs propres.
#'les quatres premiers valeurs propres qui ont une valeur supérieur á 1 représentent plus de 7% de variation
#'les autres ont une variation inférierieur á 4%

#'6.dimension du sous-espace
inert.exp = c()
for(i in 1:ncol(data)){
  inert.exp = append(inert.exp, sum(res$eig[1:i,1]))
}
inert.exp = inert.exp/sum(res$eig[,1])

plot(1:ncol(data), inert.exp, type = "b", ylab="% inertie expliquee",xlab="Composantes")

var(inert.exp[5:ncol(data)])/(var(inert.exp))
#'d'aprés la régle de (Var(V2)/Var(V))<0.05 on garde seulement les 6 premiére valeurs propre
#'donc la dimension de de sous espace est 5 .


#'Nuages des variables

#'1.Calcule de cos2 sur le sous espace
#'on présente les valeurs de cos2 qui sont les valeurs de
#'la projection des variables sur les trois axes.
#'Autrement dit le cos2 est le coefficient de corrélation avec 
#'les coordonnées sont élevées au carrée

res$var$cos2[,1:5] #'calcule de cos2 sur le sous espace


print(t(apply(res$var$cos2[,1:5],1,cumsum)),digit=2) #'Cumul des cos2

#'8.	Distinguer les variables bien représentées, moyennement représentées 
#'et faiblement représentées sur le sous espace

#'Pour le premiére axe :
#'les variables bien représentées : jitter,jitter.Abs,Jitter.Rap,
#'Jitter.PPQ5,Jitter.DDP,Shimmer,SHimmer.dB,Shimmer.APQ3,Shimmer.APQ5,
#'Shimmer.APQ11,Shimmer.DDA,NHR,HNR,PPE
#'la variable moyennement représentée : RPDE
#'les variables faiblement représentées : test_time,DFA,motor_UPDRS,total_UPDRS

#'Pour le deuxiémes axe :
#'les variables bien représentées : motor_UPDRS,total_UPDRS
#'les variables moyennement représentées :aucun variables
#'les variables faiblement représentées : jitter,jitter.Abs,Jitter.Rap,
#'Jitter.PPQ5,Jitter.DDP,Shimmer,SHimmer.dB,Shimmer.APQ3,Shimmer.APQ5,
#'Shimmer.APQ11,Shimmer.DDA,NHR,HNR,PPE,DFA,RPDE,test_time

#'Pour le troisiéme axe :
#'les variables bien représentées : aucun variables.
#'les variables moyennement représentées :jitter,jitter.Abs,Jitter.Rap,
#'Jitter.PPQ5,Jitter.DDP,Shimmer,SHimmer.dB,Shimmer.APQ3,Shimmer.APQ5,
#'Shimmer.APQ11,Shimmer.DDA
#'les variables faiblement représentées : NHR,HNR,PPE,DFA,RPDE,motor_UPDRS,
#'total_UPDRS,test_time

#'Pour le quatriéme axe :
#'la variable bien représentée : DFA
#'les variables moyennement représentées :RPDE,HNR,PPE
#'les variables faiblement représentées : NHR,motor_UPDRS,
#'total_UPDRS,test_time,jitter,jitter.Abs,Jitter.Rap,
#'Jitter.PPQ5,Jitter.DDP,Shimmer,SHimmer.dB,Shimmer.APQ3,Shimmer.APQ5,
#'Shimmer.APQ11,Shimmer.DDA

#'Pour le cinquéme axe :
#'la variable bien représentée : test_time
#'les variables moyennement représentées :aucun variable
#'les variables faiblement représentées :motor_UPDRS,
#'total_UPDRS,jitter,jitter.Abs,Jitter.Rap,Jitter.PPQ5,
#'Jitter.DDP,Shimmer,SHimmer.dB,Shimmer.APQ3,Shimmer.APQ5,
#'Shimmer.APQ11,Shimmer.DDA,NHR,HNR,PPE,DFA,RPDE

library(corrplot)
corrplot(res$var$cos2[,1:5], is.corr=FALSE)

#' 9.calculer La contribution des variables dans chaque axe du sous espace
#'Le tableau ci-dessus exprime les valeurs de la contribution 
#'qui est tout simplement les valeurs de (cos2/valeur propre associé*100)
res$var$contrib[,1:5]

#'10.Application de CAH
cont<-res$var$contrib[,1:5]

inertie.expl <- rep(0,times=9)
for (k in 2:10){
  h = HCPC(as.data.frame(cont),nb.clust=k, graph = F, consol = TRUE)
  inertie.expl[k-1] <- h$call$bw.after.consol/(h$call$t$within[k] + h$call$bw.after.consol)
}
plot(1:9,inertie.expl,type="b",xlab="Nb. de groupes",ylab="% inertie explique")
#'On constate qu’à partir de 5 classes, l’ajout d’un groupe supplémentaire n’augmente 
#'pas significativement la part d’inertie expliquée par la partition. Donc
#'le nombre de classes de notre ensemble de données est 5.
Cat_var<-HCPC(as.data.frame(cont),nb.clust=5, graph = F, consol = TRUE)

Cat_var$desc.var #'lien entre la variable classification et les variables quantitatives

#'11.Le nuage des variables projete sur les 2 premiers axes
plot(res, choix="var", autoLab="yes") 
#'12.Indiquer les variables qui sont relativement bien corrélées 
#'(positivement et négativement) avec les axes du 1er plan factoriel 

#'tous les variables sont corrélées positivement avec le premiére axe 
#'sauf les variables HNR,test_time qui sont correlés négativement
#'les variables RPDE,PPE,jitter,jitter.Abs,Jitter.Rap,Jitter.DDP,
#'motor_UPDRS,total_UPDRS,test_time,NHR sont corrélées positivement 
#'avec le deuxiéme axe le reste corrélées négativement.

#'Nuage des individus

#'13.le calcule de cos2 pour les individus
res$ind$cos2[,1:5] #'calcule de cos2
#'

print(t(apply(res$ind$cos2,1,cumsum)),digit=2)#'afficher les cos2
res$ind$contrib[,1:5] #'la contribution des individus

#'14.Application CAH


cos2Ind<-res$ind$cos2[,1:5]
inertie.expl <- rep(0,times=19)
for (k in 2:20){
  h = HCPC(as.data.frame(cos2Ind),nb.clust=k, graph = F, consol = TRUE)
  inertie.expl[k-1] <- h$call$bw.after.consol/(h$call$t$within[k] + h$call$bw.after.consol)
}
print(inertie.expl)
plot(1:19,inertie.expl,type="b",xlab="Nb. de groupes",ylab="% inertie explique")
#'On constate qu’à partir de 14 classes, l’ajout d’un groupe supplémentaire n’augmente 
#'pas significativement la part d’inertie expliquée par la partition. Donc
#'le nombre de classes de notre ensemble de données est 14.
#'on applique donc k means á 14 classes
cat2<-kmeans(cos2Ind,centers=14,nstart=5)
cat2


#'le nuage des individus projete sur les 2 premiers axes
plot(res, choix="ind", autoLab="yes")

#'conclusion :

#'Dans un premier temps résumons l’analyse en composantes principales (ACP)
#'à l’aide des étapes :
#'1.La première étape concerne la mise en forme des données brutes
#'2.La deuxième étape consiste à centrer et réduire les données.Elles sont centrées 
#'afin d’obtenir des propriétés intéressantes, et réduites pour uniformiser les unités de mesure
#'3.Les individus sont représentés dans un espace à D dimensions. nous nous
#'intéressons aux distances inter-individuelles qui déterminent les ressemblances
#'4.Les individus sont représentés dans un espace à D dimensions. nous nous
#'intéressons aux distances inter-individuelles qui déterminent les ressemblances
#'5.Les individus sont représentés dans un espace à D dimensions. nous nous
#'intéressons aux distances inter-individuelles qui déterminent les ressemblances
#'6.Cette étape consiste à projeter les points du nuage des individus sur le
#'premier plan factoriel. Les coordonnées représentent les coefficients de 
#'corrélation avec les facteurs sur les individus

#'donc L’ACP est donc une méthode puissante pour synthétiser et résumer de vastes
#'populations décrites par plusieurs variables quantitatives. Elle permet entre autre
#'de dégager de grandes catégories d’individus et de réaliser un bilan des liaisons
#'entre les variables.