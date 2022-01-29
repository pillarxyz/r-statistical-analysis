#' --- 
#' title: "ACM"
#' author: "Ayman Lafaz, Ilyas Sekkaoui" 
#' fig_width: 10
#' fig_height: 10
#' --- 

library(FactoMineR)
library(ade4)
library(corrplot)
library(factoextra)

data<-read.csv("dataParkinson.csv")

data <- subset(data, select = -c(age, sex))

#' Discrétisation
data_disc = data.frame(matrix(ncol = ncol(data), nrow = nrow(data)))
for (k in 1:ncol(data)){
  clus <- kmeans(data[,k],centers=2,nstart=5)
  vec = c()
  for(i in 1:nrow(data)){
    if(clus$cluster[i] == 1)
    {
      vec = append(vec, paste(colnames(data[,k]), "0", sep=""))
    }
    else if(clus$cluster[i] == 2)
    {
      vec = append(vec, paste(colnames(data[,k]), "1", sep=""))
    }
  }
  data_disc[paste("X", as.character(k), sep = "")] = vec
  #'calculer les taux de discrétisation
  print((clus$betweenss/clus$totss)*100)
}
colnames(data_disc) = colnames(data)
rm(data)

#' Construction du tableau disjonctif complet
K <- acm.disjonctif(subset(data_disc))
mod = apply(data_disc, 2, function(i) levels(factor(i)))
modlist = c()
for(i in 1:ncol(mod)){
  for(j in 1:2)
  {
    modlist = append(modlist, mod[j,i])
  }
}

#'afficher le tableau disjonctif complet
head(K)

#'les proportions de chaque modalité
apply(K,2,sum) 

#' Existe t-il des modalités de faibles fréquences ?
frqmod=apply(K,2,sum)/(nrow(K))
frqmod
subset(frqmod, frqmod < 0.01)
K = subset(K, select = -c(Jitter.RAP.0, Jitter.PPQ5.0, Jitter.DDP.0))

#'NCP = nbr de composantes sur les quelles on va faire les calculs
res<-MCA(data_disc,ncp=10,graph=F,axes=c(1,2)) 

#'nombre de colonnes = modalité
p = ncol(K)
p

#'nombre de lignes = individus
n = nrow(data_disc)
n

#'nombre de variables
s = ncol(data_disc)
s 

X <- res$ind$coord[,1:2] 
Y <- res$var$coord[,1:2] 
d <- sqrt(res$eig[1:2,1])
Xstand <- X %*% diag(1/d)
Ystand <- Y %*% diag(1/d)  

xlim <- c(min(c(min(Xstand[,1]),min(Ystand[,1]))),max(c(max(Xstand[,1]),max(Ystand[,1]))))+0.2 
ylim <- c(min(c(min(Xstand[,2]),min(Ystand[,2]))),max(c(max(Xstand[,2]),max(Ystand[,2]))))+0.2 


#'individus au barycentre des modalites
plot(X,pch=16,xlim=xlim,ylim=ylim,main="individus au barycentre des modalites")  
points(Ystand,pch=17,col=2) #'les points standardisee
text(Ystand,rownames(Y),pos=4,col=2)

#'modalites au barycentre des individus
plot(Y,pch=16,xlim=xlim,ylim=ylim,main="modalites au barycentre des individus") 
text(Y,rownames(Y),pos=4) 
points(Xstand,pch=17,col=2) 
text(Xstand,rownames(X),pos=4,col=2)

rm(X, Y, d, Xstand, Ystand)

#'Représentation simultannée
plot(res,title="representation simultanee")

#'Calcul des valeurs propres
res$eig
plot(1:nrow(res$eig), res$eig[,1],type="b",main="Scree plot", xlab = "indice", ylab = "valeurs propres")

mean(res$eig[,1])

#'La règle empirique permet de retounir les valeurs propres qui sont
#'supérieurs à la moyenne des valeurs propres c'est la dim = 3 dans ce cas.
#'dim = 3
var(res$eig[4:20,1])*10/((var(res$eig[,1]))*19)
#' si cette val < 0.05 on prend dim = 3 sinon on augmente la dimension
dim = 3

#'nombre de valeurs propres non nulles
min(n-1,p-s)

#'Inertie des modalités
(p-s)/s
sum(res$eig[,1])

#' Nuage des Modalités

#' Cos2 des modalités sur le sous espace
cos.mod <- res$var$cos2[,1:dim]
cos.mod

corrplot(res$var$cos2[,1:dim], is.corr=FALSE)
cos.mod <- rowSums(cos.mod)

#' Les modalités bien représentées ayant un cos2 > 0.75
which(cos.mod>=0.75)

#' Les modalités moyennement  représentées ayant un 0.25 < cos2 < 0.75
which(cos.mod<0.75 & cos.mod>=0.25)

#' Les modalités faiblement  représentées ayant un cos2 < 0.25
which(cos.mod<0.25)

#' Contribution des modalités dans chaque axe du sous espace
cont.mod<-res$var$contrib[,1:dim]
cont.mod
write.table(cont.mod, "contribution_var.csv", sep=";", col.names=TRUE, dec=',', row.names=TRUE)

#' CAH au tableau des contributions des individus
res.hcpc.mod<-HCPC(as.data.frame(cont.mod), graph = FALSE)
res.hcpc.mod$desc.var

res.hcpc.mod$desc.var
res.hcpc.mod$data.clust

#' Nuage des modalités sur les 2 premiers axes (1er plan factoriel)
plot(res, 
     invisible = c("ind", "quali.sup", "quanti.sup"),
     cex = 0.8,
     autoLab = "yes")


#' Nuage des individus

#' Cos2 des individus sur le sous espace
head(res$ind$cos2[,1:dim])
fviz_cos2(res, choice = "ind", axes = 1:dim)

#' Les individus bien représentées ayant un cos2 > 0.75
cos.ind <- rowSums(res$ind$cos2[,1:dim])
which(cos.ind >= 0.75)

#' Les individus moyennement représentées ayant un 0.25 < cos2 < 0.75
which(cos.ind < 0.75 & cos.ind >= 0.25)

#' Les individus faiblement  représentées ayant un cos2 < 0.25
which(cos.ind < 0.25)

#' Contribution des individus dans chaque axe du sous espace
cont.ind <- res$ind$contrib[,1:dim]
head(cont.ind)

#' CAH au tableau des contributions des individus
res.hcpc.ind <- HCPC(as.data.frame(cont.ind), graph = FALSE)
res.hcpc.ind$desc.var

#'#' Nuage des variables

#' Calcul des coefficients de corrélation des variables avec les projections sur les axes
res$var$eta2
#' Graphique des coefficients de corrélation des variables avec les facteurs du 1er plan factoriel
plot(res$var$eta2)  

