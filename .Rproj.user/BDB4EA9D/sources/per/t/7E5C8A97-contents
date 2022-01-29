library(lmtest)
library(stats)

data = read.csv(file = "dataParkinson.csv")


#' Calculer le modèle de régression linéaire multiple incluant toute les variables explicatives
model = lm(total_UPDRS~., data = data)

#' Existe-t-il des variables explicatives non significatives ?


#' Donner la valeur de R2 et R2_adjusted 
summary <- summary(model)
summary$r.squared
summary$adj.r.squared

#' Le test de Fisher est-il significatif ? Que signifie ce test ?
summary

#' Dans la derniere ligne de summary on trouve :
#' F-statistic:   443 on 21 and 888 DF,  p-value: < 2.2e-16
#' on constate que le test Fisher est significatif puisque p-value: < 2.2e-16

#' Améliorer le modèle initiale par la procédure step, que remarquez-vous ?
step(model)
step_model = lm(total_UPDRS ~ subject. + age + sex + test_time + motor_UPDRS + 
                  Jitter... + Jitter.RAP + Shimmer + Shimmer.APQ5 + Shimmer.APQ11 + 
                  NHR + HNR + DFA, data = data)

#' test d’homoscédasticité
plot(resid(step_model), predict(step_model), xlab="résidu", ylab = "valeur ajustée")

#' test de normalité (shapiro et ks)
shapiro.test(resid(step_model))
ks.test(resid(step_model), pnorm)

#' recherche de valeurs aberrantes
seqx = seq(1,dim(data)[1],length=dim(data)[1])
sd = sqrt(deviance(step_model)/df.residual(step_model))
abr = abs(data$total_UPDRS-predict(step_model))/sd
plot(seqx,abr)
abline(h=2, lty=2,col=2)


#' Procédure de séléction basée sur le résidu

cols = names(data)[! names(data) %in% c('total_UPDRS')]
X = data[cols]


r <- data.frame(t(rep(0, ncol(X))),stringsAsFactors = TRUE)
names(r) <- names(X)

#'Calcul des corrélation

for (i in 1:length(cols)) {
  r[i] = cor(data$total_UPDRS, X[,i])
}

ro = max(abs(r))

df = nrow(data)-2
t = ro/sqrt((1-ro^2)/df)
2*(1-pt(t,df))


#' la variable motor_UPDRS est la plus corrélée avec le résidu

r <- data.frame(t(rep(0, ncol(X))),stringsAsFactors = TRUE)
names(r) <- names(X)

e <-resid(lm(data$total_UPDRS ~ data$motor_UPDRS))

for (i in 1:length(cols)) {
  r[i] = cor(e, X[,i])
}

ro = max(abs(r))

df = nrow(data)-2
t = ro/sqrt((1-ro^2)/df)
2*(1-pt(t,df))

#' la variable sex est la plus corrélée avec le résidu

r <- data.frame(t(rep(0, ncol(X))),stringsAsFactors = TRUE)
names(r) <- names(X)

e <-resid(lm(data$total_UPDRS ~ data$motor_UPDRS + data$sex))

for (i in 1:length(cols)) {
  r[i] = cor(e, X[,i])
}

ro = max(abs(r))

df = nrow(data)-2
t = ro/sqrt((1-ro^2)/df)
2*(1-pt(t,df))

#' la variable age est la plus corrélée avec le résidu

r <- data.frame(t(rep(0, ncol(X))),stringsAsFactors = TRUE)
names(r) <- names(X)

e <-resid(lm(data$total_UPDRS ~ data$motor_UPDRS + data$sex + data$age))

for (i in 1:length(cols)) {
  r[i] = cor(e, X[,i])
}

ro = max(abs(r))

df = nrow(data)-2
t = ro/sqrt((1-ro^2)/df)
2*(1-pt(t,df))

#' la variable subject. est la plus corrélée avec le résidu

r <- data.frame(t(rep(0, ncol(X))),stringsAsFactors = TRUE)
names(r) <- names(X)

e <-resid(lm(data$total_UPDRS ~ data$motor_UPDRS + data$sex + data$age + data$subject.))

for (i in 1:length(cols)) {
  r[i] = cor(e, X[,i])
}

ro = max(abs(r))

df = nrow(data)-2
t = ro/sqrt((1-ro^2)/df)
2*(1-pt(t,df))

#' la variable DFA est la plus corrélée avec le résidu

r <- data.frame(t(rep(0, ncol(X))),stringsAsFactors = TRUE)
names(r) <- names(X)

e <-resid(lm(data$total_UPDRS ~ data$motor_UPDRS + data$sex + 
             data$age + data$subject. + data$DFA))

for (i in 1:length(cols)) {
  r[i] = cor(e, X[,i])
}

ro = max(abs(r))

df = nrow(data)-2
t = ro/sqrt((1-ro^2)/df)
2*(1-pt(t,df))

#' la variable HNR est la plus corrélée avec le résidu

r <- data.frame(t(rep(0, ncol(X))),stringsAsFactors = TRUE)
names(r) <- names(X)

e <-resid(lm(data$total_UPDRS ~ data$motor_UPDRS + data$sex + 
             data$age + data$subject. + data$DFA + data$HNR))

for (i in 1:length(cols)) {
  r[i] = cor(e, X[,i])
}

ro = max(abs(r))

df = nrow(data)-2
t = ro/sqrt((1-ro^2)/df)
2*(1-pt(t,df))

#' la variable Shimmer.APQ11 est la plus corrélée avec le résidu


r <- data.frame(t(rep(0, ncol(X))),stringsAsFactors = TRUE)
names(r) <- names(X)

e <-resid(lm(data$total_UPDRS ~ data$motor_UPDRS + data$sex + data$age + 
             data$subject. + data$DFA + data$HNR + data$Shimmer.APQ11))

for (i in 1:length(cols)) {
  r[i] = cor(e, X[,i])
}

ro = max(abs(r))

df = nrow(data)-2
t = ro/sqrt((1-ro^2)/df)
2*(1-pt(t,df))

#' la variable test_time est la plus corrélée avec le résidu

r <- data.frame(t(rep(0, ncol(X))),stringsAsFactors = TRUE)
names(r) <- names(X)

e <-resid(lm(data$total_UPDRS ~ data$motor_UPDRS + data$sex + data$age +
               data$subject. + data$DFA + data$HNR + data$Shimmer.APQ11 + data$test_time))

for (i in 1:length(cols)) {
  r[i] = cor(e, X[,i])
}

ro = max(abs(r))

df = nrow(data)-2
t = ro/sqrt((1-ro^2)/df)
2*(1-pt(t,df))

#' la variable NHR est la plus corrélée avec le résidu

r <- data.frame(t(rep(0, ncol(X))),stringsAsFactors = TRUE)
names(r) <- names(X)

e <-resid(lm(data$total_UPDRS ~ data$motor_UPDRS + data$sex + data$age +
               data$subject. + data$DFA + data$HNR + data$Shimmer.APQ11 +
               data$test_time + data$NHR))

for (i in 1:length(cols)) {
  r[i] = cor(e, X[,i])
}

ro = max(abs(r))

df = nrow(data)-2
t = ro/sqrt((1-ro^2)/df)
2*(1-pt(t,df))

#' la variable Jitter.RAP est la plus corrélée avec le résidu

r <- data.frame(t(rep(0, ncol(X))),stringsAsFactors = TRUE)
names(r) <- names(X)

e <-resid(lm(data$total_UPDRS ~ data$motor_UPDRS + data$sex + data$age +
               data$subject. + data$DFA + data$HNR + data$Shimmer.APQ11 +
               data$test_time + data$NHR + data$Jitter.RAP))

for (i in 1:length(cols)) {
  r[i] = cor(e, X[,i])
}

ro = max(abs(r))

df = nrow(data)-2
t = ro/sqrt((1-ro^2)/df)
2*(1-pt(t,df))

#' la variable PPE est la plus corrélée avec le résidu

r <- data.frame(t(rep(0, ncol(X))),stringsAsFactors = TRUE)
names(r) <- names(X)

e <-resid(lm(data$total_UPDRS ~ data$motor_UPDRS + data$sex + data$age +
               data$subject. + data$DFA + data$HNR + data$Shimmer.APQ11 +
               data$test_time + data$NHR + data$Jitter.RAP + data$PPE))

for (i in 1:length(cols)) {
  r[i] = cor(e, X[,i])
}

ro = max(abs(r))

df = nrow(data)-2
t = ro/sqrt((1-ro^2)/df)
2*(1-pt(t,df))

#'Le coeficient de corrélation est non significatif car elle a dépasser le 5%

resid_model = lm(data$total_UPDRS ~ data$motor_UPDRS + data$sex + data$age +
                 data$subject. + data$DFA + data$HNR + data$Shimmer.APQ11 +
                 data$test_time + data$NHR + data$Jitter.RAP + data$PPE)

bptest(resid_model)


plot(predict(resid_model), resid(resid_model))
abline(h=0)


shapiro.test(data$total_UPDRS)
ks.test(resid(resid_model), pnorm)


AIC(resid_model)
AIC(step_model)
