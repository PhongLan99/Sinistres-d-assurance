#Partie 1

#1

don<-source("C:/Users/pljea/Documents/Cours/L3 Miashs/S6/MI-Statistiques S6/Memoire/assurance.R")
ls()
summary(dat)

Atyph<-as.factor(dat$Atyph)
addmargins(table(dat$Atyph))

Acompm<-as.factor(dat$Acompm)
addmargins(table(dat$Acompm))

summary(dat$Police1)

summary(dat$Sin1)
summary(dat$Sin2)

std(dat$Sin1)
std(dat$Sin2)

#2 Histogrammes
Sin1<-as.numeric(dat$Sin1)
Sin2<-as.numeric(dat$Sin2)

ks.test(Sin1,Sin2)
ks.test(Sin1, "pnorm")
ks.test(Sin2, "pnorm")

mean(Sin1)
mean(Sin2)
sd(Sin1)
sd(Sin2)

par(mfrow=c(1,2))
hist(Sin1, freq=FALSE, main="Sinistres 1", xlab="Coût en euros", ylab="Fréquence")
lines(density(Sin1, bw="nrd0", adjust=1, kernel="gaussian"), lwd = 2, col = "blue")
curve(dnorm(x,mean=11.96042,sd=2.305368),add=TRUE, lwd = 2, col = "red")
hist(Sin2, freq=FALSE, main="Sinistres 2", xlab="Coût en euros", ylab="Fréquence")
lines(density(Sin2, bw="nrd0", adjust=1, kernel="gaussian"), lwd = 2, col = "blue")
curve(dnorm(x,mean=8.640428,sd=2.245154),add=TRUE, lwd = 2, col = "red")

#3
Ahabi<-as.factor(dat$Ahabi)
pcs<-as.factor(dat$pcs)
chisq.test(Ahabi,pcs,p=0.01)

qchisq(0.99,28)


#4
chisq.test(Sin1,Sin2,p=0.05)

ecart<-Sin1-Sin2
ecart
mean(ecart)
ecart_moy<-abs(mean(Sin1)-mean(Sin2))
ecart_moy

wilcox.test(ecart, alternative = "greater",
            mu = 3.0, paired = FALSE)
wilcox.test(ecart, alternative = "less",
            mu = 3.5, paired = FALSE)

t.test(ecart,alternative = "greater",mu=3.0)
t.test(ecart,alternative = "less",mu=3.5)

#Partie 2
#1
RUC<-as.numeric(dat$RUC)
X<-log(RUC)
Y<-Sin1
m1 <- lm(Y ~ X)
summary(m1)

op <- par(mfrow=c(2,2))
plot(m1)
par(op)

acf(residuals(m1), main="m1")

m1$coef
b1<-cov(X,Y)/var(X)
b0<-mean(Y)-b1*mean(X)
b0;b1

qqnorm(m1$residuals,col="blue4",main="Droite de Henry") 
qqline(m1$residuals,lwd=2,col="violetred")

plot(m1$fitted,abs(rstudent(m1)),main="Nuage des résidus normalisés en valeur absolue 
sur les valeurs ajustées",
xlab="Valeurs ajustées",ylab="Résidus normalisés en valeur absolue")
lines(lowess(m1$fitted,abs(rstudent(m1)),f=0.5),lwd=2,col=2)
abline(h=2,col="red")

library(lmtest)
bptest(Y ~ X)

#2
plot(Y~X, pch=20, col="blue", xlim=c(0,28), ylim=c(3,20), yaxs="i",xaxs="i",
xlab="log du revenu par unité de consommation du ménage", ylab="montant dommage en euros pour les sinistres de type 1")
points(mean(X),mean(Y), pch=16, col="red")
text(mean(X),mean(Y), "G", pos=3, col="red")
title("Nuage de points de Sin1 en fonction de log(RUC)")

abline(lm(Y~X), col="red", lwd=2)
text(0,m1$coef[1], bquote(hat(beta[0])), col="red", pos=2, xpd=T)
text(min(X),max(Y),bquote(paste("droite de régression estimée : ",
hat(y)==.(round(m1$coef[1],3))+.(round(m1$coef[2],3)) ~ x)), adj=0, col="red")

library(ggplot2)
ggplot(dat) +               
  aes(x = log(RUC), y = Sin1) +    
  ggtitle("Données et droite de régression")+
  geom_point(color="violetred") +                 
  geom_smooth(colour="red4", method="lm", fill="red4") +  
  geom_smooth(method = "lm", formula = y ~ x, col = "red")
 

#3

pcs<-as.factor(dat$pcs)
addmargins(table(pcs))
m2 <- lm(Y ~ X + pcs)
summary(m2)
anova(m2) 
anova(m1,m2)

table(pcs)
chisq.test(X,pcs,p=0.05)

op <- par(mfrow=c(2,2))
plot(m2)
par(op)

library(lmtest)
bptest(Y ~ X + pcs)

plot(m2$fitted,abs(rstudent(m2)),main="Nuage des résidus normalisés en valeur absolue 
sur les valeurs ajustées",
xlab="Valeurs ajustées",ylab="Résidus normalisés en valeur absolue")
lines(lowess(m2$fitted,abs(rstudent(m2)),f=0.5),lwd=2,col=2)
abline(h=2,col="red")

#4
m3 <- lm(Y ~ pcs)
summary(m3)
anova(m1,m3,m2)

op <- par(mfrow=c(2,2))
plot(m3)
par(op)

plot(m3$fitted,abs(rstudent(m3)),main="Nuage des résidus normalisés en valeur absolue 
sur les valeurs ajustées",
xlab="Valeurs ajustées",ylab="Résidus normalisés en valeur absolue")
lines(lowess(m3$fitted,abs(rstudent(m3)),f=0.5),lwd=2,col=2)
abline(h=2,col="red")

library(lmtest)
bptest(Y ~ pcs)

#Partie 3
#1
newdat<-dat[- c(5343:5352),]
fix(newdat)
dat0<-dat[c(5343:5352),]
fix(dat0)

Sin1<-as.numeric(newdat$Sin1)
RUC<-as.numeric(newdat$RUC)
Atyph<-as.factor(newdat$Atyph)
Acompm<-as.factor(newdat$Acompm)
nbpers<-as.factor(newdat$nbpers)
Nbadulte<-as.factor(newdat$Nbadulte)
cs<-as.factor(newdat$cs)
habi<-as.factor(newdat$habi)


library(MASS)
stepAIC(m4)

library(Rcmdr)
m4<-lm(formula = Sin1 ~ log(RUC) + Acompm + Atyph + cs + habi + Nbadulte + 
       nbpers + pcs, data = newdat)
summary(m4)

op <- par(mfrow=c(2,2))
plot(m4)
par(op)

plot(m4$fitted,abs(rstudent(m4)),main="Nuage des résidus normalisés en valeur absolue 
sur les valeurs ajustées",
xlab="Valeurs ajustées",ylab="Résidus normalisés en valeur absolue")
lines(lowess(m4$fitted,abs(rstudent(m4)),f=0.5),lwd=2,col=2)
abline(h=2,col="red")

library(lmtest)
bptest(Sin1 ~ log(RUC) + Acompm + Atyph + cs + habi + Nbadulte + 

       nbpers + pcs, data = newdat)

library(dplyr)
pc<-predict(m4,dat0, level = 0.95, interval = "confidence")
pc<-pc[,1]
pc<-as.data.frame(pc)
montantobs<-dat0[,17]
montantobs<-as.data.frame(montantobs)
tab<-bind_cols(pc, montantobs, id = NULL)
colnames(tab)=c("Valeur prédite","Montant observé")
tab

#Partie 4

#2
a<- -0.5
b<-1
alpha<-1/5
beta<-2/5
n<-100
mb <- matrix(NA, 100, 4)

for(i in 1:n)
{
eta<-rnorm(1,mean=0,sd=1)
X<-rlnorm(1,meanlog=2,sdlog=1)
Z<-exp(rnorm(1,mean=0,sd=1))
dirac<-rbinom(1,1,pnorm(a+b*Z))

if(dirac==0)
{
Y<-0
}

else
{
Y<-alpha+beta*X+eta
}

mb[i,1]<-dirac
mb[i,2]<-Y
mb[i,3]<-X
mb[i,4]<-Z
}
mb

selection<-mb[,1]
montant<-mb[,2]
prix<-mb[,3]
vitesse<-mb[,4]

df<-data.frame(montant,prix)
df <- subset(df, montant > 0)
df

montantpos<-df[,1]
montantpos
prix2<-df[,2]
prix2

don <- data.frame(selection,montant,prix,vitesse)
cor(don)
plot(montant ~ prix, don, main="Données et droites de régression, n=100") 
abline(alpha,beta,lwd = 2, col = "royalblue3") 
abline(lm(montant ~ prix, don),lwd = 2, col = "green")
abline(lm(montantpos ~ prix2, don),lwd = 2, col = "violetred")
legend(x="bottomright", legend=c("Droite y=1/5+(2/5)x", 
"Droite de régression des Yi sur Xi", 
"Droite de régression des Yi positifs sur Xi correspondants"),
col=c("royalblue3","green","violetred"), lwd=c(2,2,2), lty=c(1,1,1), cex=0.75)

m4 <- lm(montant ~ prix)
summary(m4)


library(ggplot2)
ggplot(don, aes(d=selection, x = prix, y = montant, z=vitesse)) +     # on s'intéresse aux 2 variables carat et price
    ggtitle("Données et droites de régression")+
    geom_point(color="violetred") +  # on représente un nuage de points de ces 2 variables
    geom_abline(intercept = alpha, slope=beta, lwd=1, col = "blue")+                  
    geom_smooth(colour="red4", method="lm", fill="red4") +  # on représente l’intervalle de confiance à 95% de la pente
    geom_smooth(method = "lm", formula = y ~ x, col = "red")+ # on ajoute une droite de régression linéaire

ggplot(data = df, aes(y = prix2, x = montantpos))+  
    geom_point(color="violetred") +                      
    geom_smooth(method = "lm", formula = y ~ x, col = "green") +     
    geom_smooth(colour="green4", method="lm", fill="green4")


a<- -0.5
b<-1
alpha<-1/5
beta<-2/5
n<-1000
mb <- matrix(NA, 1000, 4)

for(i in 1:n)
{
eta<-rnorm(1,mean=0,sd=1)
X<-rlnorm(1,meanlog=2,sdlog=1)
Z<-exp(rnorm(1,mean=0,sd=1))
dirac<-rbinom(1,1,pnorm(a+b*Z))

if(dirac==0)
{
Y<-0
}

else
{
Y<-alpha+beta*X+eta
}

mb[i,1]<-dirac
mb[i,2]<-Y
mb[i,3]<-X
mb[i,4]<-Z
}
mb

selection<-mb[,1]
montant<-mb[,2]
prix<-mb[,3]
vitesse<-mb[,4]

df<-data.frame(montant,prix)
df <- subset(df, montant > 0)
df

montantpos<-df[,1]
prix2<-df[,2]

cor(don)
plot(montant ~ prix, don, main="Données et droites de régression, n=1000") 
abline(alpha,beta,lwd = 2, col = "royalblue3") 
abline(lm(montant ~ prix, don),lwd = 2, col = "green")
abline(lm(montantpos ~ prix2, don),lwd = 2, col = "violetred")
legend(x="bottomright", legend=c("Droite y=1/5+(2/5)x", 
"Droite de régression des Yi sur Xi", 
"Droite de régression des Yi positifs sur Xi correspondants"),
col=c("royalblue3","green","violetred"), lwd=c(2,2,2), lty=c(1,1,1), cex=0.75)

m5<-lm(formula = montant ~ prix)
plot(m5$fitted,abs(rstudent(m5)),main="Nuage des résidus normalisés en valeur absolue 
sur les valeurs ajustées",
xlab="Valeurs ajustées",ylab="Résidus normalisés en valeur absolue")
lines(lowess(m5$fitted,abs(rstudent(m5)),f=0.5),lwd=2,col=2)
abline(h=2,col="red")

op <- par(mfrow=c(2,2))
plot(m5)
par(op)


m6<-lm(formula = montantpos ~ prix2)
plot(m6$fitted,abs(rstudent(m6)),main="Nuage des résidus normalisés en valeur absolue 
sur les valeurs ajustées",
xlab="Valeurs ajustées",ylab="Résidus normalisés en valeur absolue")
lines(lowess(m6$fitted,abs(rstudent(m6)),f=0.5),lwd=2,col=2)
abline(h=2,col="red")

op <- par(mfrow=c(2,2))
plot(m6)
par(op)

#3
a<- -0.5
b<-1
alpha<-1/5
beta<-2/5
n<-100
N<-999
donnees<-NULL

for(j in 1:N)
{

mb <- matrix(NA, 100, 4)
mb<-as.data.frame(mb)

for(i in 1:n)
{
eta<-rnorm(1,mean=0,sd=1)
X<-rlnorm(1,meanlog=2,sdlog=1)
Z<-exp(rnorm(1,mean=0,sd=1))
dirac<-rbinom(1,1,pnorm(a+b*Z))

if(dirac==0)
{
Y<-0
}

else
{
Y<-alpha+beta*X+eta
}

mb[i,1]<-dirac
mb[i,2]<-Y
mb[i,3]<-X
mb[i,4]<-Z

}

donnees<-rbind(donnees,mb)
}
colnames(donnees)=c("Dirac","Y","X","Z")
donnees

out<-split(donnees, factor(sort(rank(row.names(donnees))%%N)))
out[[999]]



selection<-mb[,1]
montant<-mb[,2]
prix<-mb[,3]
vitesse<-mb[,4]



vcov(m4)

library(plyr)

var_emco<-NULL

v <- matrix(NA, 3, 2)
v<-as.data.frame(v)

somme_X<-0
somme_Y<-0
num<-0
den<-0

somme_X<-(somme_X)+X
somme_Y<-(somme_Y)+Y
num<-num+X*(Y-(1/n)*somme_Y)
den<-den+X*(X-(1/n)*somme_X)

betachap<-num/den
alphachap<-(1/n)*(somme_Y)-betachap*(1/n)*(somme_X)

v[i,1]<-var(alphachap)
v[i,2]<-var(betachap)

var_emco<-rbind(var_emco,v)

#4
a<- -0.5
b<-1
alpha<-1/5
beta<-2/5
n<-1000
mb <- matrix(NA, 1000, 4)

for(i in 1:n)
{
eta<-rnorm(1,mean=0,sd=1)
X<-rlnorm(1,meanlog=2,sdlog=1)
Z<-exp(rnorm(1,mean=0,sd=1))
dirac<-rbinom(1,1,pnorm(a+b*Z))

if(dirac==0)
{
Y<-0
}

else
{
Y<-alpha+beta*X+eta
}

mb[i,1]<-dirac
mb[i,2]<-Y
mb[i,3]<-X
mb[i,4]<-Z
}
mb

selection<-mb[,1]
montant<-mb[,2]
prix<-mb[,3]
vitesse<-mb[,4]

df<-data.frame(selection,montant,prix,vitesse)
mPROBIT<-glm(selection ~ vitesse, data=df, family=binomial(link=probit))
summary(mPROBIT)

library(ggplot2)
ggplot(data = df, aes(x = vitesse, y = selection))+  
    geom_point(color="violetred") +                      
    geom_smooth(method = "glm", formula = y ~ x, col = "royalblue3") +     
    geom_smooth(colour="royalblue3", method="glm", fill="royalblue3")

op <- par(mfrow=c(2,2))
plot(mPROBIT)
par(op)

