###################################################################################
#                         Bermeo Fernández Luis Antonio                           #
#                             Econometría Aplicada                                #
#                                     Tarea 3                                     #
#              Machine Learning methods for forecasting employment              #
###################################################################################

#Paquetes que necesitamos

install.packages("haven")
library(haven)

install.packages("readxl")
library(readxl)

install.packages("glmnet")
library(Matrix)
library(glmnet)

install.packages("texreg")
install.packages("broom")
library(texreg)
library(broom)

install.packages("xtable")
library(xtable)

install.packages("ggplot2")
library(ggplot2)

install.packages("stargazer")
library(stargazer)

#Generamos matrix de regresión y regresando

empleo <- as.data.frame(read_excel("empleo.xlsx"))

empleoP<-empleo

empleo<-empleo[-63,]
empleoP$asegurados[63]<-0

x<-model.matrix(asegurados~.,empleo)[,-1]
y=empleo$asegurados

#Para hacer Cross-Validation dividimos la muestra en train y test

set.seed(1)
train=sample(c(TRUE,FALSE), nrow(x), rep(TRUE))
test=(!train)

#Implementación del LASSO usando todas las variables

grid=10^seq(10,-2,length.out = 100)

lasso.mod=glmnet(x[train,],y[train],alpha = 1, lambda=grid, scale=TRUE)

plot(lasso.mod, xlim=c(0,5000000),ylim=c(-1000000,1000000)) 

set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=1)
plot(cv.out)
bestlam<-cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam, newx=x[test,])
y.test<-y[test]
lasso<-mean((lasso.pred-y.test)^2)

out=glmnet(x,y,alpha=1,lambda = grid)
lasso.best=glmnet(x,y,alpha = 1, lambda = bestlam)
lasso.coef=predict(out,type="coefficients", s=bestlam)[1:27,]
lasso.coef<-lasso.coef[lasso.coef!=0]

lasso.coef<-as.matrix(lasso.coef)

xtable(lasso.coef)

#Realizamos la predicción con los datos que contamos

px<-model.matrix(asegurados~.,empleoP)[,-1]

empleo10_2020<-predict(lasso.mod,s=bestlam, newx=px)

empleo10_2020[63]

#Graficamos la tendencia y los datos 

y<-c(y,NA)

p<-as.data.frame(cbind(y,empleo10_2020))

ggplot(p,aes(x=seq(as.Date("2015/08/1"), by = "month", length.out = length(y)), y=valor, colour=variable))+
  geom_line(aes(y=`1`, col="modelo ajustado")) + geom_point(aes(y=`y`, col="Obervaciones"))+
  xlab("tiempo en meses")+ylab("empleados asegurados")

#Principal Components Regression

install.packages("pacman")
pacman::p_load(pls)
library(pls)

y<-y[-63]

set.seed(1)
pcr.fit=pcr(asegurados~., data = empleo, scale = TRUE, validation="CV")

summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP")


set.seed(1)
pcr.fit=pcr(asegurados~., data = empleo, subset=train , scale = TRUE, validation="CV")
validationplot(pcr.fit, val.type = "MSEP")
summary(pcr.fit)

#usando full data y subset train se puede ver que 
#8 componentes es una buena elección del numero de componentes

pcr.test=predict(pcr.fit, x[test,],ncomp=8)
pcr<-mean((pcr.test-y.test)^2)

#predicción usando datos

pcr.pred=predict(pcr.fit, px,ncomp=8)

#Graficamos la tendencia y los datos

#tendencia

y<-c(y,NA)

p<-as.data.frame(cbind(y,pcr.pred))

ggplot(p,aes(x=seq(as.Date("2015/08/1"), by = "month", length.out = length(y)), y=valor, colour=variable))+
  geom_line(aes(y=p$pcr.pred, col="modelo ajustado")) + geom_point(aes(y=`y`, col="Obervaciones"))+
  xlab("tiempo en meses")+ylab("empleados asegurados")

pcr.pred[63]
y[62]

y<-y[-63]
#NonParametric Approach using GAM

install.packages("gam")
library(gam)

#usamos una nueva matrix para nuestro modelo

tiempo<-seq(-length(y),-1,1)

m<-as.data.frame(cbind(tiempo,empleo$ingrese_vm7, empleo$icc1, empleo$tpu1, empleo$asegurados_1 ))

m<-m[train,]

gam.lo=gam(y[train]~s(m$tiempo, df=4)+lo(m$V2, span=0.7)+lo(m$V3,span=0.7)+lo(m$V4,span=0.7)+lo(m$V5, span = 0.7), data=m)

plot.Gam(gam.lo, se=TRUE, col="blue")

gam.pred=as.numeric(predict(gam.m1, newdata=m[test,]))

mean((gam.pred-y[test])^2)

#Predicción 
tiempo<-c(tiempo,0)
mp<-as.data.frame(cbind(tiempo,empleoP$ingrese_vm7, empleoP$icc1, empleoP$tpu1, empleoP$asegurados_1))

empleo_pred<-as.numeric(predict(gam.m1, newdata = mp))

#Fit regression tree

pacman::p_load(tree)
library(tree)

tree.empleo=tree(asegurados~., empleo, subset=train)
summary(tree.empleo)

plot(tree.empleo)
text(tree.empleo,pretty = 0)

#usamos cross-validation for selecting tree 

cv.empleo=cv.tree(tree.empleo)
plot(cv.empleo$size, cv.empleo$dev, type = "b")

#test MSE

tree.test=predict(tree.empleo, newdata = empleo[test,])
empleo.test=empleo[test,"medv"]
plot(tree.test, empleo.test)
abline(0,1)

mean((tree.test-y[test])^2)

#Predicción 

tree.pred=predict(tree.empleo, newdata = empleoP)

plot(tree.pred)

#RandomForest

pacman::p_load(randomForest)
library(randomForest)

set.seed(1)
ran.empleo=randomForest(asegurados~.,data=empleo, subset=train, mtry=6, importance=TRUE)
ran.empleo

importance(ran.empleo)
varImpPlot(ran.empleo)

rf.test=predict(ran.empleo, newdata = empleo[test,])
rf<-mean((rf.test-y[test])^2)

#Predicción

rf.pred=predict(ran.empleo, newdata = empleoP)

#tendencia

y<-c(y,NA)

p<-as.data.frame(cbind(y,rf.pred))

ggplot(p,aes(x=seq(as.Date("2015/08/1"), by = "month", length.out = length(y)), y=valor, colour=variable))+
  geom_line(aes(y=p$rf.pred, col="modelo ajustado")) + geom_point(aes(y=`y`, col="Obervaciones"))+
  xlab("tiempo en meses")+ylab("empleados asegurados")

rf.pred[63]
y[62]

y<-y[-63]

#Mejor modelo 
#yo ocuparé Random Forest, PCR y LASSO

if (rf>lasso & lasso<pcr){
  
  print(paste("Lasso es el mejor modelo"))
  
}

if (pcr<lasso & rf>pcr){
  
  print(paste("Random Forest es el mejor modelo"))
  
}

if (rf<lasso & rf<pcr){
  
  print(paste("Random Forest es el mejor modelo"))
  
}
