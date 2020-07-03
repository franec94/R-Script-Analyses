#### #### #### #### #### #### #### #### #### #### #### 
#### 
#### Regression tree boosting
#### https://cran.r-project.org/web/packages/mvtboost/vignettes/mvtboost_vignette.html
#### #### #### #### #### #### #### #### #### #### #### 


rm(list=ls())

library(mvtboost)

## dataset
data("mpg",package="ggplot2")
# A data frame with 234 rows and 11 variables
#model - model name
#displ - engine displacement, in litres
#year - year of manufacture
#cyl - number of cylinders
#trans - type of transmission
#drv - f = front-wheel drive, r = rear wheel drive, 4 = 4wd
#cty - city miles per gallon
#hwy - highway miles per gallon
#fl - fuel type
#class - "type" of car
summary(mpg)

#  Consumo cittadino e su autostrada come Y
Y <- mpg[,c("cty","hwy")]
# Variabiloi vanno standardizzate
Ys <- scale(Y)    

# Come X abbiamo manufacturer, displacement, year, 
# cylinder, transmission,drive, class
X <- mpg[,-c(2,8:9)]  
# troviamo le variabili che sono caratteri
# e trasformiamole in fattori         
char.ids <- unlist(lapply(X,is.character))
X[,char.ids] <- lapply(X[,char.ids],as.factor)

## qualche plot
i = 1
par(mfrow=c(1,2))
plot(X[,i],Ys[,1], main=colnames(X)[i])
plot(X[,i],Ys[,2], main=colnames(X)[i])

# Tree Boosting
out <- mvtb(Y=Ys,X=X,          # data
        n.trees=1000,          # numero d'alberi
        interaction.depth=3)   # numero di variabili in interazione

# Stimiamo lo stesso modello utilizzando la cross validation
out2 <- mvtb(Y=Ys,X=X,
            n.trees=1000, 
            interaction.depth=3,
            
            bag.fraction=.5,      # Frazione delle osservazioni per la CV            
            cv.folds=3,           # numero di cross-validation folds
            mc.cores=1,           # numero di cores
            seednum=103)          # seed
out2$best.trees

# vediamo i due summaries
summary(out)
summary(out2)

# possiamo calcolare "manualmente" R^2 
yhat <- predict(out2,newdata=X)
(r2 <- var(yhat)/var(Ys))

yhatNoCV <- predict(out2,newdata=X)
(r2NoCV <- var(yhat)/var(Ys))



# vediamo gli effetti "non-lineari" di alcuni predittori 

# plot risultati
par(mfrow=c(3,3))
plot(out2, predictor.no=1, response.no=1)
plot(out2, predictor.no=2, response.no=1)
plot(out2, predictor.no=3, response.no=1)
plot(out2, predictor.no=4, response.no=1)
plot(out2, predictor.no=5, response.no=1)
plot(out2, predictor.no=6, response.no=1)
plot(out2, predictor.no=7, response.no=1)
plot(out2, predictor.no=8, response.no=1)
par(mfrow=c(1,1))
# apre una nuova finestra grafica
dev.new()

par(mfrow=c(3,3))
plot(out2, predictor.no=1, response.no=2)
plot(out2, predictor.no=2, response.no=2)
plot(out2, predictor.no=3, response.no=2)
plot(out2, predictor.no=4, response.no=2)
plot(out2, predictor.no=5, response.no=2)
plot(out2, predictor.no=6, response.no=2)
plot(out2, predictor.no=7, response.no=2)
plot(out2, predictor.no=8, response.no=2)
par(mfrow=c(1,1))


# possiao anche vedere le interazioni con un plot
k = 1
i = 1
j = 2
mvtb.perspec(out2,response.no = k,predictor.no = c(i,j),xlab=colnames(X)[i],ylab=colnames(X)[j],zlab=colnames(Y)[k] ,phi=15, theta=0)

# oppure "numericamente"
nonlin.out <- mvtb.nonlin(out2,X=X,Y=Y)
nonlin.out$hwy$rank.list
nonlin.out$cty$rank.list
nonlin.out$hwy$nonlin.full
nonlin.out$cty$nonlin.full




