##### ##### ##### ##### ##### ##### ##### ##### ##### #####  
# car.csv  - by Gianluca Mastrantonio
#              DSMS 2018 Esempio Cars - LDA.   
# il dataset contiene dati su 388 macchine prodotte nel 2014, su cui
# sono state registrate 18 caratteristiche
#
# Sports: Binary indicator for being a sports car
# SUV: Indicator for sports utility vehicle
# Wagon: Indicator
# Minivan: Indicator
# Pickup: Indicator
# AWD: Indicator for all-wheel drive
# RWD: Indicator for rear-wheel drive
# Retail: Suggested retail price (US$)
# Dealer: Price to dealer (US$)
# Engine: Engine size (liters)
# Cylinders: Number of engine cylinders
# Horsepower: Engine horsepower
# CityMPG: City gas mileage - 
# HighwayMPG: Highway gas mileage
# Weight: Weight (pounds)
# Wheelbase: Wheelbase (inches)
# Length: Length (inches)
# Width: Width (inches)
#  nota che
# gas mileage is the ratio of the number of miles travelled to the number of gallons of gasoline burned; fuel efficiency
# Puliamo il workspace 
rm(list=ls())

## 



#librerie
library(MASS)
library(car)
library(rattle)
library(klaR)
library(ROCR)


DataTot = read.csv("181106 Dati per lo studio del caso Car.csv")
summary(Data)

table(rowSums(DataTot[,c(1:5)]))
DataTot$Type = "NA"
DataTot$Type[DataTot$Sports==1] = "Sp" 
DataTot$Type[DataTot$SUV==1] = "SUV"
DataTot$Type[DataTot$Wagon==1] = "Wag"
DataTot$Type[DataTot$Minivan==1] = "Mini"
DataTot$Type[DataTot$Pickup==1] = "Pick"
DataTot$Type = as.factor(DataTot$Type)
## 
DataNonNA      = DataTot[DataTot$Type!="NA",- c(1:7)]
DataNA       = DataTot[DataTot$Type=="NA",-c(1:7)]

# Elimino il valore NA
DataNonNA$Type = as.factor(as.character(DataNonNA$Type))


Samp = sample(1:nrow(DataNonNA),nrow(DataNonNA)*0.9, replace=F )

Data      = DataNonNA[Samp,]
DataTest  = DataNonNA[-Samp,]



# qualche plot
un = unique(Data$Type)
h = 0
par(mfrow=c(2,2))
for(i in 1:11)
{
  boxplot(Data[,i]~ Data$Type, col=2:5, main=colnames(Data)[i])
  
  m = max(c(density(Data[Data$Type==un[1],i])$y,density(Data[Data$Type==un[2],i])$y,density(Data[Data$Type==un[3],i])$y,density(Data[Data$Type==un[4],i])$y))
  
  plot(density(Data[Data$Type==un[1],i]), col=2, xlim= c(min(Data[,i]),max(Data[,i])), ylim=c(0,m))
  lines(density(Data[Data$Type==un[2],i]), col=3,add=T)
  lines(density(Data[Data$Type==un[3],i]), col=4,add=T)
  lines(density(Data[Data$Type==un[4],i]), col=5,add=T)
  h = h+1
  if((h%%(2))==0)
  {
    readline(prompt = "Pause. Press <Enter> to continue...")
  }
}

### ### ### ### ### 
### LDA 
### ### ### ### ### 
LDA = lda(Type ~ ., data=Data)
summary(LDA)
str(LDA)
LDA


## qualità previsione
un = as.character(un)
Pred = predict(LDA)
summary(Pred)
tab = table(Data$Type, Pred$class)
conCV1 <- rbind(tab[1, ]/sum(tab[1, ]), tab[2, ]/sum(tab[2, ]), tab[3, ]/sum(tab[3, ]),tab[4, ]/sum(tab[4, ]))
dimnames(conCV1) <- list(Actual = c(un[1], un[2],un[3],un[4]), "Predicted" = c(un[1],un[2],un[3],un[4]))
round(conCV1, 3)
# accuratezza
sum(diag(tab))/(sum(c(tab)))

ldahist(data = Pred$x[,1], g=Data$Type)
ldahist(data = Pred$x[,2], g=Data$Type)
ldahist(data = Pred$x[,3], g=Data$Type)

par(mfrow=c(1,1))
plot(Pred$x[,1],Pred$x[,2], cex=0.2, col=as.numeric(Data$Type)) 
text(Pred$x[,1],Pred$x[,2],Data$Type,cex=0.7,pos=4,col=as.numeric(Data$Type)) 

par(mfrow=c(1,1))
plot(Pred$x[,1],Pred$x[,3], cex=0.2, col=as.numeric(Data$Type)) 
text(Pred$x[,1],Pred$x[,3],Data$Type,cex=0.7,pos=4,col=as.numeric(Data$Type)) 


par(mfrow=c(1,1))
plot(Pred$x[,2],Pred$x[,3], cex=0.2, col=as.numeric(Data$Type)) 
text(Pred$x[,2],Pred$x[,3],Data$Type,cex=0.7,pos=4,col=as.numeric(Data$Type)) 


par(mfrow=c(1,3))
plot(Pred$x[,1],Pred$x[,2], cex=1.5, col=as.numeric(Data$Type), pch=20)
plot(Pred$x[,1],Pred$x[,3], cex=1.5, col=as.numeric(Data$Type), pch=20)
plot(Pred$x[,2],Pred$x[,3], cex=1.5, col=as.numeric(Data$Type), pch=20)

# Interpretazione degli assi?
round(LDA$scaling[order(LDA$scaling[,1]),1],3)
round(LDA$scaling[order(LDA$scaling[,2]),2],3)
round(LDA$scaling[order(LDA$scaling[,3]),3],3)

# CV
LDA2 = lda(Type ~ ., data=Data, CV=T)
tab2 = table(Data$Type, LDA2$class)
conCV2 <- rbind(tab2[1, ]/sum(tab2[1, ]), tab2[2, ]/sum(tab2[2, ]), tab2[3, ]/sum(tab2[3, ]),tab2[4, ]/sum(tab2[4, ]))
dimnames(conCV2) <- list(Actual = c(un[1], un[2],un[3],un[4]), "Predicted (CV)" = c(un[1],un[2],un[3],un[4]))
round(conCV2, 3)
# accuratezza
sum(diag(tab2))/(sum(c(tab2)))

## confrontiamo le due tabelle
par(mfrow=c(1,2))
mosaicplot(tab)
mosaicplot(tab2)

# testing set
Pred3 = predict(LDA, newdata = DataTest)
summary(Pred3)
tab3 = table(DataTest$Type, Pred3$class)
conCV3 <- rbind(tab3[1, ]/sum(tab3[1, ]), tab3[2, ]/sum(tab3[2, ]), tab3[3, ]/sum(tab3[3, ]),tab3[4, ]/sum(tab3[4, ]))
dimnames(conCV3) <- list(Actual = c(un[1], un[2],un[3],un[4]), "Predicted" = c(un[1],un[2],un[3],un[4]))
round(conCV3, 3)

sum(diag(tab3))/(sum(c(tab3)))


## predizione  su DataNA
Pred4 = predict(LDA, newdata = DataNA)
Wmax = apply(Pred4$posterior,1,which.max)
plot(1,1, type="n", xlim=c(1,nrow(Pred4$posterior)), ylim=c(0,1))
for(i in 1:nrow(Pred4$posterior))
{
  points(i, Pred4$posterior[i,Wmax[i]], col=Wmax[i]+1, cex=2, pch=20)
}
legend(locator(1), colnames(Pred4$posterior), col=2:5, pch=20)

DataBP = rbind(Data,DataNA)

i = 5
par(mfrow=c(1,1))
boxplot(DataBP[,i]~ DataBP$Type, col=2:6, main=colnames(Data)[i])



# Continua....
LDA = lda(Type ~ (Retail+Dealer+Engine+Cylinders+Horsepower+CityMPG+HighwayMPG+Weight+Wheelbase+Length+Width)^2 , data=Data)
LDA

## ## ## ## ## ## 
## ROC
## ## ## ## ## ## 

TypeC = ifelse(Data$Type=="Mini",1,0)
pred = prediction(Pred$posterior[,1], TypeC) 
perf = performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)
abline(a=0,b=1)

TypeC = ifelse(Data$Type=="Sp",1,0)
pred = prediction(Pred$posterior[,2], TypeC) 
perf = performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)
abline(a=0,b=1)

TypeMini = ifelse(Data$Type=="SUV",1,0)
pred = prediction(Pred$posterior[,3], TypeC) 
perf = performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)
abline(a=0,b=1)

TypeMini = ifelse(Data$Type=="Wag",1,0)
pred = prediction(Pred$posterior[,4], TypeC) 
perf = performance(pred,"tpr","fpr")
plot(perf,colorize=TRUE)
abline(a=0,b=1)

#plot(perf@alpha.values[[1]] )
