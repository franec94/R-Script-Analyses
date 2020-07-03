##### ##### ##### ##### ##### ##### ##### ##### ##### #####  
# car.csv  - by Gianluca Mastrantonio
#              DSMS 2018 Esempio Cars.   
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
#---------- Puliamo il dataset
rm(list=ls())

#---------- Librerie

#---------- Caricamento dati
DIR = "/Users/gianlucamastrantonio/Desktop/statistico/lavori/lezioni/DataSpace/2018/Lez4/"
setwd(DIR)
Cars = read.csv("181130 Dati per lo studio del caso Car.csv", header=T)
summary(Cars)

#---------- Analisi

# Facciamo dei plot
plot(Cars[,-c(1:7)])
# si vede poco, facciamo diversamente
h = 0
par(mfrow=c(3,3))
for(i in 8:(ncol(Cars)-1))
{
  for(j in (i+1):ncol(Cars))
  {
    plot(Cars[,i],Cars[,j], xlab=colnames(Cars)[i],ylab=colnames(Cars)[j], pch=1, col=2, cex=1.4)
    h = h+1
    if(h%%(9)==0)
    {
      readline(prompt = "Pause. Press <Enter> to continue...")
      par(mfrow=c(3,3))
      h = 0
    }
  }
}
par(mfrow=c(1,1))
## facciamo la PCA sulle varaibili non binarie
PCA = prcomp(Cars[-c(1:7)], scale. = T)
names(PCA)
summary(PCA)
plot(PCA)

# possiamo vedere medie e varianza delle variabili 
# prima delle standardizzazione
PCA$center
PCA$scale

# e la matrice dei factor loadings
PCA$rotation

# le componenti principali possono essere viste in 
PCA$x[1:10,]

# vediamo come passare da i factor loadings alle componenti principali

# creao il dataset standardizzato
DataStand = Cars
for(i in 1:11)
{
  DataStand[,7+i] = (DataStand[,7+i]-PCA$center[i])/PCA$scale[i]
}
# calcolo la oprima componente
PrimaComponente = matrix(PCA$rotation[,1],ncol=11,nrow=1)%*%(t(DataStand[-c(1:7)]))
# vediamo che si equivalgono
plot(PrimaComponente,PCA$x[,1])

# vediamo la percentuale di varianza spiegata 
summary(PCA)

str(summary(PCA))
par(mfrow=c(1,2))
plot(summary(PCA)$importance["Proportion of Variance",], type="b", lwd=2, lty=1)
plot(summary(PCA)$importance["Cumulative Proportion",], type="b", lwd=2, lty=1)
par(mfrow=c(1,1))

# plottiamo le prime due componenti con osservazione e variabili
?biplot.princomp
biplot(PCA)
# eliminimao i nomi delle variabili
biplot(PCA,choices=c(1,2), xlabs = rep(".", nrow(Cars)))
# vediamo quele sono le variabili con alto x (prima componente) e basse y (seconda componente) 
round(PCA$rotation[,1],3)
which(PCA$rotation[,1]>0.05)
# vediamo quele sono le variabili con alto y (seconda componente)  assoluto e basse x (prima  componente) 
which((PCA$rotation[,2])>0.05)
which((PCA$rotation[,2])< -0.05)

# oppure
PCA$rotation[order(PCA$rotation[,1]),1]
PCA$rotation[order(PCA$rotation[,2]),2]


# possiamo interpretare i primi due assi
# 1 - indica macchine con motori molto potenti (valori bassi) o piccole macchine 
# con motori  efficienti (valori alti)
# 2 -  la seconda componente mette in relazione le dimensione delle macchine (Weight Wheelbase    Length     Width)
# con il   prezzo/potenza  (Retail     Dealer  Cylinders Horsepower)
# diciamo che la seconda separa i mini-vans e SUV (grandi, non troppe costosi, non troppo potenti)
# dalle macchine sportive (piccole costose e potenti)

# vediamo se le interpretazioni sono giuste, 
#vedendo come le compoennti di distribuiscono secondo il tipo di macchina
# e ruote motrici

table(rowSums(Cars[,c(1:5)]))
Cars$Type = "NA"
Cars$Type[Cars$Sports==1] = "Sp" 
Cars$Type[Cars$SUV==1] = "SUV"
Cars$Type[Cars$Wagon==1] = "Wag"
Cars$Type[Cars$Minivan==1] = "Mini"
Cars$Type[Cars$Pickup==1] = "Pick"
Cars$Type = as.factor(Cars$Type)


table(rowSums(Cars[,c(6,7)]))
Cars$Ruote= "Dav"
Cars$Ruote[Cars$AWD==1] = "4" 
Cars$Ruote[Cars$RWD==1] = "Diet"
Cars$Ruote = as.factor(Cars$Ruote)


par(mfrow=c(1,2))
boxplot(PCA$x[,1] ~ Cars$Type,col=rainbow(10))
boxplot(PCA$x[,1] ~ Cars$Ruote,col=rainbow(10))
par(mfrow=c(1,1))

par(mfrow=c(1,1))
boxplot(PCA$x[,1] ~ Cars$Ruote+Cars$Type,col=rainbow(3),las=2)
par(mfrow=c(1,1))

par(mfrow=c(1,2))
boxplot(PCA$x[,2] ~ Cars$Type,col=rainbow(10))
boxplot(PCA$x[,2] ~ Cars$Ruote,col=rainbow(10))
par(mfrow=c(1,1))

par(mfrow=c(1,1))
boxplot(PCA$x[,2] ~ Cars$Ruote+Cars$Type,col=rainbow(3),las=2)
par(mfrow=c(1,1))

# potremmo plottare anche Type o Ruote per vedere dove si collocano nel piano

plot(PCA$x[,1],PCA$x[,2],  cex=1, col=as.numeric(Cars$Type), pch = as.numeric(Cars$Ruote), lwd=2)
legend("bottomright", legend=paste(rep(levels(Cars$Type),each=3),rep(levels(Cars$Ruote),times=5)), col = rep(1:5,each=3),  pch=rep(1:3,times=5), cex=1)




# possiamo plottare anche la terza e quarta componente
biplot(PCA,choices=c(3,4), xlabs = rep(".", nrow(Cars)))
# e vedere quali sono le variabili "importanti" per la terza componente
which(PCA$rotation[,3]< -0.1)
# e la quarta
which(PCA$rotation[,4]< -0.1)

PCA$rotation[order(PCA$rotation[,3]),3]
PCA$rotation[order(PCA$rotation[,4]),4]


par(mfrow=c(1,2))
boxplot(PCA$x[,3] ~ Cars$Type,col=rainbow(10))
boxplot(PCA$x[,3] ~ Cars$Ruote,col=rainbow(10))
par(mfrow=c(1,1))

par(mfrow=c(1,1))
boxplot(PCA$x[,3] ~ Cars$Ruote+Cars$Type,col=rainbow(3),las=2)
par(mfrow=c(1,1))

par(mfrow=c(1,2))
boxplot(PCA$x[,4] ~ Cars$Type,col=rainbow(10))
boxplot(PCA$x[,4] ~ Cars$Ruote,col=rainbow(10))
par(mfrow=c(1,1))

par(mfrow=c(1,1))
boxplot(PCA$x[,4] ~ Cars$Ruote+Cars$Type,col=rainbow(3),las=2)
par(mfrow=c(1,1))



plot(PCA$x[,3],PCA$x[,4],  cex=1, col=as.numeric(Cars$Type), pch = as.numeric(Cars$Ruote), lwd=2)
legend("topleft", legend=paste(rep(levels(Cars$Type),each=3),rep(levels(Cars$Ruote),times=5)), col = rep(1:5,each=3),  pch=rep(1:3,times=5), cex=1)


# 