######################################################################
###
###  Mixture
###  Fonte: http://www.learnbymarketing.com/tutorials/k-means-clustering-in-r-example/
#######################################################################
# The data set refers to clients of a wholesale distributor. It includes the annual spending in monetary units (m.u.) on diverse product categories
# The variables are:
# 1) FRESH: annual spending (m.u.) on fresh products (Continuous); 
# 2) MILK: annual spending (m.u.) on milk products (Continuous); 
# 3) GROCERY: annual spending (m.u.)on grocery products (Continuous); 
# 4) FROZEN: annual spending (m.u.)on frozen products (Continuous) 
# 5) DETERGENTS_PAPER: annual spending (m.u.) on detergents and paper products (Continuous) 
# 6) DELICATESSEN: annual spending (m.u.)on and delicatessen products (Continuous); 
# 7) CHANNEL: customer's Channel - Horeca (Hotel/Restaurant/CafÃ©) or Retail channel (Nominal) 
# 8) REGION: customer's Region “ Lisnon, Oporto or Other (Nominal) 


rm(list=ls())
## Libraries
library(cluster)
##
WD = "/Users/gianlucamastrantonio/Desktop/statistico/lavori/lezioni/DataSpace/2018/Lez9/"
setwd(WD)

# dataset
data <-read.csv("Wholesale customers data.csv",header=T)
summary(data)

# facciamo dei plot 
plot(data[,-c(1,2)])

# vediamo le stime di densità
plot(density(data[,3]), xlab= colnames(data)[3])
plot(density(data[,4]), xlab= colnames(data)[4])
plot(density(data[,5]), xlab= colnames(data)[5])
plot(density(data[,6]), xlab= colnames(data)[6])
plot(density(data[,7]), xlab= colnames(data)[7])
plot(density(data[,8]), xlab= colnames(data)[8])

# vediamo le densità in scala log
plot(density(log(data[,3])), xlab= colnames(data)[3])
plot(density(log(data[,4])), xlab= colnames(data)[4])
plot(density(log(data[,5])), xlab= colnames(data)[5])
plot(density(log(data[,6])), xlab= colnames(data)[6])
plot(density(log(data[,7])), xlab= colnames(data)[7])
plot(density(log(data[,8])), xlab= colnames(data)[8])

# vediamo qualche densità bivariata
smoothScatter(log(data[,3]),log(data[,4]),xlab= colnames(data)[3],ylab= colnames(data)[4])
smoothScatter(log(data[,3]),log(data[,5]),xlab= colnames(data)[3],ylab= colnames(data)[5])
smoothScatter(log(data[,3]),log(data[,6]),xlab= colnames(data)[3],ylab= colnames(data)[6])
smoothScatter(log(data[,3]),log(data[,7]),xlab= colnames(data)[3],ylab= colnames(data)[7]) #
smoothScatter(log(data[,3]),log(data[,8]),xlab= colnames(data)[3],ylab= colnames(data)[8])

smoothScatter(log(data[,4]),log(data[,5]),xlab= colnames(data)[4],ylab= colnames(data)[5])#
smoothScatter(log(data[,4]),log(data[,6]),xlab= colnames(data)[4],ylab= colnames(data)[6])
smoothScatter(log(data[,4]),log(data[,7]),xlab= colnames(data)[4],ylab= colnames(data)[7]) 
smoothScatter(log(data[,4]),log(data[,8]),xlab= colnames(data)[4],ylab= colnames(data)[8]) #

smoothScatter(log(data[,5]),log(data[,6]),xlab= colnames(data)[5],ylab= colnames(data)[6])
smoothScatter(log(data[,5]),log(data[,7]),xlab= colnames(data)[5],ylab= colnames(data)[7]) #
smoothScatter(log(data[,5]),log(data[,8]),xlab= colnames(data)[5],ylab= colnames(data)[8]) 

smoothScatter(log(data[,6]),log(data[,7]),xlab= colnames(data)[6],ylab= colnames(data)[7]) #
smoothScatter(log(data[,6]),log(data[,8]),xlab= colnames(data)[6],ylab= colnames(data)[8]) 

smoothScatter(log(data[,7]),log(data[,8]),xlab= colnames(data)[7],ylab= colnames(data)[8]) #


# eliminiamo i top-clienti
top.n.custs <- function (data,cols,n=5) 
{ 
	idx.to.remove <-integer(0) 
	for (c in cols)
	{ 
		col.order <-order(data[,c],decreasing=T) 
		idx <-head(col.order, n) 
		idx.to.remove <-union(idx.to.remove,idx) 
	}
	return(idx.to.remove) #Return the indexes of customers to be removed
}
top.custs <-top.n.custs(data,cols=3:8,n=5)
length(top.custs) 
# vediamo chi sono
data[top.custs,] 
# eliminiamoli
data.rm.top<-data[-c(top.custs),] 
 # standardizziamo
data.rm.top = as.data.frame(scale(data.rm.top))

# il modello
set.seed(76964057) 
# definiamo 5 cluster
k <-kmeans(data.rm.top[,-c(1,2)], centers=5) 
k$centers
summary(data.rm.top[,-c(1,2)]) 
table(k$cluster) 
# rapporto tra varianze
k$withinss/k$betweenss

# qualche plot
plot(log(data[,3]),log(data[,7]),xlab= colnames(data)[3],ylab= colnames(data)[7], col=k$cluster)
plot(log(data[,7]),log(data[,8]),xlab= colnames(data)[7],ylab= colnames(data)[8], col=k$cluster)

# stimiamo il numero di componenti
rng<-2:20  # numero di cluster da testare
tries <-100  # iterazioni dell'algoritmo
avg.totw.ss <-integer(length(rng)) 
for(v in rng)
{ 
 	v.totw.ss <-integer(tries)
 	for(i in 1:tries)
	{
 		k.temp <-kmeans(data.rm.top[,-c(1,2)],centers=v) 
 		v.totw.ss[i] <-k.temp$tot.withinss
 	}
 	avg.totw.ss[v-1] <-mean(v.totw.ss) #Average the 100 total withinss
}
plot(rng,avg.totw.ss,type="b", main="Total Within SS by Various K",
 ylab="Average Total Within Sum of Squares",
 xlab="Value of K")

#### #### #### #### #### #### #### #### #### #### #### #### 
# cosa ci saremmo dovuto aspettare
# in caso di cluster ben separati?
# Simuliamo
n  = 1000
Data = cbind(c(rnorm(n, 0, 1),rnorm(n, 10, 1),rnorm(n, -10, 1)),c(rnorm(n, 0, 1),rnorm(n, 10, 1),rnorm(n, -10, 1)))
plot(Data)

Ksim1 = kmeans(Data,centers=1)
Ksim2 = kmeans(Data,centers=2)
Ksim3 = kmeans(Data,centers=3)
Ksim4 = kmeans(Data,centers=4)
Ksim5 = kmeans(Data,centers=5)

plot(1:5,c(mean(Ksim1$tot.withinss),mean(Ksim2$tot.withinss),mean(Ksim3$tot.withinss),mean(Ksim4$tot.withinss),mean(Ksim5$tot.withinss)),type="b", main="Total Within SS by Various K",
 ylab="Average Total Within Sum of Squares",
 xlab="Value of K")
#### #### #### #### #### #### #### #### #### #### #### #### 
# esame della silhouette
# matrice delle distanze 
D <- daisy(data.rm.top[-c(1,2)])
Nclust = 6
plot(silhouette(kmeans(data.rm.top[,-c(1,2)], centers=Nclust)$cluster, D), col=1:8, border=NA)

