######################################################################
###
### Hierarchilca clustering
### https://www.datacamp.com/community/tutorials/hierarchical-clustering-R
#######################################################################
# Data Set Information:
# The examined group comprised kernels belonging to three different varieties of wheat: Kama, Rosa and Canadian, 70 elements each, randomly selected for  the experiment. High quality visualization of the internal kernel structure was detected using a soft X-ray technique. It is non-destructive and considerably cheaper than other more sophisticated imaging techniques like scanning microscopy or laser technology. The images were recorded on 13x18 cm X-ray KODAK plates. Studies were conducted using combine harvested wheat grain originating from experimental fields, explored at the Institute of Agrophysics of the Polish Academy of Sciences in Lublin.  

# Attribute Information:
# To construct the data, seven geometric parameters of wheat kernels were measured:  # 1. area A,  # 2. perimeter P,  # 3. compactness C = 4*pi*A/P^2,  # 4. length of kernel,  # 5. width of kernel,  # 6. asymmetry coefficient  # 7. length of kernel groove.  

rm(list=ls())
## Libraries
library(cluster)
library(dendextend)
library(dplyr)
library(ggplot2)

##
WD = "/Users/gianlucamastrantonio/Desktop/statistico/lavori/lezioni/DataSpace/2018/Lez9/"
setwd(WD)


# carichiamo i dati
seeds_df <- read.csv(paste(WD,"seeds_dataset.txt",sep=""),sep = '\t',header = FALSE)
# cambiamo i nomi
feature_name <- c('area','perimeter','compactness','length.of.kernel','width.of.kernal','asymmetry.coefficient','length.of.kernel.groove','type.of.seed')
colnames(seeds_df) <- feature_name
str(seeds_df)
summary(seeds_df)

# eliminiamo gli NA
for(i in 1:ncol(seeds_df))
{
	W = which(!is.na(seeds_df[,i]))
	seeds_df = seeds_df[W,]
}



# "standardizziamo" le variabili e salviamo la clusterizzazione
seeds_label <- seeds_df$type.of.seed
seeds_df$type.of.seed <- NULL
str(seeds_df)
seeds_df_sc <- as.data.frame(scale(seeds_df))
summary(seeds_df_sc)

# plots
ggplot(seeds_df_sc, aes(x=area, y = perimeter, color = factor(seeds_label))) + geom_point()
ggplot(seeds_df_sc, aes(x=area, y = compactness, color = factor(seeds_label))) + geom_point()
ggplot(seeds_df_sc, aes(x=area, y = length.of.kernel, color = factor(seeds_label))) + geom_point()

plot(density(seeds_df_sc$area))
plot(density(seeds_df_sc$perimeter))
plot(density(seeds_df_sc$compactness))
#...

# calcoliamo la matrice distanza
dist_mat <- dist(seeds_df_sc, method = 'euclidean')
# stimiamo il modello
hclust_avg <- hclust(dist_mat, method = 'average')
plot(hclust_avg)
# tagliamo il dendogramma con 3 clusters
cut_avg <- cutree(hclust_avg, k = 3)
table(cut_avg,seeds_label)

# qualche plot
ggplot(seeds_df_sc, aes(x=area, y = perimeter, color = factor(cut_avg))) + geom_point()
ggplot(seeds_df_sc, aes(x=area, y = compactness, color = factor(cut_avg))) + geom_point()
ggplot(seeds_df_sc, aes(x=area, y = length.of.kernel, color = factor(cut_avg))) + geom_point()


# calcoliamo le varianze dentro i gruppi con un funzione
wss <- function(d) {
  sum(scale(d, scale = FALSE)^2)
}
wrap <- function(i, hc, x) {
  cl <- cutree(hc, i)
  spl <- split(x, cl)
  wss <- sum(sapply(spl, wss))
  wss
}
res <- sapply(seq.int(1, 10), wrap, h = hclust_avg, x = seeds_df_sc)

plot(1:10,res,type="b", main="Total Within SS by Various K",
 ylab="Average Total Within Sum of Squares",
 xlab="Value of K")



# possiamo vedere i cluster che stiamo creando
plot(hclust_avg)
rect.hclust(hclust_avg , k = 3, border = 2:6)
abline(h = 3, col = 'red')

# un'latro modo di rappresentare i clusters
avg_dend_obj <- as.dendrogram(hclust_avg)
avg_col_dend <- color_branches(avg_dend_obj, h = 3)
plot(avg_col_dend)


# vediamo il numero di osservazione nei cluster
cut_avg <- cutree(hclust_avg, k = 3)
seeds_df_cl <- mutate(seeds_df, cluster = cut_avg)
count(seeds_df_cl,cluster)


ggplot(seeds_df_cl, aes(x=area, y = perimeter, color = factor(cluster))) + geom_point()

# vediamo la silhouette
D <- daisy(seeds_df_sc)
k =3
plot(silhouette(cutree(hclust_avg, k = k), D), col=1:8, border=NA)

## potremmo vedere i risultati che si
# ottengono con il k-means

