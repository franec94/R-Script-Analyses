#### #### #### #### #### #### #### #### #### #### #### 
#### 
#### MDS
#### http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/122-multidimensional-scaling-essentials-algorithms-and-r-code/
#### #### #### #### #### #### #### #### #### #### #### 


rm(list=ls())
#librerie
library(magrittr)
library(dplyr)
library(ggpubr)
library(magrittr)
library(dplyr)
library(ggpubr)
library(MASS)
## dataset
data("swiss")
head(swiss)
?swiss



# # # # # # # # # # # 
# MDS
# # # # # # # # # # # 
mds <- swiss %>%
  dist() %>%          
  cmdscale() %>%
  as.data.frame()
colnames(mds) <- c("Dim.1", "Dim.2")
# Plot MDS
ggscatter(mds, x = "Dim.1", y = "Dim.2", 
          label = rownames(swiss),
          size = 1,
          repel = TRUE)
          
          

 

### ### ### ### ### ### ### ### ### ### ### 
### Vediamo altri pacchetti che fanno
### MDS   
### ### ### ### ### ### ### ### ### ### ### 
# il dataset
?eurodist

# standard sintassi
mds1 = cmdscale(eurodist, k = 2)
# plot
plot(mds1[,1], mds1[,2], type = "n", xlab = "", ylab = "", axes = FALSE,
     main = "cmdscale (stats)")
text(mds1[,1], mds1[,2], labels(eurodist), cex = 0.9, xpd = TRUE)


library(vegan)
# 2) MDS 'wcmdscale'
mds2 = wcmdscale(eurodist, k=2, w=rep(1,21))
# plot
plot(mds2[,1], mds2[,2], type = "n", xlab = "", ylab = "",
     axes = FALSE, main = "wcmdscale (vegan)")
text(mds2[,1], mds2[,2], labels(eurodist), cex = 0.9, xpd = TRUE)


library(ecodist)
# 3) MDS 'pco'
mds3 = pco(eurodist)
# plot
plot(mds3$vectors[,1], mds3$vectors[,2], type = "n", xlab = "", ylab = "",axes = FALSE, main = "pco (ecodist)")
text(mds3$vectors[,1], mds3$vectors[,2], labels(eurodist), 
     cex = 0.9, xpd = TRUE)
       

library(labdsv)
mds4 = pco(eurodist, k = 2)
# plot
plot(mds4$points[,1], mds4$points[,2], type = "n", xlab = "", ylab = "", axes = FALSE, main = "pco (labdsv)")
text(mds4$points[,1], mds4$points[,2], labels(eurodist), 
     cex = 0.9, xpd = TRUE)

library(ape)
mds5 = pcoa(eurodist)

# plot
plot(mds5$vectors[,1], mds5$vectors[,2], type = "n", xlab = "", ylab = "",axes = FALSE, main = "pcoa (ape)")
text(mds5$vectors[,1], mds5$vectors[,2], labels(eurodist), 
     cex = 0.9, xpd = TRUE)
     
library(ade4) 
mds6 = dudi.pco(eurodist, scannf = FALSE, nf = 2)
# plot
plot(mds6$li[,1], mds6$li[,2], type = "n", xlab = "", ylab = "",
     axes = FALSE, main = "dudi.pco (ade4)")
text(mds6$li[,1], mds6$li[,2], labels(eurodist), cex = 0.9)

library(smacof)
mds7 = smacofSym(eurodist, ndim = 2)

# plot
plot(mds7$conf[,1], mds7$conf[,2], type = "n", xlab = "", ylab = "",
     axes = FALSE, main = "smacofSym (smacof)")
text(mds7$conf[,1], mds7$conf[,2], labels(eurodist), 
     cex = 0.9, xpd = TRUE)
    
