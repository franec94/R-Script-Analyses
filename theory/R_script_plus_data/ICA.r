#### #### #### #### #### #### #### #### #### #### #### 
#### 
#### ICA
#### http://rstudio-pubs-static.s3.amazonaws.com/93614_be30df613b2a4707b3e5a1a62f631d19.html
#### #### #### #### #### #### #### #### #### #### #### 


rm(list=ls())

library(fastICA)


# Generiamo dei dati
S <- cbind(sin((1:1000)/20), rep((((1:200)-100)/100), 5))
# vediamo cosa abbiamo generato
par(mfrow=c(1,2))
plot(1:1000, S[,1], type = "l",xlab = "S1", ylab = "")
plot(1:1000, S[,2], type = "l", xlab = "S2", ylab = "")
par(mfrow=c(1,1))

# Creiamo un segnale misto
# Matrice dei "pesi"
A <- matrix(c(0.291, 0.6557, -0.5439, 0.5572), 2, 2)
A
# Mixiamo i segnali
X <- S %*% A
# vediamo il nuovo segnale
par(mfrow=c(1,2))
plot(1:1000, X[,1], type = "l",xlab = "X1", ylab = "")
plot(1:1000, X[,2], type = "l", xlab = "X2", ylab = "")
par(mfrow=c(1,1))

# Implementiamo ICA
a <- fastICA(X, 2, alg.typ = "parallel", fun = "logcosh", alpha = 1,
             method = "R", row.norm = FALSE, maxit = 200,
             tol = 0.0001, verbose = TRUE)

# vediamo i risultati
par(mfrow=c(1,2))
plot(1:1000, a$S[,1], type = "l", xlab = "S'1", ylab = "")
plot(1:1000, a$S[,2], type = "l", xlab = "S'2", ylab = "")
par(mfrow=c(1,1))

# confrontiamo con i dati originali
par(mfrow=c(1,2))
plot(1:1000, a$S[,2]/S[,1], type = "l", xlab = "Ratio S'1", ylab = "")
plot(1:1000, a$S[,1]/S[,2], type = "l", xlab = "Ratio S'2", ylab = "")
par(mfrow=c(1,1))

# proviamo ad aumentare il numero di classi
a3 <- fastICA(X, 3, alg.typ = "parallel", fun = "logcosh", alpha = 1,
             method = "R", row.norm = FALSE, maxit = 200,
             tol = 0.0001, verbose = TRUE)


# Aggiungiamo del rumore
a3 <- fastICA(cbind(X,rnorm(nrow(X),0,0.2)), 3, alg.typ = "parallel", fun = "logcosh", alpha = 1,
             method = "R", row.norm = FALSE, maxit = 200,
             tol = 0.0001, verbose = TRUE)

par(mfrow=c(1,3))
plot(1:1000, a3$S[,1], type = "l", xlab = "S", ylab = "")
plot(1:1000, a3$S[,2], type = "l", xlab = "S", ylab = "")
plot(1:1000, a3$S[,3], type = "l", xlab = "S", ylab = "")
par(mfrow=c(1,1))

