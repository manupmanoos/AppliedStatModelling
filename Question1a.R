library(ggplot2)
library(jsonlite)
library(dplyr)
library(plyr)
library(MCMCpack)
library(RColorBrewer)


df1 <- read.csv("analysis.csv")



# Selecting only Viswanathan Anand(2) and Magnus Carlsen(8) from dataset when they are using white pieces
twoPlayers <- filter(df1,
                     df1$White.Player == "Anand, Viswanathan" |
                       df1$White.Player == "Carlsen, Magnus")
twoPlayers$White.Player <- factor(twoPlayers$White.Player)
apply(is.na(twoPlayers), 2, which)
summary(twoPlayers[,c("White.Player","White.ACPL")])

ggplot(twoPlayers) + geom_boxplot(aes(White.Player, White.ACPL, fill = White.Player)) + geom_jitter(aes(White.Player, White.ACPL, shape = White.Player))
tapply(twoPlayers$White.ACPL, twoPlayers$White.Player, mean)
tapply(twoPlayers$White.ACPL, twoPlayers$White.Player, median)
tapply(twoPlayers$White.ACPL, twoPlayers$White.Player, sd)


t.test(White.ACPL ~ White.Player, data=twoPlayers, var.equal = FALSE)


compare_2_gibbs <- function(y, ind, mu0 = 50, tau0 = 1/400, del0 = 0, gamma0 = 1/400, a0 = 1, b0 = 50, maxiter = 5000)
{
  y1 <- y[ind == "Anand, Viswanathan"]
  y2 <- y[ind == "Carlsen, Magnus"]
  
  n1 <- length(y1) 
  n2 <- length(y2)
  
  ##### starting values
  mu <- (mean(y1) + mean(y2)) / 2
  del <- (mean(y1) - mean(y2)) / 2
  
  mat_store <- matrix(0, nrow = maxiter, ncol = 3)
  #####
  
  ##### Gibbs sampler
  an <- a0 + (n1 + n2)/2
  
  for(s in 1 : maxiter) 
  {
    
    ##update tau
    bn <- b0 + 0.5 * (sum((y1 - mu - del) ^ 2) + sum((y2 - mu + del) ^ 2))
    tau <- rgamma(1, an, bn)
    ##
    
    ##update mu
    taun <-  tau0 + tau * (n1 + n2)
    mun <- (tau0 * mu0 + tau * (sum(y1 - del) + sum(y2 + del))) / taun
    mu <- rnorm(1, mun, sqrt(1/taun))
    ##
    
    ##update del
    gamman <-  gamma0 + tau*(n1 + n2)
    deln <- ( del0 * gamma0 + tau * (sum(y1 - mu) - sum(y2 - mu))) / gamman
    del<-rnorm(1, deln, sqrt(1/gamman))
    ##
    
    ## store parameter values
    mat_store[s, ] <- c(mu, del, tau)
  }
  colnames(mat_store) <- c("mu", "del", "tau")
  return(mat_store)
}

mean(twoPlayers$White.ACPL)
sd(twoPlayers$White.ACPL)

fit <- compare_2_gibbs(twoPlayers$White.ACPL, as.factor(twoPlayers$White.Player),13,0.0156,0,0.0156,2.639, 0.203)

plot(as.mcmc(fit))
raftery.diag(as.mcmc(fit))

apply(fit, 2, mean)

apply(fit, 2, sd)

mean(1/sqrt(fit[, 3])) 

sd(1/sqrt(fit[, 3])) 



y1_sim <- rnorm(5000, fit[, 1] + fit[, 2], sd = 1/sqrt(fit[, 3]))
y2_sim <- rnorm(5000, fit[, 1] - fit[, 2], sd = 1/sqrt(fit[, 3]))

ggplot(data.frame(y_sim_diff = y1_sim - y2_sim)) + stat_bin(aes(y_sim_diff))

mean(y1_sim <y2_sim)
ggplot(data.frame(y1_sim, y2_sim)) + geom_point(aes(y1_sim, y2_sim), alpha = 0.3) + geom_abline(slope = 1, intercept = 0)
