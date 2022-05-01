library(ggplot2)
library(jsonlite)
library(dplyr)
library(plyr)
library(MCMCpack)
library(RColorBrewer)


df2 <- read.csv("analysis.csv")

df2$White.Player_ID <- factor(df2$White.Player_ID)
nlevels(df2$White.Player_ID)

apply(is.na(df2), 2, which)
summary(df2[,c("White.Player_ID","White.ACPL")])

ggplot(df2) + geom_boxplot(aes(x = reorder(White.Player_ID, White.ACPL, median), White.ACPL, 
                               fill = reorder(White.Player_ID, White.ACPL, median)), show.legend=FALSE)+ theme(axis.text.x = element_text(angle = 90))


ggplot(df2, aes(x = reorder(White.Player_ID, White.Player_ID, length))) + stat_count()+ theme(axis.text.x = element_text(angle = 90))


ggplot(df2, aes(White.ACPL)) + stat_bin() 


ggplot(data.frame(size = tapply(df2$White.ACPL, df2$White.Player_ID, length), 
                  mean_score = tapply(df2$White.ACPL, df2$White.Player_ID, mean)), 
       aes(size, mean_score)) + geom_point()

mean(df2$White.ACPL)
sd(df2$White.ACPL)


compare_m_gibbs <- function(y, ind, maxiter = 5000)
{
  
  ### weakly informative priors
  a0 <- 2.247 ; b0 <- 0.107  ## tau_w hyperparameters
  eta0 <-1/2 ; t0 <- 50 ## tau_b hyperparameters
  mu0<-21  ; gamma0 <- 0.0051
  ###
  
  ### starting values
  m <- nlevels(ind)
  ybar <- theta <- tapply(y, ind, mean)
  tau_w <- mean(1 / tapply(y, ind, var)) ##within group precision
  mu <- mean(theta)
  tau_b <-var(theta) ##between group precision
  n_m <- tapply(y, ind, length)
  an <- a0 + sum(n_m)/2
  ###
  
  ### setup MCMC
  theta_mat <- matrix(0, nrow=maxiter, ncol=m)
  mat_store <- matrix(0, nrow=maxiter, ncol=3)
  ###
  
  ### MCMC algorithm
  for(s in 1:maxiter) 
  {
    
    # sample new values of the thetas
    for(j in 1:m) 
    {
      taun <- n_m[j] * tau_w + tau_b
      thetan <- (ybar[j] * n_m[j] * tau_w + mu * tau_b) / taun
      theta[j]<-rnorm(1, thetan, 1/sqrt(taun))
    }
    
    #sample new value of tau_w
    ss <- 0
    for(j in 1:m){
      ss <- ss + sum((y[ind == j] - theta[j])^2)
    }
    bn <- b0 + ss/2
    tau_w <- rgamma(1, an, bn)
    
    #sample a new value of mu
    gammam <- m * tau_b + gamma0
    mum <- (mean(theta) * m * tau_b + mu0 * gamma0) / gammam
    mu <- rnorm(1, mum, 1/ sqrt(gammam)) 
    
    # sample a new value of tau_b
    etam <- eta0 + m/2
    tm <- t0 + sum((theta - mu)^2) / 2
    tau_b <- rgamma(1, etam, tm)
    
    #store results
    theta_mat[s,] <- theta
    mat_store[s, ] <- c(mu, tau_w, tau_b)
  }
  colnames(mat_store) <- c("mu", "tau_w", "tau_b")
  return(list(params = mat_store, theta = theta_mat))
}


fit2 <- compare_m_gibbs(df2$White.ACPL, df2$White.Player_ID)
plot(as.mcmc(fit2$params))
raftery.diag(as.mcmc(fit))
apply(fit2$params, 2, mean)
apply(fit2$params, 2, sd)

mean(1/sqrt(fit2$params[, 3]))
sd(1/sqrt(fit2$params[, 3]))

## reformat samples for ggplot
theta_df <- data.frame(samples = as.numeric(fit2$theta), 
                       White.Player_ID = rep(1:ncol(fit2$theta), each = nrow(fit2$theta))) 

theta_med <- apply(theta_df, 2, mean) ## get basic posterior summary
sort(theta_med, decreasing = FALSE) ## which players are inferiro and superior?
ggplot(theta_df) + geom_boxplot(aes(x = reorder(White.Player_ID, samples, median), samples, 
                                    fill = reorder(White.Player_ID, samples, median)), show.legend=FALSE)

#theta_hat <- apply(fit2$theta, 2, mean)
#ggplot(data.frame(size = tapply(df2$White.ACPL, df2$White.Player_ID, length), theta_hat = theta_hat), aes(size, theta_hat)) + geom_point()
