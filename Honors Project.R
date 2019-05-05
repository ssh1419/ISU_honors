#sigma

sigma <- matrix(0, ncol = 10, nrow = 10)

for (j in 2:10) {
  for (i in 1:(j-1)) {
    sigma[i, j] <- 0.5^{abs(i-j)}
  }
}

sigma <- sigma + t(sigma)
diag(sigma) <- 2

X <- mvrnorm(n = 500, mu = rep(0, 10), Sigma = sigma)


temp <- X %*% c(2.5, 5, 0, 0, 3, 0, 0, 4, 6, 7) + rnorm(500)
pi <- exp(temp)/(1+exp(temp))


y <- rbinom(500, size = 1, prob = pi)


beta <- c(2.5, 5, 0, 0, 3, 0, 0, 4, 6, 7)



library(ncvreg)


#MCP
fit.1 <- ncvreg(X, y, penalty="MCP")
plot(fit.1,log=TRUE, main=expression(paste("MCP, ",gamma,"=",3)))

coef(fit.1)
summary(fit.1, lambda=0.05)

cvfit.1<-cv.ncvreg(X,y, penalty="MCP", family="binomial")
cvfit.1$Bias
plot(cvfit.1)
min(cvfit.1$cve)
abline(h = min(cvfit.1$cve))





#SCAD
fit.2 <- ncvreg(X, y, penalty="SCAD")
plot(fit.2, log=TRUE, main=expression(paste("SCAD, ",gamma,"=",3)))

coef(fit.2)
summary(fit.2, lambda=0.05)


cvfit.2<-cv.ncvreg(X,y, penalty="SCAD",family="binomial")
cvfit.2$Bias
plot(cvfit.2)
min(cvfit.2$cve)
abline(h = min(cvfit.2$cve))



#lasso
fit.3 <- ncvreg(X, y, penalty="lasso")
plot(fit.3,  log=TRUE, main=expression(paste("lasso, ",gamma,"=",3)))


coef(fit.3)
summary(fit.3, lambda=0.05)


cvfit.3<-cv.ncvreg(X,y, penalty="lasso",family="binomial")
cvfit.3$Bias
plot(cvfit.3)
min(cvfit.3$cve)
abline(h = min(cvfit.3$cve))



coef(fit.1)
summary(fit.1, lambda=0.05)


#whole repeat


library(MASS)


simul1=c()
simul2=c()
se1=c()


for (s in 1:500) {
  
  X <- mvrnorm(n = 500, mu = rep(0, 10), Sigma = sigma)
  
  
  temp <- X %*% c(2.5, 5, 0, 0, 3, 0, 0, 4, 6, 7) + rnorm(500)
  pi <- exp(temp)/(1+exp(temp))
  
  
  y <- rbinom(500, size = 1, prob = pi)
  
  
  
  cvfit.1<-cv.ncvreg(X,y, penalty="MCP", family="binomial")
  result1<-cvfit.1$Bias
  result2<- min(cvfit.1$cve)
  
  simul1=c(simul1,result1)
  simul2=c(simul2, result2)
  
  betaest1 <- coef(cvfit.1)
  result.se1 <- sum((betaest1[-1]-beta)^2)
  se1=c(result.se1)
  
  
  
}

mean(simul1)
mean(simul2)
mean(se1)

###########################################

simul3=c()
simul4=c()
se2=c()

for (s in 1:500) {
  
  
  X <- mvrnorm(n = 500, mu = rep(0, 10), Sigma = sigma)
  
  
  beta <- c(2.5, 5, 0, 0, 3, 0, 0, 4, 6, 7)
  temp <- X %*% c(2.5, 5, 0, 0, 3, 0, 0, 4, 6, 7) + rnorm(500)
  pi <- exp(temp)/(1+exp(temp))
  
  
  y <- rbinom(500, size = 1, prob = pi)
  
  
  
  cvfit.2<-cv.ncvreg(X,y, penalty="SCAD", family="binomial")
  result3<-cvfit.2$Bias
  result4<- min(cvfit.2$cve)
  
  
  betaest2 <- coef(cvfit.2)
  result.se2 <- sum((betaest2[-1]-beta)^2)
  se2=c(result.se2)
  
  simul3=c(simul3,result3)
  simul4=c(simul4, result4)
  
  
}

mean(simul3)
mean(simul4)
mean(se2)


#################################################


simul5=c()
simul6=c()
se3=c()


for (s in 1:500) {
  
  X <- mvrnorm(n = 500, mu = rep(0, 10), Sigma = sigma)
  
  
  
  temp <- X %*% c(2.5, 5, 0, 0, 3, 0, 0, 4, 6, 7) + rnorm(500)
  pi <- exp(temp)/(1+exp(temp))
  
  
  y <- rbinom(500, size = 1, prob = pi)
  
  
  
  cvfit.3<-cv.ncvreg(X,y, penalty="lasso", family="binomial")
  result5<-cvfit.3$Bias
  result6<- min(cvfit.3$cve)
  
  
  
  simul5=c(simul5,result5)
  simul6=c(simul6, result6)
  
  betaest3 <- coef(cvfit.3)
  result.se3 <- sum((betaest3[-1]-beta)^2)
  se3=c(result.se3)
  
  
  
}

mean(simul5)
mean(simul6)
mean(se3)


coef(cvfit.1)
coef(cvfit.2)
coef(cvfit.3)



