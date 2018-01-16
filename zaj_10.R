rm(list=ls())
install.packages("moments")
install.packages("mvtnorm")
install.packages("R2WinBUGS")
install.packages("R.utils")
install.packages("rjags")
install.packages("R2jags")
install.packages("coda")


N <- 10000
eps_sd <- 2
v <- 4
library(moments)
epsilon_n <- rnorm(n=N,mean=0,sd=eps_sd)
jarque.test(epsilon_n)
library(mvtnorm)
scale_t <- (v-2)/v*eps_sd^2
epsilon_t <- rmvt(n=N,delta=as.vector(0),sigma=as.matrix(scale_t),df=v,type="shifted")
jarque.test(as.vector(epsilon_t))

epsilon_n_plot <- epsilon_n
epsilon_n_plot[(epsilon_n_plot<(-20) | epsilon_n_plot>20)]<-NA
epsilon_t_plot <- epsilon_t
epsilon_t_plot[(epsilon_t_plot<(-20) | epsilon_t_plot>20)]<-NA
h_n<-hist(epsilon_n_plot,breaks=-20:20, col="gray")
h_t<-hist(epsilon_t_plot,breaks=-20:20, col="gray")

x1 <- rnorm(n=N,mean=10,sd=3)
x2 <- rpois(n=N,lambda=3)
X <- cbind(matrix(1,N,1),x1,x2)
beta <- matrix(c(-3,2,0.5))
y <- X%*%beta

y_n <- y+epsilon_n
y_t <- y+epsilon_t

data <- data.frame(y_t,y_n,x1,x2,epsilon_t,epsilon_n)

reg_OLS_with_t <- lm(y_t~x1+x2)
summary(reg_OLS_with_t)
confint(reg_OLS_with_t)
jarque.test(reg_OLS_with_t$residuals)

jags.lm <- function() {
  for (i in 1:10000) {
    y_t[i] ~ dt(mu[i], tau, 4)
    mu[i] <- b[1] + b[2]*x1[i] + b[3]*x2[i]
  }
  for (j in 1:3) {
    b[j] ~ dt(0, 0.001, 4)
  }  
  tau <- pow(sd, -2)
  sd ~ dchisq(10)
}

library(R2WinBUGS)
write.model(jags.lm, "jagslm.txt")

## Fit the model
library(R.utils)
attachLocally(data)
inits <- function() { list(b=c(0,0,0)) }

Sys.setenv("JAGS_HOME"="D:Program Files/JAGS/JAGS-4.2.0")
library(rjags)
library(R2jags)

t.0<-Sys.time()
jagsfit <- jags.parallel(data=c("y_t", "x1",
                                "x2", "N"), 
                         inits=inits, 
                         parameters.to.save=c("b", "sd"), 
                         model.file="jagslm.txt",
                         DIC=TRUE,
                         n.iter=5000,
                         n.chains=2, 
                         n.burnin=2000,
                         n.thin=5)
t.1<-Sys.time()
t.1-t.0

mean.b <- jagsfit$BUGSoutput$mean$b
chain.b <- mcmc(jagsfit$BUGSoutput$sims.list$b)
library(coda)
hpdi.b <- HPDinterval(chain.b)
mean.hpdi.b <- cbind(mean.b,hpdi.b)
rownames(mean.hpdi.b)<-c("(Intercept)","x1","x2")
mean.hpdi.b
width95n<-confint(reg_OLS_with_t)[,2]-confint(reg_OLS_with_t)[,1]
width95n
width95t<-mean.hpdi.b[,3]-mean.hpdi.b[,2]
width95t
width95n/width95t

combined.chains = mcmc.list(mcmc(jagsfit$BUGSoutput$sims.array[,1,1:3]),mcmc(jagsfit$BUGSoutput$sims.array[,2,1:3]))
plot(combined.chains)
summary(combined.chains, quantiles = c(0.025, 0.25, 0.5, 0.75, 0.975))

#Numerical standard error (Koop, p. 65)
S0 <- spectrum0(combined.chains)
S_1 <- dim(jagsfit$BUGSoutput$sims.array)[1]
numerical.SE <- (S0$spec/S_1)^0.5
#Raftery and Lewis diagnostics
raftery.diag(combined.chains, q=0.5, r=0.001, s=0.95)

#Scale reduction factors (Koop, p. 66)
gelman.diag(combined.chains)
gelman.plot(combined.chains)
#Geweke statistics (Koop, p. 68)
geweke.diag(combined.chains, frac1=0.1, frac2=0.5)
geweke.plot(combined.chains, frac1 = 0.1, frac2 = 0.5, nbins=40, pvalue=0.05)
#Heidel
heidel.diag(combined.chains)

#Serial correlation in chains
autocorr(combined.chains, lags = c(0, 1, 5, 10, 50), relative=TRUE)
autocorr.diag(combined.chains)
autocorr.plot(combined.chains, lag.max=50)
#Cross correlation in chains
crosscorr(combined.chains)
crosscorr.plot(combined.chains)

#Deviance Information Criterion
DIC <- jagsfit$BUGSoutput$DIC
DIC
