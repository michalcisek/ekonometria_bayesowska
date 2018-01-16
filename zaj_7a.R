install.packages("R2WinBUGS")
install.packages("R.utils")
install.packages("rjags")
install.packages("R2jags")

setwd("U:/WWW/ekonometria_bayesowska")

## Example: Forecasting as a missing data problem
## Who's going to win the 2016 presidential election?
## Data for Abramowitz "Time-for-Change" forecasting model:
##   incumbent.vote: Vote share of incumbent party candidate, major parties only
##   gdp.growth: Percent increase in GDP, Q1 to Q2 of election year
##   net.approval: President's June approval-disapproval rating from Gallup
##   two.terms: Has the incumbent party held the presidency for 2 or more terms?
## Note the NAs for the 2016 election

#MORE: http://pollyvote.com/en/components/econometric-models/time-for-change-model/

dat <- data.frame(year = c(2016,2012,2008,2004,2000,1996,1992,1988,1984,1980,1976,1972,1968,1964,1960,1956,1952,1948),
                  gdp.growth = c(NA,1.3,1.3,2.6,8,7.1,4.3,5.2,7.1,-7.9,3,9.8,7,4.7,-1.9,3.2,0.4,7.5),
                  net.approval = c(NA,-0.8,-37,-0.5,19.5,15.5,-18,10,20,-21.7,5,26,-5,60.3,37,53.5,-27,-6),
                  two.terms = c(1,0,1,0,1,0,1,1,0,0,1,0,1,0,1,0,1,1),
                  incumbent.vote = c(NA,52.0,46.3,51.2,50.3,54.7,46.5,53.9,59.2,44.7,48.9,61.8,49.6,61.3,49.9,57.8,44.5,52.4))

jags.lm <- function() {
  for (i in 1:N) {
    incumbent.vote[i] ~ dnorm(mu[i], tau)
    mu[i] <- b[1] + b[2]*gdp.growth[i] + b[3]*net.approval[i] + b[4]*two.terms[i]
  }
  # non-informative priors on b's
  for (j in 1:4) {
    b[j] ~ dnorm(0, 0.001)
  }  
  # conditional variance of y given x
  tau <- pow(sd, -2)
  sd ~ dunif(0, 100)
  # priors for missing 2016 x's: historical mean, precision (1/variance)
  gdp.growth[1] ~ dnorm(3.7, 0.05)
  net.approval[1] ~ dnorm(7.7, 0.002)
}

library(R2WinBUGS)
write.model(jags.lm, "jagslm.txt")

## Fit the model
library(R.utils)
attachLocally(dat)
N <- nrow(dat)
inits <- function() { list(b=c(50, rnorm(3))) }

Sys.getenv("JAGS_HOME")
Sys.setenv("JAGS_HOME"="C:/Users/atoroj/Desktop/JAGS/JAGS-4.2.0")
library(rjags)
library(R2jags)

jagsfit <- jags.parallel(data=c("incumbent.vote", "gdp.growth",
                                "net.approval", "two.terms", "N"), 
                         inits=inits, 
                         parameters.to.save=c("b", "sd", "incumbent.vote", 
                                              "gdp.growth", "net.approval"), 
                         model.file="jagslm.txt", 
                         n.chains=3, 
                         n.iter=10000)

## Look at the model output
jags.mod1 <- jagsfit$BUGSoutput
jags.mod1$mean$b                  # regression coefficients
jags.mod1$mean$incumbent.vote[1]  # forecast of 2016

## Probability the Democrat will win more than 50% of the vote?
## That's the proportion of simulated draws greater than 50.
dim(jags.mod1$sims.list$incumbent.vote)
h1<-hist(jags.mod1$sims.list$incumbent.vote[,1], breaks=30:70, col="gray",
         xlab="Democrat's predicted two-party vote share", main="")
table(jags.mod1$sims.list$incumbent.vote[,1]>50) / jags.mod1$n.sims




