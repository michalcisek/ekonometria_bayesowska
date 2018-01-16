#https://fred.stlouisfed.org/series/A191RL1Q225SBEA

dat <- data.frame(year = c(2016,2012,2008,2004,2000,1996,1992,1988,1984,1980,1976,1972,1968,1964,1960,1956,1952,1948),
                  gdp.growth = c(1.4,1.3,1.3,2.6,8,7.1,4.3,5.2,7.1,-7.9,3,9.8,7,4.7,-1.9,3.2,0.4,7.5),
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
  #gdp.growth[1] ~ dnorm(3.7, 0.05)
  net.approval[1] ~ dnorm(7.7, 0.002)
}

write.model(jags.lm, "jagslm.txt")

## Fit the model
attachLocally(dat)
N <- nrow(dat)
inits <- function() { list(b=c(50, rnorm(3))) }


jagsfit <- jags.parallel(data=c("incumbent.vote", "gdp.growth",
                                "net.approval", "two.terms", "N"), 
                         inits=inits, 
                         parameters.to.save=c("b", "sd", "incumbent.vote", 
                                              "gdp.growth", "net.approval"), 
                         model.file="jagslm.txt", 
                         n.chains=3, 
                         n.iter=10000)

## Look at the model output
jags.mod2 <- jagsfit$BUGSoutput
jags.mod2$mean$b                  # regression coefficients
jags.mod2$mean$incumbent.vote[1]  # forecast of 2016

## Probability the Democrat will win more than 50% of the vote?
## That's the proportion of simulated draws greater than 50.
dim(jags.mod2$sims.list$incumbent.vote)
h2<-hist(jags.mod2$sims.list$incumbent.vote[,1], breaks=30:70, col="gray",
         xlab="Democrat's predicted two-party vote share", main="")
table(jags.mod2$sims.list$incumbent.vote[,1]>50) / jags.mod2$n.sims

grey_line = rgb(80, 80, 80, 160, names=NULL, maxColorValue=255)
green_line = rgb(13, 85, 72, 160, names=NULL, maxColorValue=255)
plot(x=30.5:69.5, y=h1$density, col=grey_line, type="l",ylim=c(0,0.15), main="Rozkład predykcyjny dla wyborów w USA 2016",xlab="% głosów na kandydata partii rzšdzšcej",ylab="gęstoć predykcyjna")
lines(x=30.5:69.5, y=h2$density, col=green_line,lwd=3)
legend(x="right",fill=c(grey_line,green_line),legend=c("przed publikacjš za II kw.", "po publikacji PKB za II kw."))
abline(v=mean(jags.mod2$sims.list$incumbent.vote[,1]),col=green_line,lwd=3)
abline(v=mean(jags.mod1$sims.list$incumbent.vote[,1]),col=grey_line,lwd=1)
text(56,0.13,paste("mean=",round(mean(jags.mod1$sims.list$incumbent.vote[,1]),digits=2),"S.D.=",round((mean(jags.mod1$sims.list$incumbent.vote[,1]^2)-mean(jags.mod1$sims.list$incumbent.vote[,1])^2)^0.5,digits=2)),col=grey_line)
text(41,0.13,paste("mean=",round(mean(jags.mod2$sims.list$incumbent.vote[,1]),digits=2),"S.D.=",round((mean(jags.mod2$sims.list$incumbent.vote[,1]^2)-mean(jags.mod2$sims.list$incumbent.vote[,1])^2)^0.5,digits=2)),col=green_line)


