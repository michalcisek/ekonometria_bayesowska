#Instalacje pakietów i import danych
rm(list=ls())
# install.packages("mvtnorm")
# install.packages("coda")
dane <- read.csv2("ceny_samochodow.csv", header = TRUE, sep = ",",dec = ".")
grey_line = rgb(80, 80, 80, 160, names=NULL, maxColorValue=255)
green_line = rgb(13, 85, 72, 160, names=NULL, maxColorValue=255)

#Podejcie klasyczne
ols.results <- lm(log(cena)~produkcja+log(moc)+log(przebieg)+bezwypadkowy+wlasciciel+paliwo+skrzynia,data=dane)
summary(ols.results)


#Ustalamy parametry rozkładu a posteriori ze skrajnie nieinformacyjnym rozkładem a priori N-G
ols.beta <- ols.results$coefficients
ols.sigma <- vcov(ols.results)
ols.sum.sq.residuals <- sum(ols.results$residuals^2)

v.posterior <- nrow(dane)-length(ols.beta)
beta.posterior <- ols.beta
U.posterior <- ols.sigma/(ols.sum.sq.residuals/v.posterior)
s2.posterior <- ols.sum.sq.residuals/v.posterior

#Próbkowanie z funkcji q
library(mvtnorm)
S <- 10^5
set.seed(1)
losowanie.beta <- rmvt(S, sigma=s2.posterior*U.posterior, df=v.posterior, delta=beta.posterior, type="shifted")

beta0 <- losowanie.beta[,1] # stała
beta1 <- losowanie.beta[,2] # produkcja
beta2 <- losowanie.beta[,3] # ln(moc)
beta3 <- losowanie.beta[,4] # ln(przebieg)
beta4 <- losowanie.beta[,5] # bezwypadkowy
beta5 <- losowanie.beta[,6] # wlasciciel
beta6 <- losowanie.beta[,7] # paliwo
beta7 <- losowanie.beta[,8] # skrzynia

#Importance sampling
important.beta <- losowanie.beta[beta1>0 & beta2>0 & beta3<0 & beta4>0 & beta5>0 & beta6>0 & beta7>0,]
scaling.factor <- nrow(important.beta)/nrow(losowanie.beta)

#Ilustracja gęstoci a posteriori dla parametru przy automatycznej skrzyni biegów
important.beta7 <- important.beta[,8]
restricted.h<-hist(important.beta7, breaks=(-30:30)/100, 
                   col="gray", 
                   xlab="automatyczna skrzynia biegów: parametr", 
                   main="Ucięty rozkład a priori")
unrestricted.h<-hist(beta7, breaks=(-30:30)/100, col="gray", 
                     xlab="automatyczna skrzynia biegów: parametr", 
                     main="Skrajnie nieinformacyjny rozkład a priori")
plot(x=(-29:30)/100, y=restricted.h$density, col=green_line, type="l", lwd=3, main="Rozkłady a posteriori parametru beta7 (automatyczna skrzynia biegów)",xlab="wartoć parametru beta7",ylab="gęstoć")
lines(x=(-29:30)/100, y=unrestricted.h$density, col=grey_line,lwd=1)
legend(x="left",fill=c(grey_line,green_line),legend=c("nieinformacyjny rozkład N-G", "restrykcje narzucone a priori"))
abline(v=mean(important.beta7),col=green_line,lwd=3,lty=3)
abline(v=mean(beta7),col=grey_line,lwd=1,lty=3)

#Ilustracja gęstoci a posteriori dla parametru przy pierwszym włacicielu
important.beta5 <- important.beta[,6]
restricted.h<-hist(important.beta5, breaks=(-30:30)/100, col="gray", xlab="pierwszy właciciel: parametr", main="Ucięty rozkład a priori")
unrestricted.h<-hist(beta5, breaks=(-30:30)/100, col="gray", xlab="pierwszy właciciel: parametr", main="Skrajnie nieinformacyjny rozkład a priori")
plot(x=(-29:30)/100, y=restricted.h$density, col=green_line, type="l", lwd=3, main="Rozkłady a posteriori parametru beta5 (pierwszy właciciel)",xlab="wartoć parametru beta5",ylab="gęstoć")
lines(x=(-29:30)/100, y=unrestricted.h$density, col=grey_line,lwd=1)
legend(x="left",fill=c(grey_line,green_line),legend=c("nieinformacyjny rozkład N-G", "restrykcje narzucone a priori"))
abline(v=mean(important.beta5),col=green_line,lwd=3,lty=3)
abline(v=mean(beta5),col=grey_line,lwd=1,lty=3)

library(coda)  #Pakiet służšcy do wyliczenia przedziałów HPD (Plummer et al., 2006)

beta.restricted <- apply(important.beta,2,mean)
hpd.restricted <- HPDinterval(mcmc(important.beta)) #funkcja liczšca bayesowskie przedziały HPD (95%)
restricted.wyniki <- cbind(beta.restricted,hpd.restricted)
rownames(restricted.wyniki) <- names(ols.beta);
colnames(restricted.wyniki) <- c("Oszacowanie z ograniczeniami","Dolna granica HPD", "Górna granica HPD");
round(restricted.wyniki,digits=3)

beta.unrestricted <- apply(losowanie.beta,2,mean)
hpd.unrestricted <- HPDinterval(mcmc(losowanie.beta)) 
unrestricted.wyniki <- cbind(beta.unrestricted,hpd.unrestricted)
rownames(unrestricted.wyniki) <- names(ols.beta)
colnames(unrestricted.wyniki) <- c("Oszacowanie bez ograniczeń","Dolna granica HPD", "Górna granica HPD");
round(unrestricted.wyniki,digits=3)

confint(ols.results)


