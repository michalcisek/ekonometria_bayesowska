rm(list=ls())
dane<-read.csv2("oscary.txt")
dane$oscar_actor<-ifelse(dane$oscar_actor==0,0,1)
dane$budget<-dane$budget/1000000
dane$opening_weekend<-dane$opening_weekend/1000000
dane$gross<-dane$gross/1000000

dane<-dane[complete.cases(dane) & dane$rok>=1990,]


library(Boruta)
boruta<-Boruta(gross ~ opening_weekend+Oscar+rok+budget+czas+rating+komentarze+zewnetrzne_zapowiedzi+oscar_actor
   ,data=dane)

plot(boruta,xlab="",xaxt="n")
lz<-lapply(1:ncol(boruta$ImpHistory),function(i)boruta$ImpHistory[is.finite(boruta$ImpHistory[,i]),i])
names(lz) <- colnames(boruta$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=3,labels = names(Labels),
       at = 1:ncol(boruta$ImpHistory), cex.axis = 0.7)

ols.results <- lm(gross ~ opening_weekend+budget+rating+komentarze+zewnetrzne_zapowiedzi+czas
                  ,data=dane)
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
beta1 <- losowanie.beta[,2] # opening_weekend
beta2 <- losowanie.beta[,3] # budget
beta3 <- losowanie.beta[,4] # rating
beta4 <- losowanie.beta[,5] # komentarze
beta5 <- losowanie.beta[,6] # zewnetrzne_zapowiedzi
beta6 <- losowanie.beta[,7] # czas

#Importance sampling
important.beta <- losowanie.beta[beta1>0 & beta2>0 & beta3>0,]
scaling.factor <- nrow(important.beta)/nrow(losowanie.beta)

grey_line = rgb(80, 80, 80, 160, names=NULL, maxColorValue=255)
green_line = rgb(13, 85, 72, 160, names=NULL, maxColorValue=255)
#Ilustracja gęstoci a posteriori dla parametru przy opening weekend
important.beta1 <- important.beta[,2]
restricted.h<-hist(important.beta1, #breaks=(-1:1)/100, 
                   col="gray", 
                   xlab="opening_weekend: parametr", 
                   main="Ucięty rozkład a priori")
unrestricted.h<-hist(beta1,  col="gray", 
                     xlab="opening_weekend: parametr", 
                     main="Skrajnie nieinformacyjny rozkład a priori")
plot(x=restricted.h$breaks[-1], y=restricted.h$density, col=green_line, type="l", lwd=3, main="Rozkłady a posteriori parametru beta1 (opening_weekend)",xlab="wartość parametru beta1",ylab="gęstość")
lines(x=unrestricted.h$breaks[-1], y=unrestricted.h$density, col=grey_line,lwd=1)
legend(x="right",fill=c(grey_line,green_line),legend=c("nieinformacyjny rozkład N-G", "restrykcje narzucone a priori"))
abline(v=mean(important.beta1),col=green_line,lwd=3,lty=3)
abline(v=mean(beta1),col=grey_line,lwd=1,lty=3)

#Ilustracja gęstoci a posteriori dla parametru budget
important.beta2 <- important.beta[,3]
restricted.h<-hist(important.beta2,  col="gray", xlab="budget: parametr", main="Ucięty rozkład a priori")
unrestricted.h<-hist(beta2, col="gray", xlab="budget: parametr", main="Skrajnie nieinformacyjny rozkład a priori")
plot(x=restricted.h$breaks[-1], y=restricted.h$density, col=green_line, type="l", lwd=3, main="Rozkłady a posteriori parametru beta2 (budget)",xlab="wartość parametru beta2",ylab="gęstość")
lines(x=unrestricted.h$breaks[-1], y=unrestricted.h$density, col=grey_line,lwd=1)
legend(x="right",fill=c(grey_line,green_line),legend=c("nieinformacyjny rozkład N-G", "restrykcje narzucone a priori"))
abline(v=mean(important.beta2),col=green_line,lwd=3,lty=3)
abline(v=mean(beta2),col=grey_line,lwd=1,lty=3)

#Ilustracja gęstoci a posteriori dla parametru rating
important.beta3 <- important.beta[,4]
restricted.h<-hist(important.beta3,  col="gray", xlab="rating: parametr", main="Ucięty rozkład a priori")
unrestricted.h<-hist(beta3, col="gray", xlab="rating: parametr", main="Skrajnie nieinformacyjny rozkład a priori")
plot(x=restricted.h$breaks[-1], y=restricted.h$density,col=green_line, type="l", lwd=3, main="Rozkłady a posteriori parametru beta3 (rating)",xlab="wartość parametru beta3",ylab="gęstość", xlim=c(-20,50))
lines(x=unrestricted.h$breaks[-1], y=unrestricted.h$density, col=grey_line,lwd=1)
legend(x="right",fill=c(grey_line,green_line),legend=c("nieinformacyjny rozkład N-G", "restrykcje narzucone a priori"))
abline(v=mean(important.beta3),col=green_line,lwd=3,lty=3)
abline(v=mean(beta3),col=grey_line,lwd=1,lty=3)

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


