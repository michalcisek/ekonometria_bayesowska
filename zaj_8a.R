#Instalacje pakietów i import danych
rm(list=ls())
# install.packages("mvtnorm")
dane<-read.csv("benzyna_USA.csv", header=TRUE, sep=",")

#Przywołanie przykładu
OLS_results <- lm(dane$log_Q_gasoline ~ dane$log_P_gasoline+dane$log_P_new_car+dane$log_P_used_car+dane$log_Income)
y <- as.matrix(dane[8:43,c('log_Q_gasoline')])
N.data <- length(y)
X <- cbind(as.matrix(rep(1,N.data)),as.matrix(dane[8:43,c('log_P_gasoline','log_P_new_car','log_P_used_car','log_Income')]))
Beta.ols.data <- OLS_results$coefficients
v.data <- OLS_results$df.residual
XTX.data <- t(X)%*%X
s2.data <- sum((OLS_results$residuals)^2)/v.data

Beta.prior <- c(-22.5,-0.5,-0.5,-0.5,0.5)
sm2.prior <- 4 # s2.prior=0.25, s.prior=0.5
U.prior <- 0.4*diag(5)
U.prior[1,1] <- 200
v.prior <- 100
vs2.prior <- v.prior/sm2.prior

beta.posterior <- solve(solve(U.prior)+XTX.data)%*%(solve(U.prior)%*%Beta.prior+XTX.data%*%Beta.ols.data)
U.posterior <- solve(solve(U.prior)+XTX.data)
v.posterior <- v.prior + N.data
vs2.posterior <- v.prior*1/sm2.prior+v.data*s2.data+t(Beta.ols.data-Beta.prior)%*%solve(U.prior+solve(XTX.data))%*%(Beta.ols.data-Beta.prior)
s2.posterior <- vs2.posterior/v.posterior

#Porównanie modeli
library(mvtnorm)

#pytanie 1
p.restrykcji1 <- as.numeric(pmvt(upper=0, sigma=as.matrix(s2.posterior*U.posterior[2,2]), df=v.posterior, delta=beta.posterior[2]))
PO.12 <- p.restrykcji1/(1-p.restrykcji1)
PO.12

#pytanie 2
p.restrykcji2 <- as.numeric(pmvt(upper=rep(0,2), sigma=as.numeric(s2.posterior)*(U.posterior[3:4,3:4]), df=v.posterior, delta=as.vector(beta.posterior[3:4]), type="shifted"))
PO.13 <- p.restrykcji2/(1-p.restrykcji2)
PO.13

#pytanie 3
R=diag(c(1,1,1,-1))
p.restrykcji <- as.numeric(pmvt(upper=rep(0,4), sigma=as.numeric(s2.posterior)*(R%*%U.posterior[2:5,2:5]%*%t(R)), df=v.posterior, delta=as.vector(R%*%beta.posterior[2:5]), type="shifted"))
PO.14 <- p.restrykcji/(1-p.restrykcji)
PO.14
