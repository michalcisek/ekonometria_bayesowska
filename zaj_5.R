#1. Przywołujemy przykład z poprzednich zajęć

#Potrzebny pakiet manipulate
library(manipulate)

setwd("U:/WWW/ekonometria_bayesowska")
dane<-read.csv("benzyna_USA.csv", header=TRUE, sep=",")
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

Beta.posterior <- solve(solve(U.prior)+XTX.data)%*%(solve(U.prior)%*%Beta.prior+XTX.data%*%Beta.ols.data)
U.posterior <- solve(solve(U.prior)+XTX.data)
v.posterior <- v.prior + N.data
vs2.posterior <- v.prior*1/sm2.prior+v.data*s2.data+t(Beta.ols.data-Beta.prior)%*%solve(U.prior+solve(XTX.data))%*%(Beta.ols.data-Beta.prior)
sm2.posterior <- 1/(vs2.posterior/v.posterior)

density.t <- function(b,m,Scale,df){
  dimen <- length(m)
  d.t <- ((pi*df)^(-dimen/2))*gamma(dimen/2)/beta(df/2,dimen/2)*det(Scale)^(-0.5)*((1+1/df*t(b-m)%*%solve(Scale)%*%(b-m))^(-(df+dimen)/2))
  return(d.t)
}

beta.space <- seq(from=-2,to=2,by=0.01)
n_eval_points <- length(beta.space)
n_parameters <- length(Beta.posterior)
prior.marg.dens.beta=matrix(NA,nrow=n_parameters,ncol=n_eval_points)
posterior.marg.dens.beta=matrix(NA,nrow=n_parameters,ncol=n_eval_points)
for(ii in 1:length(Beta.posterior)) {
  prior.marg.dens.beta[ii,] <- apply(as.matrix(beta.space), 1, density.t, m=Beta.prior[ii], Scale=as.matrix(U.prior[ii,ii]/sm2.prior), df=v.prior)
  posterior.marg.dens.beta[ii,] <- apply(as.matrix(beta.space), 1, density.t, m=Beta.posterior[ii], Scale=as.matrix(U.posterior[ii,ii]/sm2.posterior), df=v.posterior)
}

# 2. Szkicujemy HPDI wokół każdego z 4 współczynników kierunkowych równania regresji

grey_area <- rgb(160, 160, 160, 80, names=NULL, maxColorValue=255)
grey_line <- rgb(80, 80, 80, 160, names=NULL, maxColorValue=255)
green_area <- rgb(24, 121, 104, 80, names=NULL, maxColorValue=255)
green_line <- rgb(13, 85, 72, 160, names=NULL, maxColorValue=255)
red_area <- rgb(255, 100, 123, 80, names=NULL, maxColorValue=255)
red_line <- rgb(200, 0, 30, 160, names=NULL, maxColorValue=255)

for(ii in 2:length(Beta.posterior)){
  manipulate( 
    {#Tworzymy zmiennš binarnš wskazujšcš, gdzie będzie HPDI - tzn. o najwyżej gęstoci a posteriori ponad zadany poziom
      credible_set_indicator=as.vector(as.integer(posterior.marg.dens.beta[ii,]>line_level))
      credible_set_begin=match(1,credible_set_indicator)
      credible_set_end=length(credible_set_indicator)-match(1,rev(credible_set_indicator))
      #Lewy i prawy brzeg HPDI
      x1=beta.space[credible_set_begin]
      x2=beta.space[credible_set_end]
      #Na potrzeby wykresu tworzymy wektor, który przyjmuje wartoć gęstoci a posteriori w HPDI i zero poza nim
      posterior.cs=posterior.marg.dens.beta[ii,]*credible_set_indicator
      #Poziom ufnoci
      HPDI_probab=sum(posterior.cs)*0.01
      #Wykres gęstoci a posteriori
      plot(beta.space, posterior.marg.dens.beta[ii,], las=1, lwd=2, bty="n", col=green_line,
           ylim=c(0,max(posterior.marg.dens.beta[ii,]+1)), type="l", ylab="gęstoć", main=colnames(dane)[ii+1])
      polygon(c(beta.space, rev(beta.space)), c(posterior.marg.dens.beta[ii,], 
                                                rep(0, length(beta.space))), col=green_area, border=NA)
      text(Beta.posterior[ii],max(posterior.marg.dens.beta[ii,])+0.6,paste("E(beta) a posteriori =",round(Beta.posterior[ii],digits=4)),col=green_line)
      abline(v=Beta.posterior[ii],col=green_line,lwd=3)
      #Linia pozioma odcinajšca "najwyższe" gęstoci a posteriori (HPD)
      abline(h=line_level,col=red_line,lwd=3)
      #Pole oznaczajšce gęstoć a posteriori w przedziale ufnoci HPDI
      polygon(c(beta.space, rev(beta.space)), c(posterior.cs, 
                                                rep(0, length(beta.space))), col=red_area, border=NA)
      
      #Wywietl poziom ufnoci i granice przedziału
      text(-1.2,2.3,paste(round(HPDI_probab*100,digits=1), "% przedział HPDI: (",round(x1,digits=2)," , ", round(x2,digits=2),")"),col=red_line)
    },
    line_level=slider(0, max(posterior.marg.dens.beta[ii,])+0.002, step=0.001, initial=max(posterior.marg.dens.beta[ii,])+0.001)
  )
}

# 3. Wyznaczamy wiarygodnoci brzegowe 2 modeli: ze wszystkimi zmiennymi i bez zmiennej log_Income

## Model 0: ze wszystkimi zmiennymi
#obliczenia mamy już gotowe
P_y_M0 = ((det(U.posterior)^0.5)*gamma(v.posterior/2)*((vs2.posterior)^(-v.posterior/2)))/((pi^(N.data/2))*(det(U.prior)^0.5)*gamma(v.prior/2)*((vs2.prior)^(-v.prior/2)))

## Model 1: bez zmiennej log_Income
#tutaj musimy przejć od poczštku przez zawężony model...
OLS_results_1 <- lm(dane$log_Q_gasoline ~ dane$log_P_gasoline+dane$log_P_new_car+dane$log_P_used_car)
X_1 <- cbind(as.matrix(rep(1,N.data)),as.matrix(dane[8:43,c('log_P_gasoline','log_P_new_car','log_P_used_car')]))
Beta.ols.data_1 <- OLS_results_1$coefficients
v.data_1 <- OLS_results_1$df.residual
XTX.data_1 <- t(X_1)%*%X_1
s2.data_1 <- sum((OLS_results_1$residuals)^2)/v.data_1

Beta.prior_1 <- c(-22.5,-0.5,-0.5,-0.5)
sm2.prior_1 <- 4 # s2.prior=0.25, s.prior=0.5
U.prior_1 <- 0.4*diag(4)
U.prior_1[1,1] <- 200
v.prior_1 <- 100
vs2.prior_1 <- v.prior_1/sm2.prior_1

Beta.posterior_1 <- solve(solve(U.prior_1)+XTX.data_1)%*%(solve(U.prior_1)%*%Beta.prior_1+XTX.data_1%*%Beta.ols.data_1)
U.posterior_1 <- solve(solve(U.prior_1)+XTX.data_1)
v.posterior_1 <- v.prior_1 + N.data
vs2.posterior_1 <- v.prior_1*1/sm2.prior_1+v.data_1*s2.data_1+t(Beta.ols.data_1-Beta.prior_1)%*%solve(U.prior_1+solve(XTX.data_1))%*%(Beta.ols.data_1-Beta.prior_1)
sm2.posterior_1 <- 1/(vs2.posterior_1/v.posterior_1)

#Tak jak w modelu 0 obliczamy marginal likelihood:
P_y_M1 = ((det(U.posterior_1)^0.5)*gamma(v.posterior_1/2)*((vs2.posterior_1)^(-v.posterior_1/2)))/((pi^(N.data/2))*(det(U.prior_1)^0.5)*gamma(v.prior_1/2)*((vs2.prior_1)^(-v.prior_1/2)))

#Czynnik Bayesa - porównujemy model 0 z 1:
BF_0_1 = P_y_M0/P_y_M1




