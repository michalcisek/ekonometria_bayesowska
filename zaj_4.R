#1. Wielowymiarowy rozkład normalny

install.packages("mvtnorm")
library(mvtnorm)
library(manipulate)

x <- seq(from=-2,to=2,by=0.1)
y <- seq(from=-2,to=2,by=0.1)
mu.x <- 0
mu.y <- 0
sigma.x <- 1
sigma.y <- 0.5
rho.xy <- 0

manipulate(
  {big.Sigma <- matrix(c(sigma.x^2,rho.xy*sigma.x*sigma.y,rho.xy*sigma.x*sigma.y,sigma.y^2),nrow=2,ncol=2)
  z=matrix(rep(0,length(x)*length(y)),nrow=length(x),ncol=length(y))
  for(ii in 1:length(x)) {
    for(jj in 1:length(y)) {
      z[ii,jj]=dmvnorm(c(x[ii],y[jj]),mean=c(mu.x,mu.y),sigma=big.Sigma,log=FALSE)
    }}
  
  persp(x, y, z, phi=obrot.w.pionie, theta=obrot.w.poziomie,
        xlab = "x", ylab = "y", zlab="gęstoć",
        main = "Rozkład normalny 2-wymiarowy")},
  obrot.w.pionie=slider(0,360,step=1,initial=30),
  obrot.w.poziomie=slider(0,360,step=1,initial=0),
  rho.xy=slider(-1,1,step=0.1,initial=0),
  sigma.x=slider(0.1,5,step=0.1,initial=1),
  sigma.y=slider(0.1,5,step=0.1,initial=0.5)
)

#2. Regresja OLS

setwd("U:/WWW/ekonometria_bayesowska")
dane<-read.csv("benzyna_USA.csv", header=TRUE, sep=",")
OLS_results <- lm(dane$log_Q_gasoline ~ dane$log_P_gasoline+dane$log_P_new_car+dane$log_P_used_car+dane$log_Income)
summary(OLS_results)

y <- as.matrix(dane[8:43,c('log_Q_gasoline')])
N.data <- length(y)
X <- cbind(as.matrix(rep(1,N.data)),as.matrix(dane[8:43,c('log_P_gasoline','log_P_new_car','log_P_used_car','log_Income')]))
Beta.ols.data <- OLS_results$coefficients
v.data <- OLS_results$df.residual
XTX.data <- t(X)%*%X
s2.data <- sum((OLS_results$residuals)^2)/v.data

#3. Parametry a priori

Beta.prior <- c(-22.5,-0.5,-0.5,-0.5,0.5)
sm2.prior <- 4 # s2.prior=0.25, s.prior=0.5
U.prior <- 0.4*diag(5)
U.prior[1,1] <- 200
v.prior <- 100

#4. Parametry a posteriori
Beta.posterior <- solve(solve(U.prior)+XTX.data)%*%(solve(U.prior)%*%Beta.prior+XTX.data%*%Beta.ols.data)
U.posterior <- solve(solve(U.prior)+XTX.data)
v.posterior <- v.prior + N.data
vs2.posterior <- v.prior*1/sm2.prior+v.data*s2.data+t(Beta.ols.data-Beta.prior)%*%solve(U.prior+solve(XTX.data))%*%(Beta.ols.data-Beta.prior)
sm2.posterior <- 1/(vs2.posterior/v.posterior)

#5. Gęstoci brzegowe

#5.1. Definiujemy funkcję gęstoci rozkładu t
#        (wielowymiarowa, ale korzystamy z 1-wymiarowego wariantu jako przypadku szczególnego)
density.t <- function(b,m,Scale,df){
  dimen <- length(m)
  d.t <- ((pi*df)^(-dimen/2))*gamma(dimen/2)/beta(df/2,dimen/2)*det(Scale)^(-0.5)*((1+1/df*t(b-m)%*%solve(Scale)%*%(b-m))^(-(df+dimen)/2))
  return(d.t)
}

#5.2. Dane do wykresów - gęstoć a priori i a posteriori
beta.space <- seq(from=-2,to=2,by=0.01)
n_eval_points <- length(beta.space)
n_parameters <- length(Beta.posterior)
prior.marg.dens.beta=matrix(NA,nrow=n_parameters,ncol=n_eval_points)
posterior.marg.dens.beta=matrix(NA,nrow=n_parameters,ncol=n_eval_points)
for(ii in 1:length(Beta.posterior)) {
  prior.marg.dens.beta[ii,] <- apply(as.matrix(beta.space), 1, density.t, m=Beta.prior[ii], Scale=as.matrix(U.prior[ii,ii]/sm2.prior), df=v.prior)
  posterior.marg.dens.beta[ii,] <- apply(as.matrix(beta.space), 1, density.t, m=Beta.posterior[ii], Scale=as.matrix(U.posterior[ii,ii]/sm2.posterior), df=v.posterior)
}

#5.3. Definicje kolorów do tworzenia wykresów
grey_area <- rgb(160, 160, 160, 80, names=NULL, maxColorValue=255)
grey_line <- rgb(80, 80, 80, 160, names=NULL, maxColorValue=255)
green_area <- rgb(24, 121, 104, 80, names=NULL, maxColorValue=255)
green_line <- rgb(13, 85, 72, 160, names=NULL, maxColorValue=255)

#5.4. Polecenie, które tworzy wykres składajšcy się z 4 paneli (2x2)
par(mfrow=c(2,2))
for(ii in 2:length(Beta.posterior)) {
  plot(beta.space, prior.marg.dens.beta[ii,], las=1, lwd=2, bty="n", col=grey_area,
       ylim=c(0,max(c(max(prior.marg.dens.beta[ii,]),max(posterior.marg.dens.beta[ii,])))+1), type="l", ylab="gęstoć", main=colnames(dane)[ii+1])
  polygon(c(beta.space, rev(beta.space)), c(prior.marg.dens.beta[ii,], 
                                            rep(0, length(beta.space))), col=grey_area, border=NA)
  abline(v=Beta.prior[ii],col=grey_line,lwd=3)
  text(Beta.prior[ii],max(prior.marg.dens.beta[ii,])+0.4,paste("E(beta) a priori =",Beta.prior[ii]),col=grey_line)
  abline(v=Beta.ols.data[ii],col=rgb(0,0,0,1),lwd=3)
  text(Beta.ols.data[ii],max(posterior.marg.dens.beta[ii,])+0.2,paste("parametr OLS = ",round(Beta.ols.data[ii],4)),col=rgb(0,0,0,1))
  lines(beta.space, posterior.marg.dens.beta[ii,], lwd=2, col=green_line)
  polygon(c(beta.space, rev(beta.space)), c(posterior.marg.dens.beta[ii,], 
                                            rep(0, length(beta.space))), col=green_area, border=NA)
  abline(v=Beta.posterior[ii],col=green_line,lwd=3)
  text(Beta.posterior[ii],max(posterior.marg.dens.beta[ii,])+0.6,paste("E(beta) a posteriori =",round(Beta.posterior[ii],digits=4)),col=green_line)
}
