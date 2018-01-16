rm(list=ls())
library(manipulate)

dane<-read.csv("prawo_okuna.csv", header=TRUE, sep=";")

y <- dane$y[44:80] - mean(dane$y[44:80],na.rm=TRUE)
u <- dane$u[45:81] - mean(dane$u[45:81],na.rm=TRUE)
OLS_results <- lm(u~y -1)
plot(u,y)
abline(OLS_results)
summary(OLS_results)

Sx.data <- sum(y^2)
beta.ols.data <- OLS_results$coefficients[1]
v.data <- OLS_results$df.residual
N.data <- length(y)
s2.data <- sum((OLS_results$residuals)^2)/v.data

beta.prior <- -0.4
u.prior <- 0.01
sm2.prior <- 1
v.prior <- 100

densityNG <- function(b,h,beta,u,sm2,v){
  dNG <- exp(-h/2*(1/u*((b-beta)^2)+v*1/sm2))*(h^((v-1)/2))
  return(dNG)
}

beta.space <- seq(from=-0.8,to=0.2,by=0.01)
gamma.space <- seq(from=0.1,to=2,by=0.1)

manipulate(
  {beta.posterior <- (beta.ols.data*Sx.data+beta.prior*1/u.prior)/(Sx.data+1/u.prior)
  u.posterior <- 1/(1/u.prior+Sx.data)
  v.posterior <- v.prior + N.data
  vs2.posterior <- v.prior/sm2.prior+v.data*s2.data+v.data*((beta.prior-beta.ols.data)^2)/(u.prior+1/Sx.data)
  sm2.posterior <- 1/(vs2.posterior/v.posterior)
  
  z=matrix(rep(0,length(beta.space)*length(gamma.space)),nrow=length(beta.space),ncol=length(gamma.space))
  for(ii in 1:length(beta.space)) {
    for(jj in 1:length(gamma.space)) {
      z[ii,jj]=densityNG(beta.space[ii],gamma.space[jj],beta.posterior,u.posterior,sm2.posterior,v.posterior)
    }}
  
  persp(beta.space, gamma.space, z, phi=obrot.w.pionie, theta=obrot.w.poziomie,
        xlab = "beta", ylab = "gamma", zlab="gęstoć",
        main = "Rozkład beta-h a posteriori (normalny-gamma)")},
  obrot.w.pionie=slider(0,360,step=1,initial=30),
  obrot.w.poziomie=slider(0,360,step=1,initial=0),
  beta.prior=slider(-1,-0.1,step=0.1,initial=-0.4),
  u.prior=slider(0.001,1,step=0.001,initial=0.01),
  sm2.prior=slider(0.1,2,step=0.1,initial=1),
  v.prior=slider(2,1000,step=1,initial=100)
)

density.t <- function(b,m,scale,df){
  d.t <- ((pi*df*scale)^(-1/2))*gamma(1/2)/beta(df/2,1/2)*(1+((b-m)^2)/(df*scale))^(-(df+1)/2)
  return(d.t)
}


grey_area <- rgb(160, 160, 160, 80, names=NULL, maxColorValue=255)
grey_line <- rgb(80, 80, 80, 160, names=NULL, maxColorValue=255)
green_area <- rgb(24, 121, 104, 80, names=NULL, maxColorValue=255)
green_line <- rgb(13, 85, 72, 160, names=NULL, maxColorValue=255)

manipulate(
  {beta.posterior <- (beta.ols.data*Sx.data+beta.prior*1/u.prior)/(Sx.data+1/u.prior)
  u.posterior <- 1/(1/u.prior+Sx.data)
  v.posterior <- v.prior + N.data
  vs2.posterior <- v.prior*1/sm2.prior+v.data*s2.data+((beta.prior-beta.ols.data)^2)/(u.prior+1/Sx.data)
  sm2.posterior <- 1/(vs2.posterior/v.posterior)
  
  posterior.marg.dens.beta=density.t(beta.space,beta.posterior,u.posterior*1/sm2.posterior,v.posterior)
  prior.marg.dens.beta=density.t(beta.space,beta.prior,u.prior*1/sm2.prior,v.prior)
  
  plot(beta.space, prior.marg.dens.beta, las=1, lwd=2, bty="n", col=grey_area,
       ylim=c(0,10), type="l", ylab="gęstoć", 
       main=expression(paste("Rozkład brzegowy ",italic("a priori")," i ",italic("a posteriori")," parametru beta")))
  polygon(c(beta.space, rev(beta.space)), c(prior.marg.dens.beta, 
                                            rep(0, length(beta.space))), col=grey_area, border=NA)
  abline(v=beta.prior,col=grey_line,lwd=3)
  text(beta.prior,max(prior.marg.dens.beta)+0.4,paste("E(beta) a priori =",beta.prior),col=grey_line)
  #lines(c, like.rescale, lwd=2, col=rgb(0,0,0,1))
  abline(v=beta.ols.data,col=rgb(0,0,0,1),lwd=3)
  text(beta.ols.data,max(posterior.marg.dens.beta)+0.2,paste("parametr OLS = ",round(beta.ols.data,4)),col=rgb(0,0,0,1))
  lines(beta.space, posterior.marg.dens.beta, lwd=2, col=green_line)
  polygon(c(beta.space, rev(beta.space)), c(posterior.marg.dens.beta, 
                                            rep(0, length(beta.space))), col=green_area, border=NA)
  abline(v=beta.posterior,col=green_line,lwd=3)
  text(beta.posterior,max(posterior.marg.dens.beta)+0.6,paste("E(beta) a posteriori =",round(beta.posterior,digits=4)),col=green_line)
  legend(x="topleft",legend=c(expression(paste("prawdopodobieństwo ",italic("a priori"))),expression(paste("prawdopodobieństwo ",italic("a posteriori")))),fill=c(grey_area,green_area)) },
  beta.prior=slider(-1,-0.1,step=0.1,initial=-0.4),
  u.prior=slider(0.001,1,step=0.001,initial=0.01),
  sm2.prior=slider(0.1,2,step=0.1,initial=1),
  v.prior=slider(2,1000,step=1,initial=100)
)

beta.posterior <- (beta.ols.data*Sx.data+beta.prior*1/u.prior)/(Sx.data+1/u.prior)
u.posterior <- 1/(1/u.prior+Sx.data)
v.posterior <- v.prior + N.data
vs2.posterior <- v.prior*1/sm2.prior+v.data*s2.data+((beta.prior-beta.ols.data)^2)/(u.prior+1/Sx.data)
sm2.posterior <- 1/(vs2.posterior/v.posterior)

posterior.marg.dens.beta=density.t(beta.space,beta.posterior,u.posterior*1/sm2.posterior,v.posterior)
prior.marg.dens.beta=density.t(beta.space,beta.prior,u.prior*1/sm2.prior,v.prior)
