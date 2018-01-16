#Instalacja i załadowanie pakietów (pakiet manipulate tworzy interaktywne wykresy i działa w RStudio)
# install.packages("manipulate")
rm(list=ls())
library(manipulate)

#Wektor wielu równomiernie rozłożonych punktów z odcinka (0,1) jako rozpatrywana dziedzina parametru p
p <- seq(from=0.005, to=0.995, by=0.005)

#Wartoć oczekiwana i odchylenie standardowe parametru (a priori)
p_prior=0.696
p_sd_prior=0.15
#W dalszej częci obliczeń posługujemy się następujšcym przeliczeniem na parametry alpha i beta rozkładu beta:
#alpha_prior <- (p_prior^2)*(1-p_prior)/(p_sd_prior^2)-p_prior
#beta_prior <- (p_prior)*((1-p_prior)^2)/(p_sd_prior^2)-(1-p_prior)

#Definicje kolorów do wykresów
grey_area = rgb(160, 160, 160, 80, names=NULL, maxColorValue=255)
grey_line = rgb(80, 80, 80, 160, names=NULL, maxColorValue=255)
green_area = rgb(24, 121, 104, 80, names=NULL, maxColorValue=255)
green_line = rgb(13, 85, 72, 160, names=NULL, maxColorValue=255)

#Rozkład a priori
png(file="01_a_priori.png",width=800,height=500,res=100)
manipulate( 
  #Zaczynamy konstruować wykres - plot
  {plot(p, rep(0,199), lwd=2, las=1, bty="n", col=grey_area,
        ylim=c(0,8), type="l", ylab="gęstoć", 
        main=substitute(paste("Rozkład ",italic("a priori")," prawdopodobieństwa porażki Polski")))
    
    #Obliczamy parametry a priori (należy to zrobić w bloku manipulate / plot!)
    alpha_prior <- (p_prior^2)*(1-p_prior)/(p_sd_prior^2)-p_prior
    beta_prior <- (p_prior)*((1-p_prior)^2)/(p_sd_prior^2)-(1-p_prior)
    
    #Szkicujemy pole pod wykresem
    polygon(c(p, rev(p)), c(dbeta(p, alpha_prior, beta_prior), 
                            rep(0, length(p))), col=grey_area, border=NA)
    
    #Zaznaczamy wartoć oczekiwanš
    abline(v=p_prior,col=grey_line,lwd=3)
    text(p_prior,max(dbeta(p, alpha_prior, beta_prior))+1,paste("E(p) a priori =",p_prior),col=grey_line)
    
    #Legenda wykresu
    legend(x="top",legend=c(expression(paste("prawdopodobieństwo ",italic("a priori")))),fill=c(grey_area))},
  
  #Zasadnicza częć polecenia manipulate - wartoć oczekiwanš i odchylenie standardowe a priori... 
  #...będziemy mogli zmieniać suwakami, modyfikujšc wykres
  p_prior=slider(0.01, 1, step=0.01, initial=0.696), 
  p_sd_prior=slider(0.01, 0.5, step=0.01, initial=0.15) )
dev.off()

#Dane: 3 mecze, 0 porażek
y=c(0,0,0)
n <- length(y)
k <- sum(y)

#Rozkład a posteriori
likelihood <- sapply(p, function(p) { prod(p^y * (1-p)^(1-y)) } )
like.rescale <- 3 * likelihood/max(likelihood)
alpha_posterior <- (p_prior^2)*(1-p_prior)/(p_sd_prior^2)-p_prior + k
beta_posterior <- (p_prior)*((1-p_prior)^2)/(p_sd_prior^2)-(1-p_prior) + n - k
p_posterior <- alpha_posterior/(alpha_posterior+beta_posterior)
p_sd_posterior <- (alpha_posterior*beta_posterior)/(((alpha_posterior+beta_posterior)^2)*(alpha_posterior+beta_posterior+1))

png(file="01_a_posteriori.png",width=800,height=500,res=100)
manipulate(
  {plot(p, like.rescale, lwd=2, las=1, bty="n", 
        ylim=c(0,8), type="l", ylab="gęstość",
        main=substitute(paste("Rozkład ",italic("a priori")," i ",italic("a posteriori")," prawdopodobieństwa porażki Polski")))
    
    alpha_prior <- (p_prior^2)*(1-p_prior)/(p_sd_prior^2)-p_prior
    beta_prior <- (p_prior)*((1-p_prior)^2)/(p_sd_prior^2)-(1-p_prior)
    alpha_posterior <- alpha_prior + k
    beta_posterior <- beta_prior + n - k
    p_posterior <- alpha_posterior/(alpha_posterior+beta_posterior)
    
    lines(p, dbeta(p, alpha_prior, beta_prior), col=grey_area, lwd=2)
    polygon(c(p, rev(p)), c(dbeta(p, alpha_prior, beta_prior), 
                            rep(0, length(p))), col=grey_area, border=NA)
    lines(p, dbeta(p, alpha_posterior, beta_posterior), col=green_area, lwd=2)
    polygon(c(p, rev(p)), c(dbeta(p, alpha_posterior, beta_posterior), 
                            rep(0, length(p))), col=green_area, border=NA)
    lines(p, like.rescale, lwd=2)
    
    abline(v=p_prior,col=grey_line,lwd=3)
    text(p_prior,max(dbeta(p, alpha_prior, beta_prior))+1,paste("E(p) a priori =",p_prior),col=grey_line)
    abline(v=p_posterior,col=green_line,lwd=3)
    text(p_posterior,max(dbeta(p, alpha_posterior, beta_posterior))+2,paste("E(p) a posteriori =",round(p_posterior,digits=4)),col=green_line)
    legend(x="top",legend=c(expression(paste("prawdopodobieństwo ",italic("a priori"))),expression(paste("prawdopodobieństwo ",italic("a posteriori"))),"wartosć funkcji wiarygodnosci danych (przeskalowana)"),fill=c(grey_area,green_area,rgb(0,0,0,1)))},
  
  p_prior=slider(0.01, 1, step=0.01, initial=0.696), 
  p_sd_prior=slider(0.01, 0.5, step=0.01, initial=0.15) )
dev.off()
