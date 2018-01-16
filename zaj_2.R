### 1. WSTĘPNE DEFINICJE ###

#Instalacja i załadowanie pakietów (pakiet manipulate tworzy interaktywne wykresy i działa w RStudio)
rm(list=ls())
library(manipulate)

#Definiujemy potrzebne nam skalary z treci zadania
c_true <- 0.5
sigma_2 <- 0.3
n <- 20
c_prior <- 0
c_var_prior <- 0.05

#Wektor wielu równomiernie rozłożonych punktów z odcinka (0,1) jako rozpatrywana (na wykresach) dziedzina parametru c
c <- seq(from=-1.5, to=1.5, by=0.01)

#Definicje kolorów do wykresów za pomocš funkcji RGB
#aby sprawdzić jej działanie, wpisujemy ?RGB na konsoli
grey_area <- rgb(160, 160, 160, 80, names=NULL, maxColorValue=255)
grey_line <- rgb(80, 80, 80, 160, names=NULL, maxColorValue=255)
green_area <- rgb(24, 121, 104, 80, names=NULL, maxColorValue=255)
green_line <- rgb(13, 85, 72, 160, names=NULL, maxColorValue=255)



### 2. WYKRES GĘSTOCI A PRIORI PARAMETRU c ###

#Wykres z krzywš gęstoci a priori parametru c
plot(c, dnorm(c, mean=c_prior, sd=(c_var_prior^0.5)), las=1, lwd=2, bty="n", col=grey_area,
     ylim=c(0,3), type="l", ylab="gęstoć", 
     main=expression(paste("Rozkład ",italic("a priori")," parametru c")))
#c - wektor wartoci osi poziomej
#dnorm(c, mean=c_prior, sd=(c_var_prior^0.5)) - wartoci na osi pionowej (dnorm - gęstoć rozkładu normalnego)
#type="l" - wykres liniowy
#col - kolor linii
#lwd - gruboć linii
#ylab - opis osi pionowej
#ylim - zakres wartoci osi pionowej; c(0,3) to 2-elementowy wektor, składajšcy się z zera i trójki
#las=1 - sprawia, że liczby przy osiach sš napisane w poziomie
#bty="n" - rezygnacja z ramki dookoła wykresu
#main - tytuł
#paste - złšczenie zmiennych tekstowych będšcych kolejnymi argumentami funkcji
#italic - tekst pisany kursywš
#expression - zapewnia interpretowanie funkcji "italic" wewnštrz funkcji "paste" 
#dzięki temu nie otrzymujemy niesformatowanego napisu: Rozkład italic("a priori") parametru c

#Zapełnimy pole pod wykresem - musimy podać wektor punktów obwodzšcych kolorowane pole
#pierwszy argument to ich położenie na osi poziomej, drugi - na osi pionowej
polygon(c(c, rev(c)), c(dnorm(c, mean=c_prior, sd=(c_var_prior^0.5)), 
                        rep(0, length(c))), col=grey_area, border=NA)

#Dodajemy legendę, kolejne argumenty to:
#1. położenie legendy na wykresie (?legend - wpisujemy aby podejrzeć możliwe opcje)
#2. wektor napisów
#3. wektor kolorów
legend(x="topright",legend=c(expression(paste("prawdopodobieństwo ",italic("a priori")))),fill=c(grey_area))

#Dodajemy do wykresu linię o okrelonym kolorze (col) i gruboci (lwd) 
#jako linię pionowš (v) odpowiadajšcš wartoci c_prior na osi poziomej
abline(v=c_prior,col=grey_line,lwd=3)

#Dodajemy tekst opisujšcy tę linię - dwa pierwsze argumenty to położenia rodka tekstu
text(c_prior,max(dnorm(c, mean=c_prior, sd=(c_var_prior^0.5)))+1,paste("E(c) a priori =",c_prior),col=grey_line)



### 3. INTERAKTYWNY WYKRES GĘSTOCI A PRIORI ###

manipulate( 
  #Na poczštek tych samych 5 poleceń ujętych w klamrę
  {plot(c, dnorm(c, mean=c_prior, sd=(c_var_prior^0.5)), las=1, lwd=2, bty="n", col=grey_area,
        ylim=c(0,3), type="l", ylab="gęstoć", 
        main=expression(paste("Rozkład ",italic("a priori")," parametru c")))
    polygon(c(c, rev(c)), c(dnorm(c, mean=c_prior, sd=(c_var_prior^0.5)), 
                            rep(0, length(c))), col=grey_area, border=NA)
    legend(x="topright",legend=c(expression(paste("prawdopodobieństwo ",italic("a priori")))),fill=c(grey_area))
    abline(v=c_prior,col=grey_line,lwd=3)
    text(c_prior,max(dnorm(c, mean=c_prior, sd=(c_var_prior^0.5)))+1,paste("E(c) a priori =",c_prior),col=grey_line)},
  
  #Zasadnicza częć polecenia manipulate - wartoć oczekiwanš i odchylenie standardowe a priori... 
  #...będziemy mogli zmieniać suwakami, modyfikujšc wykres
  c_prior=slider(-4, 4, step=0.01, initial=0), 
  c_var_prior=slider(0.01, 1, step=0.01, initial=0.05) )


### 4. GĘSTOĆ A PRIORI I A POSTERIORI ###

manipulate(
  { #Losujemy wektor danych zgodnie z treciš zadania
    y <- c_true*rep(1,n) + rnorm(n,mean=0,sd=(sigma_2^0.5))
    #Gęstoć danych - funkcja sapply powtarza wielokrotnie to samo skalarne działanie na wektorze
    likelihood <- sapply(c, function(c) { prod(dnorm(y-c,mean=0,sd=(sigma_2^0.5))) } )
    like.rescale <- 3 * likelihood/max(likelihood)
    #Parametry rozkładu a posteriori
    c_posterior <- n*c_var_prior/(n*c_var_prior+sigma_2)*mean(y)+sigma_2/(n*c_var_prior+sigma_2)*c_prior
    c_var_posterior <- 1/((1/c_var_prior)+(n/sigma_2))
    
    #Wykres a priori
    plot(c, dnorm(c, mean=c_prior, sd=(c_var_prior^0.5)), las=1, lwd=2, bty="n", col=grey_area,
         ylim=c(0,8), type="l", ylab="gęstoć", 
         main=expression(paste("Rozkład ",italic("a priori")," i ",italic("a posteriori")," parametru c")))
    polygon(c(c, rev(c)), c(dnorm(c, mean=c_prior, sd=(c_var_prior^0.5)), 
                            rep(0, length(c))), col=grey_area, border=NA)
    
    abline(v=c_prior,col=grey_line,lwd=3)
    text(c_prior,max(dnorm(c, mean=c_prior, sd=(c_var_prior^0.5)))+1,paste("E(c) a priori =",c_prior),col=grey_line)
    #Wykres gęstoci
    lines(c, like.rescale, lwd=2, col=rgb(0,0,0,1))
    abline(v=mean(y),col=rgb(0,0,0,1),lwd=3)
    text(mean(y),max(like.rescale)+1,paste("rednia x = ",round(mean(y),4)),col=rgb(0,0,0,1))
    #Wykres a posteriori
    lines(c, dnorm(c, mean=c_posterior, sd=(c_var_posterior^0.5)), lwd=2, col=green_line)
    polygon(c(c, rev(c)), c(dnorm(c, mean=c_posterior, sd=(c_var_posterior^0.5)), 
                            rep(0, length(c))), col=green_area, border=NA)
    abline(v=c_posterior,col=green_line,lwd=3)
    text(c_posterior,max(dnorm(c, mean=c_posterior, sd=(c_var_posterior^0.5)))+2,paste("E(c) a posteriori =",round(c_posterior,digits=4)),col=green_line)
    #Legenda
    legend(x="topleft",legend=c(expression(paste("prawdopodobieństwo ",italic("a priori"))),expression(paste("prawdopodobieństwo ",italic("a posteriori"))),"gęstoć danych"),fill=c(grey_area,green_area,rgb(0,0,0,1))) },
  
  c_prior=slider(-0.5, 1, step=0.01, initial=0),
  c_var_prior=slider(0.01, 0.5, step=0.01, initial=0.05),
  c_true=slider(-0.5, 1, step=0.01, initial=0.5),
  n=slider(1,100,step=1,initial=20),
  sigma_2=slider(0.01,1, step=0.01,initial=0.3)   )

### 5. EKSPORT WYKRESU DO PLIKU PNG ###
# uzupełniamy powyższy kod o dwie linie
# w oczywisty sposób nie może to już być wykres interaktywny

#Pierwszy dodatkowy wiersz
png(file="wyklad2_a_posteriori.png",width=800,height=500,res=100)
##########################
manipulate(
  { y <- c_true*rep(1,n) + rnorm(n,mean=0,sd=(sigma_2^0.5))
  likelihood <- sapply(c, function(c) { prod(dnorm(y-c,mean=0,sd=(sigma_2^0.5))) } )
  like.rescale <- 3 * likelihood/max(likelihood)
  c_posterior <- n*c_var_prior/(n*c_var_prior+sigma_2)*mean(y)+sigma_2/(n*c_var_prior+sigma_2)*c_prior
  c_var_posterior <- 1/((1/c_var_prior)+(n/sigma_2))
  plot(c, dnorm(c, mean=c_prior, sd=(c_var_prior^0.5)), las=1, lwd=2, bty="n", col=grey_area,
       ylim=c(0,8), type="l", ylab="gęstoć", 
       main=expression(paste("Rozkład ",italic("a priori")," i ",italic("a posteriori")," parametru c")))
  polygon(c(c, rev(c)), c(dnorm(c, mean=c_prior, sd=(c_var_prior^0.5)), 
                          rep(0, length(c))), col=grey_area, border=NA)
  abline(v=c_prior,col=grey_line,lwd=3)
  text(c_prior,max(dnorm(c, mean=c_prior, sd=(c_var_prior^0.5)))+1,paste("E(c) a priori =",c_prior),col=grey_line)
  lines(c, like.rescale, lwd=2, col=rgb(0,0,0,1))
  abline(v=mean(y),col=rgb(0,0,0,1),lwd=3)
  text(mean(y),max(like.rescale)+1,paste("rednia x = ",round(mean(y),4)),col=rgb(0,0,0,1))
  lines(c, dnorm(c, mean=c_posterior, sd=(c_var_posterior^0.5)), lwd=2, col=green_line)
  polygon(c(c, rev(c)), c(dnorm(c, mean=c_posterior, sd=(c_var_posterior^0.5)), 
                          rep(0, length(c))), col=green_area, border=NA)
  abline(v=c_posterior,col=green_line,lwd=3)
  text(c_posterior,max(dnorm(c, mean=c_posterior, sd=(c_var_posterior^0.5)))+2,paste("E(c) a posteriori =",round(c_posterior,digits=4)),col=green_line)
  legend(x="topleft",legend=c(expression(paste("prawdopodobieństwo ",italic("a priori"))),expression(paste("prawdopodobieństwo ",italic("a posteriori"))),"gęstoć danych"),fill=c(grey_area,green_area,rgb(0,0,0,1))) },
  c_prior=slider(-0.5, 1, step=0.01, initial=0),
  c_var_prior=slider(0.01, 0.5, step=0.01, initial=0.05),
  c_true=slider(-0.5, 1, step=0.01, initial=0.5),
  n=slider(1,100,step=1,initial=20),
  sigma_2=slider(0.01,1, step=0.01,initial=0.3)   )
#Drugi dodatkowy wiersz
dev.off()
#######################
