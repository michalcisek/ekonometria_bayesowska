rm(list=ls())
#install.packages("BMS") #za pierwszym razem konieczna jest instalacja pakietu - usuń #
library(BMS)
setwd("C:/Users/lenovo/Desktop/Prezentacja BMA")


######################################################################
##############     Wczytywanie danych        #########################
######################################################################

data.raw1=read.csv2("dane.csv",header=TRUE,sep=";")
panelDat1=data.raw1[,-c(1:2)] #usuwamy kolumny ze zmiennymi identyfikacyjnymi


######################################################################
#####################    Obliczenia    ###############################
######################################################################

?bms #opis ustawień parametrów
modelCd1=bms(panelDat1, g="UIP", mprior="random", mprior.size =6, mcmc="enumerate", nmodel=500)
image(modelCd1) #podsumowanie wyników
plotModelsize(modelCd1) 



#################################################
########    Funkcje   ###########################

panel_unstack= function(stackeddata, tstep=NULL) {
  # ta funkacj utworzy następujšcš macierz danych
  #               Zmienna1  Zmienna2
  # kraj1_rok1
  # kraj1_rok2
  # ...
  # kraj2_rok1
  # kraj2_rok2
  # ...
  # tstep okrela ile lat obejmuje panel (wymiar czasu)
  # panel_unstack tworzy trójwymiarowš tablicę danych na tej podstawie
  
  bigT=nrow(stackeddata);K=ncol(stackeddata);
  if (is.null(tstep)) tstep=bigT
  X1=aperm(array(as.vector(t(as.matrix(stackeddata))),dim=c(K,tstep,bigT/tstep)), perm=c(2,1,3))
  try(dimnames(X1)[[1]] <-  unique(sapply(strsplit(rownames(stackeddata),"_"),
                                          function(x) x[[2]])), silent=TRUE)
  try(dimnames(X1)[[2]] <-  colnames(stackeddata), silent=TRUE)
  try(dimnames(X1)[[3]] <-  unique(sapply(strsplit(rownames(stackeddata),"_"),
                                          function(x) x[[1]])), silent=TRUE)
  return(X1)
}


panel_stack = function(array3d) {
  x1= apply(array3d,2,rbind)
  try(rownames(x1) <-  as.vector(sapply(dimnames(array3d)[[3]],
                                        FUN=function(x) paste(x, dimnames(array3d)[[1]], sep="_"))), silent=TRUE)
  return(as.data.frame(x1))
}

#Funkacja odejmujšca redniš wartoć dla danego kraju w czasie (fixed effects)

demean = function(x, margin) {
  #x is an array
  #margin is the dimension along which should be demeaned
  if (!is.array(x)) stop("x must be an array/matrix")
  otherdims=(1:length(dim(x)))[-margin]
  sweep(x,otherdims,apply(x,otherdims,mean))
}

##########################################################
########### Przetwarzanie danych panelowych  ###################



rownames(panelDat1)=paste(data.raw1[,"unit_id"],data.raw1[,"time"],sep="_")
panelDat2=as.matrix(panelDat1)

dat.array2=panel_unstack(panelDat2, tstep=13)

countryDat2=panel_stack(demean(dat.array2,1))


modelCd2=bms(countryDat2, g="UIP", mprior="random", mcmc="enumerate", nmodel=500)
image(modelCd2)


w1<-coef(modelCd1)
w2<-coef(modelCd2)


x<-rbind(w1,w2)
write.csv2(x, "Wyniki.csv")




