# Zaladowanie srodowiska --------------------------------------------------
rm(list=ls())
library(rvest)
#devtools::install_github("hrbrmstr/omdbapi")
library(omdbapi)
library(stringi)
library(stringr)
library(pbapply)

# Wczytanie filmow z wikipedii --------------------------------------------

web<-"https://en.wikipedia.org/wiki/Academy_Award_for_Best_Picture"
web<-read_html(web)

web %>%
  html_nodes('.wikitable i a') %>%
  html_text() -> filmy

web %>%
  html_nodes('.wikitable') -> pr


# Dodanie identyfikatora wygranej i roku wydania --------------------------

df<-data.frame("Film"=c(),"Production.company.s."=c() ,"Producer.s."=c(),"Oscar"=c(),"rok"=c())
for (i in 1:88){
  #dodaje zmiennÄ… objasniana
  df1<-html_table(pr[[i]])
  df1<-as.data.frame(df1)
  df1$Oscar<-rep(0,nrow(df1))
  df1[1,4]<-1
  
  #dodanie roku/lat za ktorych przyznana zostala nagroda
  pr[[i]]%>%html_nodes("caption")%>%html_text() -> rok
  if (grepl("/",rok)) rok<-substr(rok,1,7) else rok<-substr(rok,1,4) 
  df1$rok<-rep(rok,nrow(df1))  
  
  df<-rbind(df,df1)
}

df<-df[,c(1,4,5)]
rm(df1,i,pr,rok)




# Znalezienie ID z serwisu IMDb -------------------------------------------

znajdz_id<-function(tytul,rok){
  if(nchar(rok)>4){
    rok1<-substr(rok,1,4)
    if(substr(rok,1,2)=="19"){
      rok2<-paste("19",substr(rok,6,7),sep="")
    } else {
      rok2<-paste("20",substr(rok,6,7),sep="")
    }
    wyniki1<-search_by_title(tytul,year_of_release = rok1)
    wyniki2<-search_by_title(tytul,year_of_release = rok2)
    wyniki<-rbind(wyniki1,wyniki2)
  } else {
    wyniki<-search_by_title(tytul,year_of_release = rok)
  } 
  return(wyniki)
}


#pobranie informacji o wszystkich filmach - lista
df[,1]<-filmy
tytuly<-df[,1]
lata<-df[,3]
wyniki<-mapply(tytul=tytuly,znajdz_id,rok=lata)

id<-c()
for (i in 1:528){
  if(nrow(wyniki[[i]])==0){
    #jeĹ›li nie znalazlo dla konkretnego roku to robi ogolny search
    dd<-search_by_title(filmy[i])
    if (nrow(dd)==0){
      id[i]<-NA
    } else {
      id[i]<-dd[1,3]
    }
  } else {
    #przypisuje pierwszy wiersz (czy slusznie?)
    id[i]<-wyniki[[i]][1,3]
  }
}

porownaj<-function(range){
  for (id in range){
    print(list(df[id,c(1,3)],wyniki[[id]][,1:3]))
    inp<-readline("Nastepny film?: y/n ")
    if (inp=="y") next else stop()
  }
}

#sprawdzone recznie za pomoca funkcji 'porownaj' czy na pewno dobre id pobrane od 1990
porownaj(371:528)


# 270 - film 'Z', nieanglojezyczny, brak informacji, usunac
# 288 - film 'Cries and Whispers', nieanglojezyczny, brak informacji, usunac
# 446 - drugi wiersz wziac zamiast pierwszego. Dla pierwszego jest film 'Crash Landing'
# 472 - Precious: Based on the Novel \"Push\" by Sapphire - trzeba wyszukaÄ‡ po prostu dla 'Precious'
# 488 - Extremely Loud and Incredibly Close - 'and' jest problemem, znajduje dla '&'

id[446]<-search_by_title(filmy[446])[1,3]
id[472]<-search_by_title("Precious")[1,3]
id[488]<-search_by_title("Extremely Loud & Incredibly Close")[1,3]

df$Id<-id
usun<-c(270,288)
df<-df[-usun,]

rm(dd,filmy,id,lata,tytuly,usun,wyniki,i)


# Lista aktorow/aktorek z wygranym oscarem za pierwszoplanowa role --------


web<-"http://www.nndb.com/honors/511/000032415/"
web<-read_html(web)
web %>%
  html_nodes('.bordered') %>%
  html_table() %>%
  data.frame()-> aktorzy

web<-"http://www.nndb.com/honors/512/000032416/"
web<-read_html(web)
web %>%
  html_nodes('.bordered') %>%
  html_table() %>%
  data.frame()-> aktorki

web<-"http://www.nndb.com/honors/860/000048716/"
web<-read_html(web)
web %>%
  html_nodes('.bordered') %>%
  html_table() %>%
  data.frame()-> drugo_aktorzy

web<-"http://www.nndb.com/honors/035/000048888/"
web<-read_html(web)
web %>%
  html_nodes('.bordered') %>%
  html_table() %>%
  data.frame()-> drugo_aktorki

aktor_list<-rbind(aktorzy,aktorki,drugo_aktorzy,drugo_aktorki)
rm(aktorzy,aktorki,drugo_aktorki,drugo_aktorzy)



# Scraping zmiennych objasniajacyh ----------------------------------------

df[nchar(df$rok)>4, 3] <- paste("19",substr(df[nchar(df$rok)>4, 3],6,7),sep="")
aktor_list[2:6, 1] <- paste("19",substr(aktor_list[2:6, 1],6,7),sep="")
aktor_list[89:94, 1] <- substr(aktor_list[89:94, 1],1,4)


scrapuj_info <- function(id){
  
  web<-paste("http://www.imdb.com/title/",id,sep="")
  web<-read_html(web)
  
  
  web %>%
    html_nodes('#titleDetails :nth-child(10)')%>%
    html_text() %>%
    stri_replace_all_fixed(" ","") %>%
    gsub("\\([^\\]]*\\)", "", ., perl=TRUE) %>%
    str_replace_all(., "[\r\n]" , "") -> finansowe_1  
  
  web %>%
    html_nodes('#titleDetails :nth-child(11)')%>%
    html_text() %>%
    stri_replace_all_fixed(" ","") %>%
    gsub("\\([^\\]]*\\)", "", ., perl=TRUE) %>%
    str_replace_all(., "[\r\n]" , "") -> finansowe_2  
  
  web %>%
    html_nodes('#titleDetails :nth-child(12)') %>%
    html_text() %>%
    stri_replace_all_fixed(" ","") %>%
    gsub("\\([^\\]]*\\)", "", ., perl=TRUE) %>%
    str_replace_all(., "[\r\n]" , "") -> finansowe_3
  
  web %>%
    html_nodes('#titleDetails :nth-child(13)') %>%
    html_text() %>%
    stri_replace_all_fixed(" ","") %>%
    gsub("\\([^\\]]*\\)", "", ., perl=TRUE) %>%
    str_replace_all(., "[\r\n]" , "") -> finansowe_4
  
  
  web %>%
    html_nodes('#titleDetails time') %>%
    html_text() -> czas
  
  web %>%
    html_nodes('strong span') %>%
    html_text() -> rating
  
  # web %>%
  #   html_nodes('#titleStoryLine :nth-child(10) a') %>%
  #   html_text() -> gatunek
  
  web %>%
    html_nodes('.subtext .itemprop') %>%
    html_text() -> gatunek  
  
  web %>%
    html_nodes('.titleReviewbarItemBorder a:nth-child(1)') %>%
    html_text() -> komentarze
  
  web %>%
    html_nodes('.subText a:nth-child(3)') %>%
    html_text() -> zewnetrzne_zapowiedzi
  
  web %>%
    html_nodes('#title-overview-widget :nth-child(4) .itemprop') %>%
    html_text() -> aktorzy
  
  rok<-df[match(id,df$Id),3]
  
  lata<-c(na.omit(aktor_list[match(aktorzy,aktor_list[,2]),1]))
  if (length(lata)>0){
    lata<-as.numeric(lata)-1
    oscar_actor<-sum(lata<rok)  
  } else oscar_actor<-0
  
  lista<-list(
    film=df[match(id,df$Id),1],
    rok=rok,
    finansowe_1=finansowe_1,
    finansowe_2=finansowe_2,
    finansowe_3=finansowe_3,
    finansowe_4=finansowe_4,
    czas=czas,
    rating=rating,
    gatunek=gatunek,
    komentarze=komentarze,
    zewnetrzne_zapowiedzi=zewnetrzne_zapowiedzi,
    aktorzy=aktorzy,
    oscar_actor=oscar_actor
  )
  return(lista)
}

# scrapuj_info(df[490,4])
pr<-pblapply(df[,4],scrapuj_info)
# pr<-pbsapply(df[1:20,4],scrapuj_info)


# Obrobka zmiennych -------------------------------------------------------

oscar_actor<-sapply(pr, "[[", "oscar_actor") 

sapply(pr, "[[", "zewnetrzne_zapowiedzi") %>%
  gsub(" critic","", .) %>%
  as.numeric(.)-> zewnetrzne_zapowiedzi

sapply(pr, "[[", "komentarze") %>%
  gsub(" user","", .) %>%
  gsub(",","", .) %>%
  as.numeric(.) -> komentarze

sapply(pr, "[[", "rating") %>%
  as.numeric() -> rating

czas<-sapply(pr, "[[", "czas")
for (i in 1:length(czas)){
  cz<-czas[[i]]
  if (length(czas)>1) czas[[i]]<-cz[1]
}
czas %>% 
  unlist() %>%
  gsub(" min", "", .) %>%
  as.numeric(.) -> czas


finansowe_1<-sapply(pr, "[[", "finansowe_1")
finansowe_2<-sapply(pr, "[[", "finansowe_2")
finansowe_3<-sapply(pr, "[[", "finansowe_3")
finansowe_4<-sapply(pr, "[[", "finansowe_4")
nl<-list(finansowe_1,finansowe_2,finansowe_3,finansowe_4)

budget<-c()
opening_weekend<-c()
gross<-c()
for (i in 1:length(pr)){
  for (j in 1:4){
    for (k in 1:length(nl[[j]][[i]])){
      if (grepl("Budget",nl[[j]][[i]][k])) budget[i]<-nl[[j]][[i]][k]
      if (grepl("OpeningWeekend",nl[[j]][[i]][k])) opening_weekend[i]<-nl[[j]][[i]][k]
      if (grepl("Gross",nl[[j]][[i]][k])) gross[i]<-nl[[j]][[i]][k]
    }
  }
}
budget %>% substr(., 9, nchar(.)) %>% gsub(",","",.) -> budget
opening_weekend %>% substr(., 17, nchar(.)) %>% gsub(",","",.) -> opening_weekend
gross %>% substr(., 8, nchar(.)) %>% gsub(",","",.)  -> gross


gatunek<-sapply(pr, "[[", "gatunek")
gatunek<-sapply(gatunek, "[[", 1)

df<-data.frame(df,budget,opening_weekend,gross,czas,rating,komentarze, zewnetrzne_zapowiedzi,oscar_actor,gatunek)
rm(finansowe_1,finansowe_2,finansowe_3,finansowe_4,gatunek,budget,gross,opening_weekend,i,j,k,nl,czas,
   rating,komentarze,zewnetrzne_zapowiedzi,oscar_actor,cz)

# Dogranie brakujacych wartosci dla zmiennych ------------------------------

df$czas[df$Film=="Midnight in Paris"]<-94
df$czas[df$Film=="The Blind Side"]<-129

dodaj_opening<-function(id){
  web<-paste("http://www.imdb.com/title/",df[id,4],"/business?ref_=tt_dt_bus",sep="")
  web<-read_html(web)
  
  web %>% 
    html_nodes('#tn15content')  %>% 
    html_text() %>%
    stri_replace_all_fixed(" ","") -> open
  
  unlist(regmatches(open, regexec("(OpeningWeekend\\n\\$).+?\\(",open)))[1] -> open
  
  # unique(na.omit(as.numeric(unlist(strsplit(unlist(open1), "[^0-9]+")))))  
  open %>%
    unlist() %>% 
    strsplit(.,"[^0-9]+") %>%
    unlist() %>%
    #as.numeric() %>%
    na.omit() %>%
    #unique() %>%
    paste(.,collapse="") -> open
  open<-ifelse(length(open)==0,NA,as.numeric(open))
  
  return(open)
}

dodaj_budget<-function(id){
  web<-paste("http://www.imdb.com/title/",df[id,4],"/business?ref_=tt_dt_bus",sep="")
  web<-read_html(web)
  
  web %>% 
    html_nodes('#tn15content')  %>% 
    html_text() %>%
    stri_replace_all_fixed(" ","") -> budget
  
  unlist(regmatches(budget, regexec("(Budget\\n\\$).+?\\(",budget)))[1] -> budget
  
  # unique(na.omit(as.numeric(unlist(strsplit(unlist(open1), "[^0-9]+")))))  
  budget %>%
    unlist() %>% 
    strsplit(.,"[^0-9]+") %>%
    unlist() %>%
    #as.numeric() %>%
    na.omit() %>%
    paste(.,collapse="") -> budget  
  budget<-ifelse(length(budget)==0,NA,as.numeric(budget))
  
  return(budget)
}

dodaj_gross<-function(id){
  web<-paste("http://www.imdb.com/title/",df[id,4],"/business?ref_=tt_dt_bus",sep="")
  web<-read_html(web)
  
  web %>% 
    html_nodes('#tn15content')  %>% 
    html_text() %>%
    stri_replace_all_fixed(" ","") -> gross
  
  unlist(regmatches(gross, regexec("(Gross\\n\\$).+?\\(",gross)))[1] -> gross
  
  # unique(na.omit(as.numeric(unlist(strsplit(unlist(open1), "[^0-9]+")))))  
  gross %>%
    unlist() %>% 
    strsplit(.,"[^0-9]+") %>%
    unlist() %>%
    #as.numeric() %>%
    na.omit() %>%
    paste(.,collapse="") -> gross  
  gross<-ifelse(length(gross)==0,NA,as.numeric(gross))
  
  return(gross)
}

braki<-colSums(apply(df,2,is.na))

brak_open<-which(is.na(df$opening_weekend))
open<-pbsapply(brak_open,dodaj_opening)
df$opening_weekend<-as.numeric(levels(df$opening_weekend))[df$opening_weekend]
df$opening_weekend[brak_open]<-open

brak_budget<-which(is.na(df$budget))
budget<-pbsapply(brak_budget,dodaj_budget)
# View(data.frame(df[brak_budget,4],budget))
df$budget<-as.numeric(levels(df$budget))[df$budget]
df$budget[brak_budget]<-budget

brak_gross<-which(is.na(df$gross))
gross<-pbsapply(brak_gross,dodaj_gross)
# View(data.frame(df[brak_gross,4],gross))
df$gross<-as.numeric(levels(df$gross))[df$gross]
df$gross[brak_gross]<-gross

#liczba poprawionych
braki-colSums(apply(df,2,is.na))
rm(braki,brak_open,open,brak_budget,budget,brak_gross,gross)

write.csv2(df,"dane_oscar.csv")
df1<-df[complete.cases(df),]
