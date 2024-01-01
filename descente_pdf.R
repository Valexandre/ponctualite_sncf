library(tidyverse)
library(rvest)
#fichiers dispo dans data
dispoavantscrap<-read_csv("data/fichiersdejadispo.csv")

urlponctualite<-"https://www-ter-sncf-com.translate.goog/normandie/se-deplacer/info-trafic/ponctualite?_x_tr_sl=auto&_x_tr_tl=en&_x_tr_hl=fr&_x_tr_hist=true"

listelienspdf<-read_html(urlponctualite)%>%
  html_elements("a")%>%
  html_attr("href")

goodlistelienspdf<-listelienspdf[grepl("Normandie.pdf",listelienspdf)]
goodlistelienspdf<-substr(goodlistelienspdf,60,3000)
goodlistelienspdf<-gsub("\\%2520","%20",goodlistelienspdf)
goodlistelienspdf<-gsub("\\%25C3%25A9","é",goodlistelienspdf)
Fichiers<-tibble(urlfichier=goodlistelienspdf)
Fichiers$bonnedate<-substr(Fichiers$urlfichier,58,77)

for (i in 1:nrow(Fichiers)){
  download.file(url=Fichiers$urlfichier[i],destfile=paste0("data/",Fichiers$bonnedate[i],".pdf"), 
                mode="wb") 
}

#fichiers dispo dans data
alldispo<-list.files("data/",pattern="pdf")
nouveauxdispo<-alldispo[!alldispo%in%fichiersdejadispo]


# On crée le tableur avec les pdf
TestDF<-nouveauxdispo[1]%>%map_dfr(SorsLesDonneesDesPDF)
TestSors<-possibly(SorsLesDonneesDesPDF,otherwise = TestDF[1:5,])

ResultatScrap<-nouveauxdispo%>%map_dfr(TestSors)
ResultatScrap<-ResultatScrap%>%distinct(.keep_all = TRUE)

write.csv(ResultatScrap,paste0("data/",Sys.Date(),".csv"))
