library(tidyverse)
library(rvest)

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

fichiersdejadispo<-list.files("data/",pattern="pdf")
for (i in 1:nrow(Fichiers)){
  download.file(url=Fichiers$urlfichier[i],destfile=paste0("data/",Fichiers$bonnedate[i],".pdf"), 
                mode="wb") 
}
