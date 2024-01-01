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


SorsLesDonneesDesPDF<-function(urlascrap){
  gooddate<-gsub("\\.pdf","",gsub("%20"," ",urlascrap))
  PDF <- pdf_text(paste0("data/",urlascrap)) %>%
    readr::read_lines() #open 
  debutdata<-which(grepl("AXE PARIS - ROUEN - LE HAVRE",PDF))
  
  PDFDF<-PDF[debutdata:length(PDF)] %>%
    str_squish() %>%
    as.data.frame(.)
  
  colnames(PDFDF)<-"brut"
  PDFDF$EstuneHeure<-ifelse(grepl("\\d\\dh\\d\\d",substr(PDFDF$brut,1,5)),"heure","non")
  
  PDFDF<-PDFDF%>%
    mutate(Depart=case_when(EstuneHeure=="heure"~lag(brut),TRUE~""),
           Arrivee=case_when(EstuneHeure=="heure"~gsub("\\d\\dh\\d\\d (.+) \\d\\dh\\d\\d","\\1",brut), TRUE~""),
           HeureDepart=case_when(EstuneHeure=="heure"~substr(brut,1,5),TRUE~""),
           HeureArrivee=case_when(EstuneHeure=="heure"~gsub("\\d\\dh\\d\\d (.+) (\\d\\dh\\d\\d)","\\2",brut),      TRUE~""),
           ALheure=case_when(EstuneHeure=="heure" & grepl("A l'heure",brut)~"A l'heure",
                             EstuneHeure=="heure" & grepl("Supprimé",lag(brut))~"Supprimé",
                             EstuneHeure=="heure" & grepl("Supprimé",brut)~"Supprimé",
                             EstuneHeure=="heure" & grepl("Supp",lag(brut))~"Supp partielle",
                             TRUE~"Non"),
           Retard=case_when(EstuneHeure=="heure" & grepl("min ",brut)~as.numeric(gsub("\\+","",gsub("\\d\\dh\\d\\d (.+) min (.+)","\\1",HeureArrivee))),TRUE~0),
           MessagePart1=gsub("ROUEN RIVE DROITE","",gsub("PARIS ST-LAZARE","",gsub("PARIS-MONTPARNASSE","",gsub("ARGENTAN","",gsub("PARIS-VAUGIRARD","",gsub("GRANVILLE","",gsub("SERQUIGNY","",gsub("VERNON-GIVERNY","",gsub("ÉVREUX-NORMANDIE","",gsub("TROUVILLE-DEAUVILLE","",gsub("CAEN","",gsub("CHERBOURG","",gsub("PARIS-ST-LAZARE","",gsub("LE HAVRE","",gsub("ROUEN-RIVE-DROITE","",Depart))))))))))))))),
           MessagePart2=gsub("\\d\\dh\\d\\d (.+) min (.+)","\\2",HeureArrivee),
           EnsembleMessage=ifelse(nchar(MessagePart1)<2,"",str_trim(paste0(MessagePart1," ",MessagePart2))),
           Depart=case_when(grepl("ÉVREUX-NORMANDIE",Depart)~"ÉVREUX-NORMANDIE",
                            grepl("TROUVILLE-DEAUVILLE",Depart)~"TROUVILLE-DEAUVILLE",
                            grepl("CAEN",Depart)~"CAEN",
                            grepl("PARIS-MONTPARNASSE",Depart)~"PARIS-MONTPARNASSE",
                            grepl("SERQUIGNY",Depart)~"SERQUIGNY",
                            grepl("PARIS-VAUGIRARD",Depart)~"PARIS-VAUGIRARD",
                            grepl("ARGENTAN",Depart)~"ARGENTAN",
                            grepl("GRANVILLE",Depart)~"GRANVILLE",
                            grepl("VERNON-GIVERNY",Depart)~"VERNON-GIVERNY",
                            grepl("CHERBOURG",Depart)~"CHERBOURG",
                            grepl("PARIS-ST-LAZARE",Depart)~"PARIS-ST-LAZARE",
                            grepl("PARIS ST-LAZARE",Depart)~"PARIS-ST-LAZARE",
                            grepl("LE HAVRE",Depart)~"LE HAVRE",
                            grepl("ROUEN-RIVE-DROITE",Depart)~"ROUEN-RIVE-DROITE",
                            grepl("ROUEN RIVE DROITE",Depart)~"ROUEN-RIVE-DROITE"),
           Arrivee=case_when(grepl("ÉVREUX-NORMANDIE",Arrivee)~"ÉVREUX-NORMANDIE",
                             grepl("TROUVILLE-DEAUVILLE",Arrivee)~"TROUVILLE-DEAUVILLE",
                             grepl("CAEN",Arrivee)~"CAEN",
                             grepl("PARIS-MONTPARNASSE",Arrivee)~"PARIS-MONTPARNASSE",
                             grepl("SERQUIGNY",Arrivee)~"SERQUIGNY",
                             grepl("PARIS-VAUGIRARD",Arrivee)~"PARIS-VAUGIRARD",
                             grepl("ARGENTAN",Arrivee)~"ARGENTAN",
                             grepl("GRANVILLE",Arrivee)~"GRANVILLE",
                             grepl("VERNON-GIVERNY",Arrivee)~"VERNON-GIVERNY",
                             grepl("CHERBOURG",Arrivee)~"CHERBOURG",
                             grepl("PARIS-ST-LAZARE",Arrivee)~"PARIS-ST-LAZARE",
                             grepl("PARIS ST-LAZARE",Arrivee)~"PARIS-ST-LAZARE",
                             grepl("LE HAVRE",Arrivee)~"LE HAVRE",
                             grepl("ROUEN-RIVE-DROITE",Arrivee)~"ROUEN-RIVE-DROITE",
                             grepl("ROUEN RIVE DROITE",Arrivee)~"ROUEN-RIVE-DROITE"),
           HeureArrivee=substr(HeureArrivee,1,5))%>%
    filter(!is.na(Depart))
  
  Resultat<-PDFDF%>%select(Depart,Arrivee,HeureDepart,HeureArrivee,ALheure,Retard,EnsembleMessage)%>%
    mutate(date=gooddate)
  return(Resultat)
}

# On crée le tableur avec les pdf
TestDF<-nouveauxdispo[1]%>%map_dfr(SorsLesDonneesDesPDF)
TestSors<-possibly(SorsLesDonneesDesPDF,otherwise = TestDF[1:5,])

ResultatScrap<-nouveauxdispo%>%map_dfr(TestSors)
ResultatScrap<-ResultatScrap%>%distinct(.keep_all = TRUE)

write.csv(ResultatScrap,paste0("data/",Sys.Date(),".csv"))
