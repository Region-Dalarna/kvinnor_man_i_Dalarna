## =================================================================================================================
# Skript som laddar hem data för förvärvsarbetande, bransch från SCB
# https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0207__AM0207Z/DagSni07KonKN/
# =================================================================================================================
if (!require("pacman")) install.packages("pacman")
p_load(pxweb,
       openxlsx)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

#test_list <- data_forvarvsarbetande_bransch(skapa_fil = TRUE)

data_forvarvsarbetande_bransch<-function(region_vekt = "20", 
                                         output_mapp = "G:/skript/jon/Slask/",
                                         filnamn = "forvarvsarbetande_bransch.xlsx",
                                         spara_data = TRUE){
  
  # =============================================== API-uttag ===============================================
  # "Adresser" till SCBs databas
  url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0207/AM0207Z/DagSni07KonKN"
  
  # Variabler som skall tas ut
  varlista <-  list(Region = c("00",region_vekt),
                    SNI2007 = c("*") ,
                    Kon=c("1","2"),
                    ContentsCode=c("00000544"),
                    Tid=max(hamta_giltiga_varden_fran_tabell(url, "tid")))
  
  # Uttag av data
  forvarvsarbetande_bransch <- pxweb_get(url = url,query = varlista) %>% 
    as.data.frame(., column.name.type = "text", variable.value.type = "text") %>% 
      rename("antal" = `Förvärvsarbetande 16-74 år med arbetsplats i regionen (dagbefolkning) (RAMS)`,
             "naringsgren" = `näringsgren SNI 2007`) %>% 
          group_by(år,region,naringsgren) %>%
              mutate(andel = antal/sum(antal)) %>% 
                mutate(region=skapa_kortnamn_lan(region,byt_ut_riket_mot_sverige = TRUE))
  
  # Döper om branscher till kortare namn
  forvarvsarbetande_bransch$naringsgren <- case_when(
                                            forvarvsarbetande_bransch$naringsgren == "jordbruk, skogsbruk och fiske"  ~ "Jordbruk och skogsbruk",
                                            forvarvsarbetande_bransch$naringsgren == "tillverkning och utvinning" ~ "Tillverkning och utvinning",
                                            forvarvsarbetande_bransch$naringsgren == "energiförsörjning; miljöverksamhet" ~ "Energi och miljö",
                                            forvarvsarbetande_bransch$naringsgren == "byggverksamhet" ~ "Bygg",
                                            forvarvsarbetande_bransch$naringsgren == "handel" ~ "Handel",
                                            forvarvsarbetande_bransch$naringsgren == "transport och magasinering"~ "Transport",
                                            forvarvsarbetande_bransch$naringsgren == "hotell- och restaurangverksamhet"  ~ "Hotell och restaurang",
                                            forvarvsarbetande_bransch$naringsgren == "information och kommunikation" ~ "IT och kommunikation",
                                            forvarvsarbetande_bransch$naringsgren == "finans- och försäkringsverksamhet"  ~ "Finans- och försäkring",
                                            forvarvsarbetande_bransch$naringsgren == "fastighetsverksamhet" ~ "Fastighet",
                                            forvarvsarbetande_bransch$naringsgren == "företagstjänster" ~ "Företagstjänster",
                                            forvarvsarbetande_bransch$naringsgren == "offentlig förvaltning och försvar" ~ "Offentlig förvaltning",
                                            forvarvsarbetande_bransch$naringsgren == "utbildning " ~ "Utbildning",
                                            forvarvsarbetande_bransch$naringsgren == "vård och omsorg; sociala tjänster"~ "Vård och omsorg",
                                            forvarvsarbetande_bransch$naringsgren == "kulturella och personliga tjänster m.m."  ~ "Kultur, fritid och nöje",
                                            forvarvsarbetande_bransch$naringsgren == "okänd verksamhet" ~ "Okänd verksamhet")

  
  # Sparar data till Excel
  if (spara_data==TRUE){
    flik_lista=lst("forvarsarbetande_bransch"= forvarvsarbetande_bransch)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  }
  
}
