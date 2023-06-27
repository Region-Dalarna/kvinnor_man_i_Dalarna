## =================================================================================================================
# Skript som laddar hem data för arbetsmarknadsstatus från SCB för senaste år (regioner och kommuner)
# Källa : https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0210__AM0210A/ArbStatusM/   
# =================================================================================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pxweb,tidyverse,openxlsx)

# Laddar in de funktioner som används för att skapa diagram
source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")

#test_list <- diag_arbetsmarknadsstatus(skapa_fil = FALSE)

diag_arbetsmarknadsstatus <-function(vald_region = "20",
                                     output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                     skapa_fil = TRUE){
  
  
  # =============================================== API-uttag ===============================================
  # "Adresser" till SCBs databas
  url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0210/AM0210A/ArbStatusM"
  
  # Om man vill ta reda vilka variabler som finns i tabellen. 
  # pxvarlist("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0210/AM0210A/ArbStatusM")                        # för att se alla variabler för en tabell
  # pxvardelist("https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0210/AM0210A/ArbStatusM", "contentscode")      # för att se alla värden för vald variabel
  # Variabler som skall tas ut
  
  varlista_lan <-  list("Region"=hamtaAllaLan(tamedriket = TRUE),
                        "Kon"=c("*"),
                        "Alder"=c("20-64"),
                        "Fodelseregion"=c("*"),
                        "ContentsCode"=c("000006II","000006IJ","000006IK"),
                        "Tid"=max(hamta_giltiga_varden_fran_tabell(url, "tid")))
  
  varlista_kommun <- list("Region"=hamtakommuner(vald_region,tamedlan = TRUE),
                          "Kon"=c("*"),
                          "Alder"=c("20-64"),
                          "Fodelseregion"=c("*"),
                          "ContentsCode"=c("000006II","000006IJ","000006IK"),
                          "Tid"=max(hamta_giltiga_varden_fran_tabell(url, "tid")))
  
  varlista=lst(varlista_lan,varlista_kommun)          
  lista <- list()
  i=1
  
  while(i<=length(varlista)){
    px_uttag <- pxweb_get(url = url,query = varlista[[i]])
    
    lista[[i]] <- as.data.frame(px_uttag) %>% 
      cbind(regionkod = as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
              select(Region)) %>% 
        rename(regionkod = Region)%>%
          relocate(regionkod, .before = region) %>% 
            mutate(ar=substr(månad,1,4),
                   manad_long=format(as.Date(paste(ar, str_sub(månad, 6,7),"1", sep = "-")), "%B"),
                    Period=paste(ar, str_sub(månad, 6,7),sep = "-")) %>% 
              select(-månad)
    
    lista[[i]]$region <-skapa_kortnamn_lan(lista[[i]]$region,byt_ut_riket_mot_sverige = TRUE)
    i=i+1
  }
  
  names(lista) <- c("Län","Kommun")
  
  openxlsx::write.xlsx(lista,paste0(output_mapp,"/arbetsmarknadsstatus_senastear.xlsx"))
  
}

