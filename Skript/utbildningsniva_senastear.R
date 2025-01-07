## =================================================================================================================
# Skript som laddar hem data för utbildningsnivå från SCB
# Senast ändrad: 2025-01-07. I summarise har jag bytt från sum(Befolkning) till sum(Antal)
# =================================================================================================================
if (!require("pacman")) install.packages("pacman")
p_load(pxweb,openxlsx)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

#test_list <- data_utbildningsniva(skapa_fil = TRUE)

data_utbildningsniva<-function(region_vekt = "20", 
                               output_mapp = "G:/skript/jon/Slask/",
                               filnamn = "utbildningsniva.xlsx",
                               spara_data = TRUE){
  
  # =============================================== API-uttag ===============================================
  # "Adresser" till SCBs databas
  url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/UF/UF0506/UF0506B/Utbildning"
  
  # Variabler som skall tas ut
  varlista <-  list(Region=hamtaAllaLan(),
                    UtbildningsNiva=c("1","2","3","4","5","6","7","US"),
                    Kon=c("1","2"),
                    Alder= as.character(25:64),
                    ContentsCode=c("UF0506A1"),
                    Tid=max(hamta_giltiga_varden_fran_tabell(url, "tid")))

  # Uttag av data
  px_uttag <- pxweb_get(url = url,query = varlista)
  
  # Konverterar data till en Data Frame
  utbildning_df <- as.data.frame(px_uttag, column.name.type = "text", variable.value.type = "text")
  
  # Summerar alla samt gör ytterligare justeringar
  utbildning_df <- utbildning_df %>%
    mutate(utbildningsnivå = ifelse(utbildningsnivå %in% c("eftergymnasial utbildning, 3 år eller mer","forskarutbildning"),"eftergymnasial utbildning, 3 år eller mer",utbildningsnivå)) %>% 
      group_by(region, kön, år,utbildningsnivå) %>% 
        summarise("antal"= sum(Antal)) %>% 
          mutate(andel=(antal/sum(antal))*100) %>% 
            mutate(region=skapa_kortnamn_lan(region,byt_ut_riket_mot_sverige = TRUE))
  
  # Sparar data till Excel
  
  if (spara_data==TRUE){
    flik_lista=lst("Utbildningsnivå"= utbildning_df)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  }

}
