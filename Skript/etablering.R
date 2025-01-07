####################################
### Etableringstid för nyanlända ###
####################################
# Källa: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AA__AA0003__AA0003B/IntGr1LanKonUtb/
# Senast uppdaterad 2023-04-12 (på SCBs hemsida)
# Har justerat skript då data nu har uppdaterats och finns i två tabeller, en fram till 2021 och en från 2022

# Läser in nödvändiga bibliotek med pacman
if (!require("pacman")) install.packages("pacman")
p_load(pxweb,
       openxlsx)

# Funktioner som behövs
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

#test_list=diag_etablering(skapa_fil=FALSE)

diag_etablering <- function(region_vekt = "20",
                            output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                            spara_data = TRUE,
                            filnamn = "etableringstid.xlsx" ){
  
  # ========================================== Inställningar ============================================
  # Text till diagram
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."
  #skapa_fil=FALSE

  # ========================================== Läser in data ============================================
  
  # Äldre data (RAMS). Fram till 2021
  pxweb_query_list <- 
    list("Region"=c(region_vekt),
         "Kon"=c("*"),
         "UtbNiv"=c("*"),
         "BakgrVar"=c("INT010","INT020","INT030","INT040"),
         "ContentsCode"=c("0000001X"),
         "Tid"=c("*"))
  
  # Hämtar data 
  etablering_gammal <- pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AA/AA0003/AA0003X/IntGr1LanKonUtb",
              query = pxweb_query_list) %>% 
      as.data.frame(., column.name.type = "text", variable.value.type = "text") %>% 
        mutate("bakgrundsvariabel" =  word(bakgrundsvariabel,2,3)) %>% 
          rename(andel = `Andel förvärvsarbetande (ny definition från och med 2019)`)
  
  # Nyare data (BAS). Från 2022
  pxweb_query_list <- 
    list("Region"=c(region_vekt),
         "Kon"=c("*"),
         "UtbNiv"=c("*"),
         "BakgrVar"=c("INT010","INT020","INT030","INT040"),
         "ContentsCode"=c("000007KE"),
         "Tid"=c("*"))
  
  # Hämtar data 
  etablering_ny <- pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AA/AA0003/AA0003B/IntGr1LanUtbBAS",
                          query = pxweb_query_list) %>% 
    as.data.frame(., column.name.type = "text", variable.value.type = "text") %>% 
      mutate("bakgrundsvariabel" =  word(bakgrundsvariabel,2,3))%>% 
        rename(andel = `Andel sysselsatta`)
  
  etablering <- rbind(etablering_gammal,etablering_ny)
  
  # Sparar data till Excel
  if (spara_data==TRUE){
    flik_lista=lst("etableringstid"= etablering)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  }
  
}
