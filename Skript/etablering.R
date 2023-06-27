####################################
### Etableringstid för nyanlända ###
####################################
# Källa: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AA__AA0003__AA0003B/IntGr1LanKonUtb/

# Läser in nödvändiga bibliotek med pacman
if (!require("pacman")) install.packages("pacman")
p_load(pxweb,
       openxlsx)

# Funktioner som behövs
source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")

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
  pxweb_query_list <- 
    list("Region"=c(region_vekt),
         "Kon"=c("*"),
         "UtbNiv"=c("*"),
         "BakgrVar"=c("INT010","INT020","INT030","INT040"),
         "ContentsCode"=c("0000001X"),
         "Tid"=c("*"))
  
  # Hämtar data 
  etablering <- pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/AA/AA0003/AA0003B/IntGr1LanKonUtb",
              query = pxweb_query_list) %>% 
      as.data.frame(., column.name.type = "text", variable.value.type = "text") %>% 
        mutate("bakgrundsvariabel" =  word(bakgrundsvariabel,2,3))
  
  # Sparar data till Excel
  if (spara_data==TRUE){
    flik_lista=lst("etableringstid"= etablering)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  }
  
}
