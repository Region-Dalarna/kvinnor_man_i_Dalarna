## =================================================================================================================
# Skript som laddar hem data för andel chefer från SCB och skapar ett diagram
# Källa: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AA__AA0003__AA0003X/IntGr1LanKonUtb/
# Data kontrollerat 2025-01-07. Uppdaterades senast (av SCB) 2024-12-23. Finns data till 2022 (2023 står som NA).
# =================================================================================================================
# Läser in nödvändiga bibliotek med pacman
if (!require("pacman")) install.packages("pacman")
p_load(pxweb,
       openxlsx)

# Funktioner som behövs
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

#test_list <- diag_chefer(skapa_fil = FALSE)

diag_chefer<-function(region_vekt = "20", 
                      output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                      filnamn = "chefer.xlsx",
                      spara_data = TRUE){
  
  # =============================================== API-uttag ===============================================
  # "Adresser" till SCBs databas
  
  # Data som inte uppdateras (fram till 2021 - RAMS)
  url_tidigare <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AA/AA0003/AA0003X/IntGr1LanKonUtb"
  #url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AA/AA0003/AA0003B/IntGr1LanKonUtb"
  # Variabler som skall tas ut
  varlista <-  list(Region=c(region_vekt),
                    Kon=c("1","2"),
                    UtbNiv=c("000","F","3","EU","US"),
                    BakgrVar=c("tot20-64"),
                    ContentsCode=c("0000001Y"),
                    Tid=c("*"))
  
  # Uttag av data
  chefer_RAMS_df <- pxweb_get(url = url_tidigare,query = varlista) %>% 
    as.data.frame(column.name.type = "text", variable.value.type = "text") %>% 
    rename(Andel = `Andel i chefsposition, procent`)
  
  # Data som uppdateras (från 2022 - BAS)
  url_ny <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AA/AA0003/AA0003B/IntGr1LanUtbBAS"

  # Variabler som skall tas ut
  varlista <-  list(Region=c(region_vekt),
                    Kon=c("1","2"),
                    UtbNiv=c("000","F","3","EU","US"),
                    BakgrVar=c("TOT"),
                    ContentsCode=c("000007KF"),
                    Tid=c("*"))
  
  # Uttag av data
  chefer_bas_df <- pxweb_get(url = url_ny,query = varlista) %>% 
    as.data.frame(column.name.type = "text", variable.value.type = "text") %>% 
      rename(Andel = `Andel i chefsposition`)
  
  chefer_df <- rbind(chefer_RAMS_df,chefer_bas_df)
  
  # Andrar namnet på vissa kategorier
  chefer_df[chefer_df=="utbildningsnivå: förgymnasial utbildning"] <- "förgymnasial utbildning"
  chefer_df[chefer_df=="utbildningsnivå: gymnasial utbildning"] <- "gymnasial utbildning"
  chefer_df[chefer_df=="utbildningsnivå: eftergymnasial utbildning"] <- "eftergymnasial utbildning"
  
  # Sparar data till Excel
  if (spara_data==TRUE){
    flik_lista=lst("Chefer" = chefer_df)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  }
}
