## =================================================================================================================
# Skript som laddar hem data för andel chefer från SCB och skapar ett diagram
# Källa: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AA__AA0003__AA0003B/IntGr1LanKonUtb/
# =================================================================================================================
# Läser in nödvändiga bibliotek med pacman
if (!require("pacman")) install.packages("pacman")
p_load(pxweb,
       openxlsx)

# Funktioner som behövs
source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")

#test_list <- diag_chefer(skapa_fil = FALSE)

diag_chefer<-function(region_vekt = "20", 
                      output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                      filnamn = "chefer.xlsx",
                      spara_data = TRUE){
  
  # =============================================== API-uttag ===============================================
  # "Adresser" till SCBs databas
  url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AA/AA0003/AA0003B/IntGr1LanKonUtb"

  # Variabler som skall tas ut
  varlista <-  list(Region=c(region_vekt),
                    Kon=c("1","2"),
                    UtbNiv=c("000","F","3","EU","US"),
                    BakgrVar=c("tot20-64"),
                    ContentsCode=c("0000001Y"),
                    Tid=c("*"))
  
  # Uttag av data
  chefer_df <- pxweb_get(url = url,query = varlista) %>% 
    as.data.frame(column.name.type = "text", variable.value.type = "text") %>% 
      rename(Andel = `Andel i chefsposition, procent`)
  
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
