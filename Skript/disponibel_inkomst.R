## =================================================================================================================
# Skript som laddar hem data för disponibel inkomst från SCB
# Källa: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__HE__HE0110__HE0110G/TabVX4bDispInkN/
# =================================================================================================================

# Läser in nödvändiga bibliotek med pacman
if (!require("pacman")) install.packages("pacman")
p_load(pxweb,
       openxlsx)

# Funktioner som behövs
source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")

#test_list <- diag_disponibelinkomst(skapa_fil = FALSE)

diag_disponibelinkomst<-function(region_vekt = "20", 
                                 output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                 spara_data = TRUE,
                                 filnamn = "disponibel_inkomst.xlsx" ){
  # =============================================== API-uttag ===============================================
  # "Adresser" till SCBs databas
  url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/HE/HE0110/HE0110G/Tab4bDispInkN"
  
  # Variabler som skall tas ut
  varlista <-  list("Region"=c(region_vekt),
                    "Hushallstyp"=c("*"),
                    "Alder"=c("*"),
                    "ContentsCode"=c('000006SW','000006SY','000006SX','000006SZ'),
                    "Tid"=c("*"))
  
  # Uttag av data
  disponibel_inkomst_df <- pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/HE/HE0110/HE0110G/TabVX4bDispInkN",query = varlista) %>% 
    as.data.frame(column.name.type = "text", variable.value.type = "text")
  
  # Sparar data till Excel
  if (spara_data==TRUE){
    flik_lista=lst("disponibel inkomst"= disponibel_inkomst_df)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  }

  # 
  # gg_list[[i]] <-gg_obj
  # i=i+1
  # 
  # names(gg_list) <-  c(objektnamn)
  # return(gg_list)
}

