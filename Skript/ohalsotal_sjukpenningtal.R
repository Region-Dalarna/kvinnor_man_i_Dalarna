## =================================================================================================================
# Skript som laddar hem data ohälsotalet
# Källa: https://www.dataportal.se/sv/datasets?p=1&q=&s=2&t=20&f=&rt=dataset%24esterms_IndependentDataService%24esterms_ServedByDataService&c=false
# Välj organisation: Försäkringskassan
# =================================================================================================================
# Läser in nödvändiga bibliotek med pacman
if (!require("pacman")) install.packages("pacman")
p_load(janitor,
       keyring,
       httr,
       rKolada,
       openxlsx,
       rio,
       tidyverse)

# Funktioner som behövs
source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")

#test_list <- diag_chefer(skapa_fil = FALSE)

diag_ohalsa<-function(region_vekt = "20", 
                      output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                      filnamn = "ohalsotal_sjukpenningtal.xlsx",
                      spara_data = TRUE){

  # =============================================== Uttag ===============================================
  
  
  ohalsotalet_df <- import(file="https://www.forsakringskassan.se/fk_apps/MEKAREST/public/v1/ohm-ohalsotal/SJPohttal.xlsx") %>% 
    filter(!row_number() %in% c(0, 1)) %>% 
      row_to_names(1) %>% 
        filter(substr(Län,1,2) == region_vekt)
  
 sjukpenningtalet_df <- import(file="https://www.forsakringskassan.se/fk_apps/MEKAREST/public/v1/ohm-sjptal/SJPsjptal.xlsx") %>% 
    filter(!row_number() %in% c(0, 1)) %>% 
      row_to_names(1) %>% 
        filter(substr(Län,1,2) == region_vekt) 
  
  # Sparar data till Excel
  if (spara_data==TRUE){
    flik_lista=lst("Ohälsotalet" = ohalsotalet_df,"Sjukpenningtalet" = sjukpenningtalet_df)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  }
}
