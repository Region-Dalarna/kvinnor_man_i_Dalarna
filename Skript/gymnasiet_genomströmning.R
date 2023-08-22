# Skript som anropar Peters funktion för att ladda hem data för gymnasieantagning och sparar till Excel
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse,
       openxlsx,
       rKolada)

#test_lista <-diag_genomstromning(skapa_fil = FALSE)

diag_genomstromning <-function(region_vekt = "20", 
                               output_mapp = "G:/skript/jon/Slask/",
                               filnamn = "gymnasie_genomströmning.xlsx",
                               spara_data = TRUE){
  
  # =========================== läs in gymnasiedata från Gymnasieantagningen =========================

  genomstromning_df <- get_values(
    kpi = c("N60960"),
    municipality = c("0020"),
    period = 2000:2100
  )

  genomstromning_df <- genomstromning_df %>%
    mutate(kon = ifelse(gender=="K","Kvinnor",
                        ifelse(gender=="M","Män",
                        "Totalt"))) %>% 
      select(year,kon, municipality,value)

  if (spara_data==TRUE){
    flik_lista=lst("genomströmning"= genomstromning_df)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  }

}
