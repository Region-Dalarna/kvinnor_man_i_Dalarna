## =================================================================================================================
# Skript som laddar hem data för pågående sjukfall kopplade till stress
# Källa: https://www.dataportal.se/sv/datasets/547_7334/antal-pagaende-sjukfall-pa-grund-av-stress#ref=?p=1&q=stress&s=2&t=20&f=http%3A%2F%2Fpurl.org%2Fdc%2Fterms%2Fpublisher%7C%7Chttp%3A%2F%2Fdataportal.se%2Forganisation%2FSE2021005521%7C%7Cfalse%7C%7Curi%7C%7COrganisation%7C%7CF%C3%B6rs%C3%A4kringskassan&rt=dataset%24esterms_IndependentDataService%24esterms_ServedByDataService&c=false
# =================================================================================================================
# # Läser in nödvändiga bibliotek med pacman
if (!require("pacman")) install.packages("pacman")
p_load(openxlsx)

# Funktioner som behövs
source("https://raw.githubusercontent.com/JonFrank81/funktioner_alternativ/main/hamta_data.R")

#test_list <- diag_chefer(skapa_fil = FALSE)

diag_stress<-function(region_vekt = "20", 
                      output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                      filnamn = "sjukfall_stress.xlsx",
                      spara_data = TRUE){
  
  # =============================================== Uttag ===============================================
  # Adresser till data
  path = c("https://www.forsakringskassan.se/fk_apps/MEKAREST/public/v1/sjp-pagaende-sjukfall-diagnos-f43/SJPPagSjukfallDiagnosF43.xlsx")
  
  # Anropar funktionen hamta_data_FK som hämtar data från öppna data på Försäkringskassan och returnerar en lista.
  flik_lista <- hamta_data_FK(path, c("Stress"),region_vekt)
  
  # Döper om vissa variabler i dataset
  flik_lista[[1]] <- flik_lista[[1]] %>% 
    rename(Antal = `Antal pågående sjukfall`,
           Andel_procent = `Andel pågående sjukfall (%)`)
  
  
  # Sparar data till Excel
  if (spara_data==TRUE){
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  }
  
}
