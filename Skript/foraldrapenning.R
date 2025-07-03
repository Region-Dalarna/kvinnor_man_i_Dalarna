## =================================================================================================================
# Skript som laddar hem data föräldrapenning och tillfällig föräldrapenning
# Källa: https://www.dataportal.se/sv/datasets?p=1&q=&s=2&t=20&f=&rt=dataset%24esterms_IndependentDataService%24esterms_ServedByDataService&c=false
# Använder numera Peters skript för att hämta data från FK (2024-11-21)
# =================================================================================================================
# # Läser in nödvändiga bibliotek med pacman
if (!require("pacman")) install.packages("pacman")
p_load(openxlsx)

# Funktioner som behövs
#source("https://raw.githubusercontent.com/JonFrank81/funktioner_alternativ/main/hamta_data.R")
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

#test_list <- diag_chefer(skapa_fil = FALSE)

diag_foraldraforsakring<-function(region_vekt = "20", 
                                  output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                  filnamn = "foraldrapenning.xlsx",
                                  spara_data = TRUE){
  
  # =============================================== Uttag ===============================================
  # Adresser till data
  path = c("https://www.forsakringskassan.se/api/sprstatistikrapportera/public/v1/fp-antal-mottagare-nettodagar-belopp/FPAntalDagarBeloppLanKommun.xlsx",
           "https://www.forsakringskassan.se/api/sprstatistikrapportera/public/v1/tfp-vab-antal-mottagare-belopp/tfpVabAntalDagarBeloppLanKommun.xlsx")
  
  # https://www.forsakringskassan.se/fk_apps/MEKAREST/public/v1/tfp-antal-mottagare-belopp/TfpAntalDagarBeloppLanKommun.xlsx
  # https://www.forsakringskassan.se/fk_apps/MEKAREST/public/v1/tfp-vab-antal-mottagare-belopp/tfpVabAntalDagarBeloppLanKommun.xlsx
  
  # Tidigare med mitt skript
  
  # # Anropar funktionen hamta_data_FK som hämtar data från öppna data på Försäkringskassan och returnerar en lista.
  # flik_lista <- hamta_data_FK(path, c("Föräldrapenning","VAB"),region_vekt)
  # 
  # # Döper om vissa variabler i dataset
  # flik_lista[[1]] <- flik_lista[[1]] %>% 
  #   rename(Antal_mottagare = `Antal mottagare`,
  #          Andel = `Andel nettodagar per kön`)
  # 
  # flik_lista[[2]] <- flik_lista[[2]] %>% 
  #   rename(Antal_mottagare = `Antal mottagare`,
  #          Antal_nettodagar = `Antal nettodagar`,
  #          Andel = `Andel nettodagar per kön`)
  
  # Uppdaterat med Peters skript istället
  
  flik_lista = list()
  
  fp_df = hamta_excel_dataset_med_url(path[1],skippa_rader = 2) %>% 
    filter(substr(Län,1,2) %in% region_vekt) %>% 
      rename(Antal_mottagare = `Antal mottagare`,
             Andel = `Andel nettodagar per kön`) %>% 
        select(-kolumnnamn)
  
  flik_lista[[1]] = fp_df
  
  vab_df = hamta_excel_dataset_med_url(path[2],skippa_rader = 2) %>%
    filter(substr(Län,1,2) %in% region_vekt) %>% 
      rename(Antal_mottagare = `Antal mottagare`,
             Antal_nettodagar = `Antal nettodagar`,
             Andel = `Andel nettodagar per kön`) %>% 
        select(-kolumnnamn)
  
  flik_lista[[2]] = vab_df
  
  names(flik_lista) = c("Föräldrapenning","VAB")
    
  
  
  # Sparar data till Excel
  if (spara_data==TRUE){
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  }

}
