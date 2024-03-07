## =================================================================================================================
# Skript som laddar hem data för förvärvsarbetande, bransch. Data hämtas med hjälp av ett hamta_data_skript.
# Enbart senaste år.
# =================================================================================================================
if (!require("pacman")) install.packages("pacman")
p_load(pxweb,
       openxlsx)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

#test_list <- data_forvarvsarbetande_bransch(skapa_fil = TRUE)

data_forvarvsarbetande_bransch<-function(region_vekt = "20", 
                                         output_mapp = "G:/skript/jon/Slask/",
                                         filnamn = "forvarvsarbetande_bransch.xlsx",
                                         spara_data = TRUE){
  
  # =============================================== API-uttag ===============================================
  # Hämtar data
  source("G:/skript/hamta_data/hamta_syss_bransch_kon_manad_bas.R")
  df = hamta_syss_branscher_kon_manad_bas(region_vekt = region_vekt,
                                          ar_vekt = "9999")
  
  # Beräknar andelar
  df = df %>% 
    rename(antal = `sysselsatta efter arbetsställets belägenhet`) %>% 
      group_by(år,månad_år,bransch) %>%
        mutate(andel = round((antal/sum(antal))*100,0))

  # Sparar data till Excel
  if (spara_data==TRUE){
    flik_lista=lst("forvarsarbetande_bransch"= df)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  }
  
}

