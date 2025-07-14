diag_utbniva_senaste<-function(region_vekt = "20", 
                               output_mapp = "G:/skript/jon/Slask/",
                               spara_figur = TRUE,
                               returnera_data = TRUE){
  
  ## =================================================================================================================
  # Skript som skapar ett diagram för utbildningsnivå
  #
  # =================================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         tidyverse)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  
  # =============================================== API-uttag ===============================================
  # "Adresser" till SCBs databas
  url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/UF/UF0506/UF0506B/Utbildning"
  
  gg_list <- list()
  
  # Variabler som skall tas ut
  varlista <-  list(Region = region_vekt,
                    UtbildningsNiva = c("1","2","3","4","5","6","7","US"),
                    Kon = c("1","2"),
                    Alder = as.character(25:64),
                    ContentsCode = c("UF0506A1"),
                    Tid =max (hamta_giltiga_varden_fran_tabell(url, "tid")))
  
  # Uttag av data
  px_uttag <- pxweb_get(url = url,query = varlista)
  
  # Konverterar data till en Data Frame
  utbildning_df <- as.data.frame(px_uttag, column.name.type = "text", variable.value.type = "text")
  
  # Summerar alla samt gör ytterligare justeringar
  utbildning_df <- utbildning_df %>%
    mutate(utbildningsnivå = ifelse(utbildningsnivå %in% c("eftergymnasial utbildning, 3 år eller mer","forskarutbildning"),"eftergymnasial utbildning, 3 år eller mer",utbildningsnivå)) %>% 
    group_by(region, kön, år,utbildningsnivå) %>% 
    summarise("antal"= sum(Antal)) %>% 
    mutate(andel=(antal/sum(antal))*100) %>% 
    mutate(region=skapa_kortnamn_lan(region,byt_ut_riket_mot_sverige = TRUE)) %>% 
    ungroup()
  
  if(returnera_data == TRUE){
    assign("utbildning_df", utbildning_df, envir = .GlobalEnv)
  }

  # Sparar data till Excel
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
  
  # Skapar en faktorvariabel som styr vilken ordning som utbildningsgrupper visas
  utbildning_df$utbildningsnivå <- factor(utbildning_df$utbildningsnivå, levels = c("eftergymnasial utbildning, 3 år eller mer","eftergymnasial utbildning, mindre än 3 år",
                                                                                    "gymnasial utbildning, 3 år","gymnasial utbildning, högst 2 år","förgymnasial utbildning, 9 (10) år",
                                                                                    "förgymnasial utbildning kortare än 9 år")[6:1])
  
  diagramtitel <- paste0("Utbildningsnivåer (25-64 år) i ",unique(utbildning_df$region)," år ",max(utbildning_df$år))
  diagramfilnamn <- paste0("utbildningsniva_",unique(utbildning_df$region),"_senaste_ar.png")
  
  gg_obj <- SkapaStapelDiagram(skickad_df <- utbildning_df %>%
                                         filter(utbildningsnivå != "uppgift om utbildningsnivå saknas"), 
                                       skickad_x_var = "utbildningsnivå", 
                                       skickad_y_var = "andel", 
                                       skickad_x_grupp = "kön",
                                       x_axis_lutning = 0,
                                       diagram_liggande = TRUE,
                                       x_axis_sort_value = FALSE,
                                       manual_color = diagramfarger("kon"),
                                       manual_y_axis_title = "procent",
                                       diagram_titel = diagramtitel,
                                       diagram_capt =  diagram_capt,
                                       stodlinjer_avrunda_fem = TRUE,
                                       berakna_index = FALSE,
                                       output_mapp = output_mapp,
                                       filnamn_diagram = diagramfilnamn,
                                       skriv_till_diagramfil = spara_figur)
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  
  return(gg_list)
  
}
