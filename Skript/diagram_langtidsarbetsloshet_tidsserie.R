diag_langtidsarbetsloshet<-function(region_vekt = "20", 
                                    output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                    returnera_data = TRUE,
                                    startar = 2010, # Finns från 2010
                                    spara_figur = TRUE){ 
  
  # =================================================================================================================
  # Skript som skapat ett diagram för långtidsarbetslöshet
  # Källa: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__HE__HE0110__HE0110G/TabVX4bDispInkN/
  # =================================================================================================================
  # Läser in nödvändiga bibliotek med pacman
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         openxlsx)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  
  gg_list <- list()
  
  långtidsarbetslöshet <- hamta_kolada_df(kpi_id = c("N03926"),
                                          valda_kommuner = region_vekt,
                                          valda_ar = startar:2100,
                                          konsuppdelat = TRUE) %>% 
    mutate(kon = tolower(kon))
  
  if(returnera_data == TRUE){
    assign("långtidsarbetslöshet", långtidsarbetslöshet, envir = .GlobalEnv)
  }
  
  diagram_capt <- "Källa: Arbetsförmedlingen (via Kolada)\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Antal invånare 25-64 år (årsmedelvärde år T) som varit öppet arbetslösa eller i program\nmed aktivitetsstöd i minst sex månader,dividerat med antal invånare 25-64 år den 31/12 år T-1."
  diagramtitel <- paste0("Långtidsarbetslöshet 25-64 år i ",unique(långtidsarbetslöshet$region))
  diagramfilnamn <- paste0("langtidsarbetsloshet_kolada_",unique(långtidsarbetslöshet$region),".png")
  
  gg_obj <- SkapaStapelDiagram(skickad_df = långtidsarbetslöshet,
                             skickad_x_var = "ar",
                             skickad_y_var = "varde",
                             skickad_x_grupp = "kon",
                             diagram_titel = diagramtitel,
                             diagram_capt = diagram_capt,
                             manual_y_axis_title = "procent",
                             x_axis_lutning = 0,
                             manual_color=diagramfarger("kon"),
                             output_mapp = output_mapp,
                             filnamn_diagram = diagramfilnamn,
                             skriv_till_diagramfil = spara_figur)
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  
  return(gg_list)
}

