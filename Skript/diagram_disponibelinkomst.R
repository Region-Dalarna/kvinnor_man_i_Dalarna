diag_disponibelinkomst<-function(region_vekt = "20", 
                                 output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                 returnera_data = TRUE,
                                 spara_figur = TRUE,
                                 alder = "18+ år"){ # Finns även: "18+ år" "18-29 år" "30-49 år" "50-64 år" "65+ år" "65-79 år" "66+ år" "80+ år". Max 1 åt gången){
  
  # =================================================================================================================
  # Skript som laddar hem data för disponibel inkomst från SCB
  # Källa: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__HE__HE0110__HE0110G/TabVX4bDispInkN/
  # =================================================================================================================
  # Läser in nödvändiga bibliotek med pacman
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         openxlsx)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  
  gg_list <- list()
  
  # "Adresser" till SCBs databas
  url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/HE/HE0110/HE0110G/Tab4bDispInkN"
  
  # Variabler som skall tas ut
  varlista <-  list("Region"=c(region_vekt),
                    "Hushallstyp"=c("*"),
                    "Alder"=c("*"),
                    "ContentsCode"=c('000006SY'),
                    "Tid"=c("*"))
  
  # Uttag av data
  disponibel_inkomst_df <- pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/HE/HE0110/HE0110G/TabVX4bDispInkN",query = varlista) %>% 
    as.data.frame(column.name.type = "text", variable.value.type = "text") %>%
      filter(hushållstyp %in% c("ensamstående kvinnor utan barn","ensamstående kvinnor med barn 0-19 år","ensamstående män utan barn","ensamstående män med barn 0-19 år","sammanboende utan barn","sammanboende med barn 0-19 år")) %>%
        filter(ålder==alder,
               år %in% c(min(år),max(år)))
  
  if(returnera_data == TRUE){
    assign("disponibel_inkomst_df", disponibel_inkomst_df, envir = .GlobalEnv)
  }
  
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."
  diagramtitel <- paste0("Disponibel inkomst (",unique(disponibel_inkomst_df$ålder),") i ",skapa_kortnamn_lan(unique(disponibel_inkomst_df$region),byt_ut_riket_mot_sverige = TRUE))
  diagramfilnamn <- paste0("disponibel_inkomst_",unique(disponibel_inkomst_df$ålder) %>% str_remove_all(" år|\\+") %>% str_replace_all("-", "_"),".png")
  
  # Skapar diagram för disponibel inkomst i Dalarna där män jämförs med kvinnor.
  gg_obj <- SkapaStapelDiagram(skickad_df = disponibel_inkomst_df,
                                     skickad_x_var = "hushållstyp",
                                     skickad_y_var = "Medianvärde, tkr",
                                     skickad_x_grupp = "år",
                                     x_axis_lutning = 0,
                                     manual_color = diagramfarger("rus_sex"),
                                     diagram_titel = diagramtitel,
                                     diagram_capt =  diagram_capt,
                                     stodlinjer_avrunda_fem = TRUE,
                                     diagram_liggande = TRUE,
                                     output_mapp = output_mapp,
                                     filnamn_diagram = diagramfilnamn,
                                     skriv_till_diagramfil = spara_figur)
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  
  return(gg_list)
}

