diag_ledamoter_val <- function(outputmapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                               spara_dataframe_till_global_environment = FALSE,
                               spara_figur = TRUE,
                               typ_av_val = c("riksdag","region","kommun") # Minst en av dessa måste väljas
                               ){
  
  # =================================================================================================================
  # Skript som skriver ut tre diagram för respektive val (riksdag, region och kommun) med antal ledamöter per kön.
  # Källor för data:
  # Riksdag: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__ME__ME0107__ME0107C/Riksdagsledamoter/
  # Region: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__ME__ME0107__ME0107B/Ltledamoter/
  # Kommun_ https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__ME__ME0107__ME0107B/Kfledamoter/
  # 
  # Funkar enbart för Dalarna för tillfället. Detta eftersom vissa koder för regionval skrivs LG och vissa L. Oklart varför. 
  #
  # Uppdaterad med nya PXweb 2026-07-06 /Jon
  # =============================================== API-uttag ===============================================
  
  # Läser in nödvändiga bibliotek med pacman
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         openxlsx)
  
  gg_list <- list()
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\n Bearbetning: Samhällsanalys, Region Dalarna."
  
  # Funktioner som behövs
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_pxweb2.R")
  
  if("riksdag" %in% typ_av_val){
    
    # Hämtar data med nya PXweb
    riksdagsval_df <- pxweb2_hamta_data(
      tabell = "TAB3531",
      query = list(
        Region = "Totalt för riket",
        Parti = "*",
        Kon = c("1","2"),
        ContentsCode = "*",
        Tid = "*"
      )) %>% 
      group_by(kön,valår) %>%
      summarise("Antal" = sum(value)) %>% 
      ungroup()
    
    if(spara_dataframe_till_global_environment) {
      assign("riksdagsval_df", riksdagsval_df, envir = .GlobalEnv)
    }
    
    diagramtitel <- paste0("Riksdagsledamöter per valår i Dalarna")
    diagramfilnamn <- paste0("Riksdagsledamoter",".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = riksdagsval_df,
                                 skickad_x_var = "valår",
                                 skickad_y_var = "Antal",
                                 skickad_x_grupp = "kön",
                                 x_axis_lutning = 0,
                                 manual_color = diagramfarger("kon"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 stodlinjer_avrunda_fem = TRUE,
                                 output_mapp = outputmapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = spara_figur)    
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    
  } 
  
  if("region" %in% typ_av_val){
    
    # Hämtar data med nya PXweb
    regionval_df <- pxweb2_hamta_data(
      tabell = "TAB2589",
      query = list(
        Region = "Region Dalarna",
        Parti = "*",
        Kon = c("1","2"),
        ContentsCode = "*",
        Tid = "*"
      )) %>% 
      group_by(region,kön,valår) %>%
      summarise("Antal" = sum(value)) %>% 
      ungroup()
    
    if(spara_dataframe_till_global_environment) {
      assign("regionval_df", regionval_df, envir = .GlobalEnv)
    }
    
    diagramtitel <- paste0("Regionfullmäktigeledamöter per valår i Dalarna")
    diagramfilnamn <- paste0("Regionfullaktigeledamoter",".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = regionval_df,
                                 skickad_x_var = "valår",
                                 skickad_y_var = "Antal",
                                 skickad_x_grupp = "kön",
                                 x_axis_lutning = 0,
                                 manual_color = diagramfarger("kon"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 stodlinjer_avrunda_fem = TRUE,
                                 output_mapp = outputmapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = spara_figur)    
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    
  } 
  
  if("kommun" %in% typ_av_val){
    
    # Hämtar data med nya PXweb  
    kommunval_df <- pxweb2_hamta_data(
      tabell = "TAB1970",
      query = list(
        Region = hamtakommuner(tamedlan = FALSE,tamedriket = FALSE),
        Parti = "*",
        Kon = "*",
        ContentsCode = "*",
        Tid = "9999"
      )) %>% 
      group_by(region,kön,valår) %>%
      summarise("Antal" = sum(value)) %>% 
      ungroup()
    
    if(spara_dataframe_till_global_environment) {
      assign("kommunval_df", kommunval_df, envir = .GlobalEnv)
    }
    
    diagramtitel <- paste0("Kommunfullmäktigeledamöter i Dalarna ", max(kommunval_df$valår))
    diagramfilnamn <- paste0("Kommunfullmaktigeledamoter",".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = kommunval_df,
                                 skickad_x_var = "region",
                                 skickad_y_var = "Antal",
                                 skickad_x_grupp = "kön",
                                 x_axis_lutning = 45,
                                 manual_color = diagramfarger("kon"),
                                 manual_x_axis_text_vjust = 1,
                                 manual_x_axis_text_hjust = 1,
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 stodlinjer_avrunda_fem = TRUE,
                                 output_mapp = outputmapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = spara_figur)    
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  }
  return(gg_list)
}





