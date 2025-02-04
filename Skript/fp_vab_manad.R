## =================================================================================================================
# Skript som laddar hem data för pågående föräldrapenning och tillfällig föräldrapenning på månadsbasis från Försäkringskassan
# Källa: https://www.dataportal.se/datasets/547_12671
# Källa: https://www.dataportal.se/datasets/547_12364
# =================================================================================================================
diag_foraldrapenning_manad<-function(region_vekt = "20", 
                                     diag_foraldrapenning = TRUE,
                                     diag_vab = TRUE,
                                     output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                     spara_diagrambildfil = FALSE,
                                     spara_dataframe_till_global_environment = FALSE){
  
  # =============================================== Uttag ===============================================
  
  # # Läser in nödvändiga bibliotek med pacman
  if (!require("pacman")) install.packages("pacman")
  p_load(openxlsx,
         here)
  
  # Funktioner som behövs
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  # Adresser till data
  path = c("https://www.forsakringskassan.se/fk_apps/MEKAREST/public/v1/fp-antal-mottagare-nettodagar-belopp-manad/FPAntalDagarBeloppManadLan.xlsx","https://www.forsakringskassan.se/fk_apps/MEKAREST/public/v1/tfp-utb-ers/TfpVabErsUtbLanKommun.xlsx")

  # Med Peters nya skript
  flik_lista = list()
  
  gg_list = list()
  
  if(diag_foraldrapenning == TRUE){

    foraldrapenning_df = hamta_excel_dataset_med_url(path[1],skippa_rader = 2) %>% 
      filter(substr(Län,1,2) %in% region_vekt) %>%
       filter(Kommun == "20 Dalarnas län") %>% 
        rename(Antal_mottagare = `Antal mottagare`,
               Andel_kon = `Andel nettodagar per kön`) %>%  
          select(-kolumnnamn) %>% 
            mutate(Månad = ifelse(substr(Månad,1,1)==0,substr(Månad,2,2),Månad)) 
      
        foraldrapenning_df$Månad = case_when(
                              foraldrapenning_df$Månad == 1 ~ "Januari",
                              foraldrapenning_df$Månad == 2 ~ "Februari",
                              foraldrapenning_df$Månad == 3 ~ "Mars",
                              foraldrapenning_df$Månad == 4 ~ "April",
                              foraldrapenning_df$Månad == 5 ~ "Maj",
                              foraldrapenning_df$Månad == 6 ~ "Juni",
                              foraldrapenning_df$Månad == 7 ~ "Juli",
                              foraldrapenning_df$Månad == 8 ~ "Augusti",
                              foraldrapenning_df$Månad == 9 ~ "September",
                              foraldrapenning_df$Månad == 10 ~ "Oktober",
                              foraldrapenning_df$Månad == 11 ~ "November",
                              foraldrapenning_df$Månad == 12 ~ "December"
                            )
        
        if(spara_dataframe_till_global_environment) {
          assign("foraldrapenning_manad_df", foraldrapenning_df, envir = .GlobalEnv)
        }
        
        # Create factor-variable for month
        foraldrapenning_df$Månad = factor(foraldrapenning_df$Månad, levels = c("Januari", "Februari", "Mars", "April", "Maj", "Juni", "Juli", "Augusti", "September", "Oktober", "November", "December"))
        
        diagram_capt <- "Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna."
        diagramtitel <- "Uttagna nettodagar av föräldrapenning i Dalarna"
        diagramfilnamn <- "foraldrapenning_manad.png"
        
        
        gg_obj <- SkapaStapelDiagram(skickad_df = foraldrapenning_df%>%
                                             filter(Kommun == "20 Dalarnas län",
                                                    Kön != "Kvinnor och män",
                                                    År>"2018") %>% 
                                             mutate("år" = År), 
                                           skickad_x_var = "Månad", 
                                           skickad_y_var = "Nettodagar", 
                                           skickad_x_grupp = "år",
                                           diagram_facet = TRUE,
                                           facet_scale = "fixed",
                                           facet_grp = "Kön",
                                           manual_color = diagramfarger("rus_sex"),
                                           manual_x_axis_text_vjust = 1,
                                           manual_x_axis_text_hjust = 1,
                                           facet_legend_bottom = TRUE,
                                           #berakna_index = TRUE,
                                           x_axis_lutning = 45,
                                           diagram_titel = diagramtitel,
                                           diagram_capt =  diagram_capt,
                                           stodlinjer_avrunda_fem = TRUE,
                                           output_mapp = output_mapp,
                                           filnamn_diagram = diagramfilnamn,
                                           skriv_till_diagramfil = spara_diagrambildfil)
        
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  
  }
  
  if(diag_vab == TRUE){
      
  vab_df <- hamta_excel_dataset_med_url(path[2],skippa_rader = 2) %>% 
    filter(substr(Län,1,2) %in% region_vekt) %>%
      filter(Kommun == "20 Dalarnas län") %>% 
        rename(Antal_mottagare = `Antal mottagare`,
               Nettodagar = `Antal nettodagar`) %>%  
          select(-kolumnnamn) %>% 
            mutate(Månad = ifelse(substr(Månad,1,1)==0,substr(Månad,2,2),Månad))
  
  vab_df$Månad = case_when(
    vab_df$Månad == 1 ~ "Januari",
    vab_df$Månad == 2 ~ "Februari",
    vab_df$Månad == 3 ~ "Mars",
    vab_df$Månad == 4 ~ "April",
    vab_df$Månad == 5 ~ "Maj",
    vab_df$Månad == 6 ~ "Juni",
    vab_df$Månad == 7 ~ "Juli",
    vab_df$Månad == 8 ~ "Augusti",
    vab_df$Månad == 9 ~ "September",
    vab_df$Månad == 10 ~ "Oktober",
    vab_df$Månad == 11 ~ "November",
    vab_df$Månad == 12 ~ "December"
  )
  
  if(spara_dataframe_till_global_environment) {
    assign("vab_manad_df", vab_df, envir = .GlobalEnv)
  }
  
  # Create factor-variable for month
  vab_df$Månad = factor(vab_df$Månad, levels = c("Januari", "Februari", "Mars", "April", "Maj", "Juni", "Juli", "Augusti", "September", "Oktober", "November", "December"))
  
  diagram_capt <- "Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna.\nGäller månad när föräldern vabbade."
  diagramtitel <- "Uttagna nettodagar av tillfällig föräldrapenning i Dalarna"
  diagramfilnamn <- "vab_manad.png"
  
  gg_obj <- SkapaStapelDiagram(skickad_df = vab_df%>%
                                        filter(Kommun == "20 Dalarnas län",
                                               Kön != "Kvinnor och män",
                                               Tidsperspektiv == "Månad när föräldern vabbade",
                                               År>"2018" & År<"2024") %>% 
                                        mutate("år" = År), 
                                      skickad_x_var = "Månad", 
                                      skickad_y_var = "Nettodagar", 
                                      skickad_x_grupp = "år",
                                      diagram_facet = TRUE,
                                      facet_scale = "fixed",
                                      facet_grp = "Kön",
                                      manual_color = diagramfarger("rus_sex"),
                                      manual_x_axis_text_vjust = 1,
                                      manual_x_axis_text_hjust = 1,
                                      facet_legend_bottom = TRUE,
                                      #berakna_index = TRUE,
                                      x_axis_lutning = 45,
                                      diagram_titel = diagramtitel,
                                      diagram_capt =  diagram_capt,
                                      stodlinjer_avrunda_fem = TRUE,
                                      output_mapp = output_mapp,
                                      filnamn_diagram = diagramfilnamn,
                                      skriv_till_diagramfil = spara_diagrambildfil)
  
  gg_list <- c(gg_list, list(gg_obj))
  diagramfil <- "tf_foraldrapenning_manad.png"
  names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  }
  
  return(gg_list)
  
  # flik_lista[[1]] = stress_df
  # 
  # names(flik_lista) = c("Stress")
  
  #Sparar data till Excel
  # if (spara_data==TRUE){
  #   openxlsx::write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  # }
  
}
