diag_arbetsloshet_tidsserie <-function(vald_region = "20",
                                     alder_klartext = "20-64 år", #Välj enbart 1. Finns: 15-19 år, 16-19 år, 20-24 år, 25-29 år, 30-34 år, 35-39 år, 40-44 år, 45-49 år, 50-54 år, 55-59 år, 60-64 år, 65-69 år, 70-74 år, 15-74 år, 16-64 år, 16-65 år, 20-64 år, 20-65 år
                                     tid = "*",
                                     diagramtitel_tabort = FALSE,
                                     spara_diagrambildfil = FALSE,
                                     spara_dataframe_till_global_environment = FALSE,
                                     output_mapp = "G:/Samhällsanalys/API/Fran_R/Utskrift/" ){
  
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(tidyverse)
  
  # =============================================== API-uttag ===============================================
  # "Adresser" till SCBs databas
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_bas_arbstatus_region_kon_alder_fodelseregion_prel_manad_ArbStatusM_scb.R")
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)

  arbetsmarknadsstatus_df = hamta_bas_arbstatus_region_kon_alder_fodelseregion_prel_manad_scb(region_vekt = vald_region,
                                                                                              alder_klartext = alder_klartext,
                                                                                              kon_klartext = c("kvinnor","män"),
                                                                                              fodelseregion_klartext = c("inrikes född", "utrikes född"),
                                                                                              cont_klartext = "arbetslöshet",
                                                                                              wide_om_en_contvar = FALSE,
                                                                                              tid_koder = tid)  %>% 
   manader_bearbeta_scbtabeller() %>% 
    mutate(region = skapa_kortnamn_lan(region))
  
  if(spara_dataframe_till_global_environment) {
    assign("arbetsloshet_bas_manad_df", arbetsmarknadsstatus_df, envir = .GlobalEnv)
  }
  
  diagram_capt <- "Källa: SCB (BAS).\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Månadsdata. Diagrammet visar medelvärdet för året"
  
  diagramtitel <- paste0("Arbetslöshet i åldersgruppen " ,unique(arbetsmarknadsstatus_df$ålder)," i ",unique(arbetsmarknadsstatus_df$region))
  diagramfilnamn <- paste0("arbetsloshet_bas_tidsserie_bakgrund.png")
  
  gg_obj <- SkapaStapelDiagram(skickad_df = arbetsmarknadsstatus_df, 
                                            skickad_x_var = "månad_år",
                                            skickad_y_var = "varde",
                                            skickad_x_grupp = "kön",
                                            manual_x_axis_text_vjust=1,
                                            manual_x_axis_text_hjust=1,
                                            utan_diagramtitel = diagramtitel_tabort,
                                            manual_color = diagramfarger("kon"),
                                            diagram_titel = diagramtitel,
                                            diagram_capt =  diagram_capt,
                                            diagram_facet = TRUE,
                                            facet_legend_bottom = TRUE,
                                            stodlinjer_avrunda_fem = TRUE,
                                            facet_grp = "födelseregion",
                                            facet_scale = "fixed",
                                            x_axis_visa_var_xe_etikett = 6,
                                            x_axis_lutning = 45,
                                            manual_y_axis_title = "procent",
                                            output_mapp = output_mapp,
                                            filnamn_diagram = diagramfilnamn,
                                            skriv_till_diagramfil = spara_diagrambildfil)
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  
  return(gg_list)
  
}


