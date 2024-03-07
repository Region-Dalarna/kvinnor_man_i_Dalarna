# Skript som anropar func_INtegration_syss.R för att skapa diagram över etableringstid och spara dem som .png-filer

pacman::p_load(tidyverse,here)

#test_list<-diag_etablering_tid()
diag_etablering_tid <- function(region_vekt="20",
                                Output_mapp="G:/skript/jon/Slask/"){

  # ================================ Här gör vi inställningar  ==============================================
  
  source("G:/skript/peter/integration/func_Integration_syss.R", encoding = "utf-8", echo = FALSE)
  options(dplyr.summarise.inform = FALSE)
  
  # --------------------------- inställning för hela skriptet - ändras sällan -------------------------------------
  
  skriv_till_Excelfil <- FALSE
  
  output_mapp_xls <- Output_mapp             
  output_mapp <- output_mapp_xls
  logga_path <- "G:/Samhällsanalys/MallarLoggor/logo_liggande_fri_svart.png"       # sökväg till logga för att kunna lägga in den i diagrammen
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
  i=1
  # --------------------------- inställningar som görs per körning ------------------------------------------
  
  # Välj region med kommun- eller länskod. Välj "riket" TRUE om vi vill hämta ner statistik om riket, FALSE om vi vill hämta statistik om Dalarna
  vald_region <- c(region_vekt)          
  ta_med_dataetiketter <- FALSE
  
  bara_en_region <- TRUE         # om TRUE så visas bara den eller de regioner som valts i raden ovan, annars jämförs
  # med regionens övriga kommuner, gäller bara om kommuner väljs
  
  # De diagram vi väljer
  huvudnamn <- c("integration_syss_nyanl")
  # alla rapporter:
  #c("integration_syss_lagutb", "integration_syss_nyanl", "integration_syss_utrFodd", "integration_syss_utomeurpFodd")
  
  
  # övriga förinställda diagram som finns (och som läggs som huvudnamn):
  # integration_syss_utrFodd
  # integration_syss_utomeurpFodd
  # integration_syss_lagutb
  # integration_syss_gymn_utb
  # integration_syss_hogutb
  # integration_syss_nyanl
  
  objektnamn<-"etablering_dalarna"
  
  ladda_ned_api <- TRUE
  for (rapportlista in 1:length(huvudnamn)) {
    for (reglista in 1:length(vald_region)){
      gg_obj <-SkapaIntegrationsDiagram(huvudnamn[rapportlista], 
                               vald_region[reglista],
                               skriv_till_Excelfil, 
                               output_mapp_xls,
                               output_mapp, 
                               logga_path,
                               diagram_capt,
                               bara_en_region,
                               ladda_ned_api,
                               dataetiketter = ta_med_dataetiketter)
    }
  }
  gg_list[[i]] <-gg_obj
  i=i+1
  
  names(gg_list)<-c(objektnamn)
  return(gg_list)
  
}
