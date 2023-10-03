## =================================================================================================================
# Skript som laddar hem data för medianinkomst från SCB och skapar två diagram för Dalarna
# =================================================================================================================

library(pxweb)
library(httr)
library(askpass)
library(writexl)
library(dplyr)
library(tidyr)
library(stringi)
library(svDialogs)
library(openxlsx)
library(tidyverse)


# Laddar in de funktioner som används för att skapa diagram
source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)

options(dplyr.summarise.inform = FALSE)

#test_list <- diag_medianinkomst(skapa_fil = FALSE)

diag_medianinkomst<-function(region_vekt = "20", 
                             output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                             skapa_fil = TRUE){
  
  # ========================================== Inställningar ============================================
  # Text till diagram
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas. Bearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Sammaräknad förvärvsinkomst (20-64 år), dvs alla skattepliktiga inkomster före skatt (dock ej kapitalinkomster)."
  nyckel_mapp <- "G:/Samhällsanalys/Automatisering och R/nycklar/"
  
  valda_farger <-c(rgb(169,208,142, maxColorValue = 255),
                   rgb(112,173,71, maxColorValue = 255))
  
  ValdGeografi <- hamtaregion_kod_namn(region_vekt)$region
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  i=1 # Räknare som används för att lägga till objekt i listan

  #==========================================================================================================
  # För att komma förbi proxyn
  set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090, username = Sys.getenv("userid"), password = Sys.getenv("pwd")))
  set_config(config(ssl_verifypeer = 0L))
  # =============================================== API-uttag ===============================================
  # "Adresser" till SCBs databas
  url1 <- "https://api.scb.se"
  url2 <- "/OV0104/v1/doris/sv/ssd/HE/HE0110/HE0110A/SamForvInk1"
  url3 <- paste0(url1, url2)
  
  # Variabler som skall tas ut
  varlista <-  list("Region"=c("00",region_vekt),
                    "Kon"=c("1","2","1+2"),
                    "Alder"=c("20-64"),
                    "Inkomstklass"=c("TOT"),
                    "ContentsCode"=c("HE0110J8"),
                    "Tid"=c("*"))
  
  # Uttag av data
  px_uttag <- pxweb_get(url = url3,query = varlista)
  
  # Konverterar data till en Data Frame
  medianinkomst_df <- as.data.frame(px_uttag, column.name.type = "text", variable.value.type = "text")
  
  # Summerar på år
  medianinkomst_df <- medianinkomst_df %>% 
    group_by(region,kön, år) %>% 
    summarise("medianinkomst,tkr" = sum(`Medianinkomst, tkr`))
  
  # DF som enbart fokuserar på total inkomst (dvs. inte uppdelat på kvinnor och män)
  medianinkomst_df_total <- medianinkomst_df %>% filter (kön=="totalt")
  
  #DF där uppdelning är på kön
  medianinkomst_kon_df <- medianinkomst_df %>% filter (region!="Riket" & kön!="totalt")
  
  diagramtitel <- paste0("Medianinkomst i ", ValdGeografi)
  diagramfilnamn <- paste0("medianinkomst_kon_total_", ValdGeografi, ".png")
  objektnamn <- paste0("medianinkomst_kon_total_", ValdGeografi)
  
  # Skapar diagram för medianinkomsten i Dalarna där män jämförs med kvinnor.
  gg_obj <- SkapaStapelDiagram(skickad_df = medianinkomst_kon_df, 
                               skickad_x_var = "år", 
                               skickad_y_var = "medianinkomst,tkr", 
                               skickad_x_grupp = "kön",
                               manual_x_axis_text_vjust=1.2,
                               manual_color = diagramfarger("kon"),
                               diagram_titel = diagramtitel,
                               diagram_capt =  diagram_capt,
                               diagram_facet = FALSE,
                               berakna_index = FALSE,
                               output_mapp = output_mapp,
                               filnamn_diagram = diagramfilnamn,
                               skriv_till_diagramfil = skapa_fil)
  
  gg_list[[i]] <-gg_obj
  i=i+1
  
  diagramtitel <- paste0("Förändring i medianinkomst i ", ValdGeografi)
  diagramfilnamn <- paste0("medianinkomst_kon_total_", ValdGeografi, ".png")
  objektnamn <- c(objektnamn,paste0("medianinkomst_kon_total_forandring_", ValdGeografi))
  
  # Linjediagram där utvecklingen för kvinnors och mäns löner jämförs
  gg_obj <- SkapaLinjeDiagram(skickad_df = medianinkomst_kon_df, 
                              skickad_x_var = "år", 
                              skickad_y_var = "medianinkomst,tkr", 
                              skickad_x_grupp = "kön",
                              #manual_x_axis_text_vjust=1.2,
                              #skickad_filter_OR_var = "region",
                              manual_color = diagramfarger("kon"),
                              diagram_titel = diagramtitel,
                              diagram_capt =  diagram_capt,
                              diagram_facet = FALSE,
                              berakna_index = TRUE,
                              output_mapp = output_mapp,
                              filnamn_diagram = diagramfilnamn,
                              skriv_till_diagramfil = skapa_fil)
  
  gg_list[[i]] <-gg_obj
  names(gg_list) <-  c(objektnamn)
  return(gg_list)
}

