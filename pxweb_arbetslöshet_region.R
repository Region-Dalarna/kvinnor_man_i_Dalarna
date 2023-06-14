## =================================================================================================================
# Skript som laddar hem data för arbetslöshet från SCB och skapar ett diagram
# =================================================================================================================

library(pxweb)
library(httr)
library(askpass)
library(dplyr)
library(tidyr)
library(stringi)
library(svDialogs)
library(tidyverse)


# Laddar in de funktioner som används för att skapa diagram
source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)

options(dplyr.summarise.inform = FALSE)

#test_list <- diag_arbetsloshet(skapa_fil = FALSE)

diag_arbetsloshet <-function(region_vekt = "20", 
                             output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                             skapa_fil = TRUE){
  
  # ========================================== Inställningar ============================================
  # Text till diagram
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Arbetslöshet enligt arbetskraftsundersökningen (AKU)"
 
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
  url2 <- "/OV0104/v1/doris/sv/ssd/AM/AM0401/AM0401N/NAKUBefolkningLAr"
  url3 <- paste0(url1, url2)
  
  # Variabler som skall tas ut
  varlista <-  list("Region"=c(hamtaAllaLan()),
                    "Arbetskraftstillh"=c("ALÖS"),
                    "Kon"=c("1","2","1+2"),
                    "ContentsCode"=c("AM0401VF"),
                    "Tid"=c("2021"))
  
  # Uttag av data
  px_uttag <- pxweb_get(url = url3,query = varlista)
  
  # Konverterar data till en Data Frame
  arbetslösa_df <- as.data.frame(px_uttag, column.name.type = "text", variable.value.type = "text")
  
  # Vill bara titta på kön, inte båda
  arbetslösa_df <- arbetslösa_df %>% 
    filter(kön!="totalt")
  
  # Plockar ut de regioner där data saknas för någon grupp. Dessa tas sedan bort från dataset
  temp_list=list()
  j=1
  while (j <= length(arbetslösa_df$region)){
    if(is.na(arbetslösa_df$Procent[j])){
      temp_list <-append(temp_list,arbetslösa_df$region[j])
    }
       j=j+1
  }
  
  arbetslösa_df <- arbetslösa_df %>% 
    filter(!(region%in%temp_list) )
  
  rm(temp_list)
  
  diagramtitel <- paste0("Arbetslöshet ",max(arbetslösa_df$år))
  diagramfilnamn <- paste0("arbetslöshet_Sverige.png")
  objektnamn <- paste0("Arbetslöshet")
  
  # Skapar diagram för medianinkomsten i Dalarna där män jämförs med kvinnor.
  gg_obj <- SkapaStapelDiagram(skickad_df = arbetslösa_df, 
                               skickad_x_var = "region", 
                               skickad_y_var = "Procent", 
                               skickad_x_grupp = "kön",
                               manual_x_axis_text_vjust=0.7,
                               manual_color = diagramfarger("kon"),
                               diagram_titel = diagramtitel,
                               diagram_capt =  diagram_capt,
                               diagram_facet = FALSE,
                               x_axis_sort_value = TRUE,
                               berakna_index = FALSE,
                               output_mapp = output_mapp,
                               filnamn_diagram = diagramfilnamn,
                               skriv_till_diagramfil = skapa_fil)
  
  gg_list[[i]] <-gg_obj
  i=i+1
  names(gg_list)<-objektnamn
  return(gg_list)
}

