## =================================================================================================================
# Skript som laddar hem data för disponibel inkomst från SCB och skapar två diagram för Dalarna
# =================================================================================================================

library(pxweb)
library(httr)
library(askpass)
library(writexl)
library(dplyr)
library(tidyr)
library(stringi)
library(tidyverse)

# Laddar in de funktioner som används för att skapa diagram
source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)

options(dplyr.summarise.inform = FALSE)

#test_list <- diag_disponibelinkomst(skapa_fil = FALSE)

diag_disponibelinkomst<-function(region_vekt = "20", 
                             output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                             skapa_fil = TRUE){
  
  # ========================================== Inställningar ============================================
  # Text till diagram
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Disponibel inkomst (18+ år)"

  # valda_farger <-c(rgb(169,208,142, maxColorValue = 255),
  #                  rgb(112,173,71, maxColorValue = 255))
  
  ValdGeografi <- hamtaregion_kod_namn(region_vekt)$region
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  i=1 # Räknare som används för att lägga till objekt i listan
  
  # Tillfälliga variabler för att testköra
  # output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/"
  # skapa_fil = TRUE
  # region_vekt="20"
  
  #==========================================================================================================
  # För att komma förbi proxyn
  set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090, username = Sys.getenv("userid"), password = Sys.getenv("pwd")))
  set_config(config(ssl_verifypeer = 0L))
  # =============================================== API-uttag ===============================================
  # "Adresser" till SCBs databas
  url1 <- "https://api.scb.se"
  url2 <- "/OV0104/v1/doris/sv/ssd/HE/HE0110/HE0110G/Tab4bDispInkN"
  url3 <- paste0(url1, url2)
  
  # Variabler som skall tas ut
  varlista <-  list("Region"=c(region_vekt),
                    "Hushallstyp"=c("*"),
                    "Alder"=c("*"),
                    "ContentsCode"=c("000000KD","000000KE","000000KG","000000KF"),
                    "Tid"=c("*"))
  
  # Uttag av data
  px_uttag <- pxweb_get(url = url3,query = varlista)
  
  # Konverterar data till en Data Frame
  disponibel_inkomst_df <- as.data.frame(px_uttag, column.name.type = "text", variable.value.type = "text")
  
  # Filtrerar dataset på de grupper och år vi vill fokusera på
  disponibel_inkomst_utvalda_df <- disponibel_inkomst_df %>% 
    filter(hushållstyp=="ensamstående kvinnor utan barn"|hushållstyp=="ensamstående kvinnor med barn 0-19 år"|hushållstyp=="ensamstående män utan barn"|hushållstyp=="ensamstående män med barn 0-19 år"|hushållstyp=="sammanboende utan barn"|hushållstyp=="sammanboende med barn 0-19 år") %>% 
      filter(ålder=="18+ år") %>% 
        filter(år==min(år)|år==max(år))
  
  diagramtitel <- paste0("Disponibel inkomst i ", ValdGeografi)
  diagramfilnamn <- paste0("disponibel_inkomst", ValdGeografi, ".png")
  objektnamn <- paste0("disponibel_inkomst", ValdGeografi)
  
  # Skapar diagram för disponibel inkomst i Dalarna där män jämförs med kvinnor.
  gg_obj <- SkapaStapelDiagram(skickad_df = disponibel_inkomst_utvalda_df, 
                               skickad_x_var = "hushållstyp", 
                               skickad_y_var = "Medianvärde, tkr", 
                               skickad_x_grupp = "år",
                               # manual_x_axis_text_vjust=0.5,
                               x_axis_lutning = 0,
                               manual_color = diagramfarger("gron_tva_fokus_morkgron"),
                               diagram_titel = diagramtitel,
                               diagram_capt =  diagram_capt,
                               diagram_facet = FALSE,
                               berakna_index = FALSE,
                               diagram_liggande = TRUE,
                               output_mapp = output_mapp,
                               filnamn_diagram = diagramfilnamn,
                               skriv_till_diagramfil = skapa_fil)
  
  gg_list[[i]] <-gg_obj
  i=i+1
  
  names(gg_list) <-  c(objektnamn)
  return(gg_list)
}

