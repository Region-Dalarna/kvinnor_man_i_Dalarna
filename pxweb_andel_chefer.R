## =================================================================================================================
# Skript som laddar hem data för andel chefer från SCB och skapar ett diagram
# =================================================================================================================
library(pxweb)
library(httr)
library(askpass)
library(dplyr)
library(tidyr)
library(stringi)
library(svDialogs)
library(tidyverse)
library(here)

# Laddar in de funktioner som används för att skapa diagram
source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)

options(dplyr.summarise.inform = FALSE)

#test_list <- diag_chefer(skapa_fil = FALSE)

diag_chefer<-function(region_vekt = "20", 
                               output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                               skapa_fil = TRUE,
                               andel_chefer=TRUE,
                               andel_chefer_linje=TRUE){
  
  # ========================================== Inställningar ============================================
  # Text till diagram
  diagram_capt <- c("Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Andel chefer av sysselsatta med ett klassificerat yrke (20-64 år) uppdelat på utbildningsnivå",
                    "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Förändring i andelen chefer av sysselsatta med ett klassificerat yrke (20-64 år) för samtliga utbildningsnivåer")
  # Mapp där diagrammet hamnar
  nyckel_mapp <- "G:/Samhällsanalys/Automatisering och R/nycklar/"
  
  BaraEttLän <- region_vekt
  #berakna_index <- TRUE
  ValdGeografi <- c(hamtaregion_kod_namn(region_vekt)$region)
  #SummeringsVar <- "BRP, per invånare, löpande priser, tkr"
  FragaSumVar = FALSE                # om TRUE får användaren välja summeringsvariabel i en dialogruta
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  i=1 # Räknare som används för att lägga till objekt i listan
  objektnamn <- c()
  
  #==========================================================================================================
  # För att komma förbi proxyn
  set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090, username = Sys.getenv("userid"), password = Sys.getenv("pwd")))
  set_config(config(ssl_verifypeer = 0L))
  # =============================================== API-uttag ===============================================
  # "Adresser" till SCBs databas
  url1 <- "https://api.scb.se"
  url2 <- "/OV0104/v1/doris/sv/ssd/START/AA/AA0003/AA0003B/IntGr1LanKonUtb"
  url3 <- paste0(url1, url2)
  
  #https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AA__AA0003__AA0003B/IntGr1LanKonUtb/
  
  # Variabler som skall tas ut
  varlista <-  list(Region=c(region_vekt),
                    Kon=c("1","2"),
                    UtbNiv=c("000","F","3","EU","US"),
                    BakgrVar=c("tot20-64"),
                    ContentsCode=c("0000001Y"),
                    Tid=c(as.character(2000:2020)))
  
  # Uttag av data
  px_uttag <- pxweb_get(url = url3,query = varlista)
  
  # Konverterar data till en Data Frame
  chefer_df <- as.data.frame(px_uttag, column.name.type = "text", variable.value.type = "text")
  
  # Andrar namnet på vissa kategorier
  chefer_df[chefer_df=="utbildningsnivå: förgymnasial utbildning"] <- "förgymnasial utbildning"
  chefer_df[chefer_df=="utbildningsnivå: gymnasial utbildning"] <- "gymnasial utbildning"
  chefer_df[chefer_df=="utbildningsnivå: eftergymnasial utbildning"] <- "eftergymnasial utbildning"
  
  if (andel_chefer==TRUE){
  
    diagramtitel <- paste0("Andel chefer ",max(chefer_df$år)," i ",ValdGeografi)
    diagramfilnamn <- paste0("andel_chefer.png")
    objektnamn <- c(objektnamn,"andel_chefer")
    
    # Skapar diagram för andel chefer 2021.
    gg_obj <- SkapaStapelDiagram(skickad_df =chefer_df[chefer_df$år==max(chefer_df$år) & chefer_df$utbildningsnivå!="utbildningsnivå: uppgift saknas",], 
                                 skickad_x_var = "utbildningsnivå", 
                                 skickad_y_var = "Andel i chefsposition, procent", 
                                 skickad_x_grupp = "kön",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = diagramfarger("kon"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt[1],
                                 diagram_facet = FALSE,
                                 x_axis_sort_value = TRUE,
                                 manual_y_axis_title="procent",
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)
   
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  if(andel_chefer_linje==TRUE){
    diagramtitel <- paste0("Förändring i andel chefer 2001-",max(chefer_df$år)," i ",ValdGeografi)
    diagramfilnamn <- paste0("andel_chefer_linje.png")
    objektnamn <- c(objektnamn,"andel_chefer_linje")
    
    # Skapar diagram för andel chefer 2021.
    gg_obj <- SkapaLinjeDiagram(skickad_df =chefer_df[chefer_df$år!="2000" & chefer_df$utbildningsnivå=="samtliga utbildningsnivåer",], 
                                 skickad_x_var = "år", 
                                 skickad_y_var = "Andel i chefsposition, procent", 
                                 skickad_x_grupp = "kön",
                                 x_axis_lutning=0,
                                 manual_color = diagramfarger("kon"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt[2],
                                 diagram_facet = FALSE,
                                 #manual_y_axis_title="procent",
                                 berakna_index = TRUE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
  i=i+1
  }
  names(gg_list) <- c(objektnamn)
  return(gg_list)
      
}
