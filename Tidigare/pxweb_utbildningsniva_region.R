## =================================================================================================================
# Skript som laddar hem data för utbildningsnivå från SCB och skapar ett diagram
# =================================================================================================================

library(pxweb)
library(httr)
library(askpass)
library(writexl)
#library(data.table)
library(dplyr)
library(tidyr)
library(stringi)
library(svDialogs)
library(openxlsx)
library(tidyverse)
library(here)

# Laddar in de funktioner som används för att skapa diagram
source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)

#test_list <- diag_utbildningsniva(skapa_fil = TRUE)

diag_utbildningsniva<-function(region_vekt = "20", 
                               output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                               skapa_fil = TRUE){
  
  # ========================================== Inställningar ============================================
  # Text till diagram
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
  # Mapp där diagrammet hamnar
  nyckel_mapp <- "G:/Samhällsanalys/Automatisering och R/nycklar/"
  
  # region_vekt = "20"
  # output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/"
  # skapa_fil = FALSE
   
  BaraEttLän <- region_vekt
  #berakna_index <- TRUE
  ValdGeografi <- c(hamtaregion_kod_namn(region_vekt)$region,"Riket")
  #SummeringsVar <- "BRP, per invånare, löpande priser, tkr"
  FragaSumVar = FALSE                # om TRUE får användaren välja summeringsvariabel i en dialogruta
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  i=1 # Räknare som används för att lägga till objekt i listan
  j=1 # Räknare som används för att namnge objekten i listan
  
  #==========================================================================================================
  # För att komma förbi proxyn
  set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090, username = Sys.getenv("userid"), password = Sys.getenv("pwd")))
  set_config(config(ssl_verifypeer = 0L))
  # =============================================== API-uttag ===============================================
  # "Adresser" till SCBs databas
  url1 <- "https://api.scb.se"
  url2 <- "/OV0104/v1/doris/sv/ssd/UF/UF0506/UF0506B/Utbildning"
  url3 <- paste0(url1, url2)
  
  # Variabler som skall tas ut
  varlista <-  list(Region=c(region_vekt),
                    UtbildningsNiva=c("1","2","3","4","5","6","7","US"),
                    Kon=c("1","2"),
                    Alder= as.character(25:64),
                    ContentsCode=c("UF0506A1"),
                    Tid=c("*"))
  
  # Dalarna och riket - Region=c("00","20")
  
  # Uttag av data
  px_uttag <- pxweb_get(url = url3,query = varlista)
  
  # Konverterar data till en Data Frame
  utbildning_df <- as.data.frame(px_uttag, column.name.type = "text", variable.value.type = "text")
  
  # Summerar på ålder
  utbildning_df <- utbildning_df %>% 
    group_by(region, utbildningsnivå, kön, år) %>% 
    summarise(Befolkning = sum(Befolkning))
  
  # Begränsar sample till 00-talet
  utbildning_df <- utbildning_df  %>% filter(år>1999)
  
  # Döper om variabeln befolkning till antal
  utbildning_df <- utbildning_df %>% rename(Antal=Befolkning)
  
  # Slår ihop eftergymnasiel utbildning (3 år) och forskarutbildning till en grupp
  utbildning_df[utbildning_df=="eftergymnasial utbildning, 3 år eller mer"|utbildning_df=="forskarutbildning"] <- "eftergymnasial utbildning, 3 år eller mer"

  # Summerar utbildning kopplat till det totala datasettet
  utbildning_kon_summering <- utbildning_df %>% 
    group_by(år,region,kön,utbildningsnivå) %>% 
      summarise(Antal = sum(Antal)) %>% 
        mutate(andel = (Antal/sum(Antal))*100)        # beräknar andel av högutbildade - OBS! Kontrollera att det blev rätt
  
  # Skapar en faktorvariabel som styr vilken ordning som utbildningsgrupper visas
  utbildning_kon_summering$utbildningsnivå <- factor(utbildning_kon_summering$utbildningsnivå, levels = c("eftergymnasial utbildning, 3 år eller mer","eftergymnasial utbildning, mindre än 3 år","gymnasial utbildning 3 år","gymnasial utbildning, högst 2 år","förgymnasial utbildning, 9 (10) år","förgymnasial utbildning kortare än 9 år")[6:1])
  
  # De år vi vill fokusera på - gör om skriptet för att bara fokusera på senaste år
  #valda_ar=c(min(utbildning_kon_summering$år),"2010",max(utbildning_kon_summering$år))
  valda_ar <- max(utbildning_kon_summering$år)
  diagramtitel <-paste0("Utbildningsnivå (25-64 år) i ",ValdGeografi[1]," ",valda_ar)
  diagramfilnamn <- paste0("utbildningsniva_", ValdGeografi[1], ".png")
  ifelse(j==1,objektnamn <- paste0("utbildningsniva_", ValdGeografi[1]),objektnamn <- c(objektnamn,paste0("utbildningsniva_", ValdGeografi[1])))
  j=j+1
    # Skapar diagram för utbildningnivå på länsnivå 2020
  gg_obj <- SkapaStapelDiagram(skickad_df <- utbildning_kon_summering %>%
                                 filter(år%in%valda_ar,utbildningsnivå!="uppgift om utbildningsnivå saknas"), 
                                 skickad_x_var = "utbildningsnivå", 
                                 skickad_y_var = "andel", 
                                 skickad_x_grupp = "kön",
                                 #skickad_filter_OR_vect = "Högre utbildningar", 
                                 #skickad_filter_OR_var = "utbildningsnivå",
                                 diagram_liggande=TRUE,
                                 # manual_x_axis_text_vjust=1,
                                 # manual_x_axis_text_hjust=1,
                                 x_axis_lutning = 0,
                                 x_axis_sort_value=F,
                                 manual_color = diagramfarger("kon"),
                                 manual_y_axis_title="procent",
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 diagram_facet = FALSE,
                                 facet_grp ="kön",
                                 facet_scale = "fixed",
                                 facet_legend_bottom=TRUE,
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)
    
  gg_list[[i]] <-gg_obj
  i=i+1
  names(gg_list)<-c(objektnamn)
  return(gg_list)
  
}
