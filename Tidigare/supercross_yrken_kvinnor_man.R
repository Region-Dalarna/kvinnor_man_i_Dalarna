# R-skript som används för att ta fram data för de vanligaste yrkena inom byggverksamhet uppdelat på kön

library(writexl)
#library(data.table)
library(dplyr)
library(tidyr)
library(stringi)
library(svDialogs)
library(openxlsx)
library(tidyverse)
library(here)

# Läser in de funktioner som används i skriptet
source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)
options(dplyr.summarise.inform = FALSE)

#test_list <- diag_syss_bygg_kon()

diag_syss_bygg_kon <- function(skapa_fil = FALSE,
                               output_mapp="G:/skript/jon/Slask/",
                               yrken_man = TRUE,
                               yrken_kvinna = TRUE){
  # ========================================== Inställningar ============================================
  diagram_capt <- c("Källa: SCB\nBearbetning: Samhällsanalys, Region Dalarna")
  
  gg_list=list()
  i<-1
  #=======================================================================================================
  # Läser in från en excelfil med data hämtat från Supercross
  branscher_yrken_syss <- read.xlsx("G:/skript/jon/Presentationer/Byggforetagen juni/Indata/uttag_bransch_yrke_kon.xlsx",sheet=1)
  
  # Börjar med att gruppera på Dalarna (för att skippa kommunnivå)
  branscher_yrken_syss_dalarna <- branscher_yrken_syss %>% 
    group_by(Bransch,Yrke,Kon) %>% 
      summarise(summa=sum(Dagbefolkning)) %>% 
        ungroup()
  
  # Väljer de 10 största yrkena inom byggverksamhet för män - Dalarnas län
  branscher_yrken_man <- branscher_yrken_syss_dalarna %>% 
    filter(Kon=='Män',Yrke!='Uppgift saknas') %>%
      slice_max(summa,n=10)
  
  # Väljer de 10 största yrkena inom byggverksamhet för kvinnor - Dalarnas län
  branscher_yrken__kvinnor <- branscher_yrken_syss_dalarna %>% 
    filter(Kon=='Kvinnor',Yrke!='Uppgift saknas') %>%
      slice_max(summa,n=10)
  
  #Kontrollera så att det stämmer
  #Samtliga
  branscher_yrken_syss_bygg_kontroll <- branscher_yrken_syss_dalarna %>%
      summarise(antal=sum(summa))
  
  # Byggverksamhet Dalarnas län
  # Män
  if(yrken_man == TRUE){
    
    pre_titel <- "Yrken med högst antal förvärvsarbetande män (dagbef)"
    diagram_titel <- paste0(pre_titel, " i ","Dalarnas län"," 2020")
    diagram_typ <- "yrken_man"
    diagramfil=paste0(diagram_typ,".png")
    ifelse(i==1,objektnamn <- diagram_titel,objektnamn <- c(objektnamn,diagram_titel))
    
    gg_obj <- SkapaStapelDiagram(skickad_df = branscher_yrken_man, 
                                 skickad_x_var = "Yrke", 
                                 skickad_y_var = "summa", 
                                 #skickad_x_grupp = "Ar",
                                 # manual_x_axis_text_vjust=1,
                                 # manual_x_axis_text_hjust=1,
                                 manual_y_axis_title = "Antal förvärvsarbetande",
                                 manual_color = diagramfarger("gron_fyra")[2],
                                 x_axis_sort_value = TRUE,
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 x_axis_lutning = 0,
                                 diagram_liggande = TRUE,
                                 diagram_facet = FALSE,
                                 #facet_grp="Ar",
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  # Kvinnor
  if(yrken_kvinna == TRUE){
    
    pre_titel <- "Yrken med högst antal förvärvsarbetande kvinnor (dagbef)"
    diagram_titel <- paste0(pre_titel, " i ","Dalarnas län"," 2020")
    diagram_typ <- "yrken_kvinnor"
    diagramfil=paste0(diagram_typ,".png")
    ifelse(i==1,objektnamn <- diagram_titel,objektnamn <- c(objektnamn,diagram_titel))
    
    # Tillverkning och utvinning Dalarnas län först och sedan Falun
    gg_obj <- SkapaStapelDiagram(skickad_df = branscher_yrken__kvinnor, 
                                 skickad_x_var = "Yrke", 
                                 skickad_y_var = "summa", 
                                 #skickad_x_grupp = "Ar",
                                 # manual_x_axis_text_vjust=1,
                                 # manual_x_axis_text_hjust=1,
                                 manual_y_axis_title = "Antal förvärvsarbetande",
                                 manual_color = diagramfarger("gron_fyra")[2],
                                 x_axis_sort_value = TRUE,
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 x_axis_lutning = 0,
                                 diagram_liggande = TRUE,
                                 diagram_facet = FALSE,
                                 #facet_grp="Ar",
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  names(gg_list) <- c(objektnamn)
  return(gg_list)
}



