library(openxlsx)
library(tidyverse)
library(here)

# Laddar in de funktioner som används för att skapa diagram
source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)

#test_list <- diag_tf_foraldrapenning (skapa_fil=FALSE)
diag_tf_foraldrapenning <- function(region_vekt = "20", 
                                   output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                   skapa_fil = TRUE,
                                   antal_nettodagar_stapel=TRUE,
                                   antal_nettodagar_linje=TRUE,
                                   antal_nettodagar_kommun_stapel=TRUE){
  
  # ========================================== Inställningar ============================================
  # Text till diagram
  diagram_capt <- "Källa: Försäkringskassan. Bearbetning: Samhällsanalys, Region Dalarna."
  
  # region_vekt = "20"
  # output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/"
  # skapa_fil = FALSE
  
  ValdGeografi <- hamtaregion_kod_namn(region_vekt)$region
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  i=1 # Räknare som används för att lägga till objekt i listan
  j=1 # Räknare som används för att namnge objekt
  
  #==========================================================================================================
  
  # Läser in data från Excel
  tf_foraldrapenning_df <-read.xlsx(here("indata","/","Vab_antal.xlsx"),sheet=1)

  tf_foraldrapenning_lan_ar_df <- tf_foraldrapenning_df %>%
    filter(Kommun=="Samtliga kommuner") %>%
      filter(Kön!="Kvinnor och män") %>% 
        rename("år"='År')
  
  tf_foraldrapenning_kommun_ar_df <- tf_foraldrapenning_df %>%
    filter(Kommun!="Samtliga kommuner",Kommun!="Riket") %>%
      filter(Kön!="Kvinnor och män") %>%
        filter(År==max(tf_foraldrapenning_df$År)) %>%
          rename("år"='År')
  
  # Tar bort kommun-nummer
  tf_foraldrapenning_kommun_ar_df$Kommun<-substr(tf_foraldrapenning_kommun_ar_df$Kommun,6,nchar(tf_foraldrapenning_kommun_ar_df$Kommun))
  
  if(antal_nettodagar_stapel==TRUE){
    diagramtitel <- paste0("Vård av barn, antal uttagna nettodagar i ", ValdGeografi)
    diagramfilnamn <- paste0("Vab_antal", ValdGeografi, ".png")
    ifelse(j==1,objektnamn <- paste0("Vab_antal", ValdGeografi),objektnamn <-c(objektnamn,paste0("Vab_antal", ValdGeografi)))
    j=j+1
    # Skapar diagram för i Dalarna där män jämförs med kvinnor.
    gg_obj <- SkapaStapelDiagram(skickad_df = tf_foraldrapenning_lan_ar_df%>% 
                                   mutate(Kön=tolower(Kön)), 
                                 skickad_x_var = "år", 
                                 skickad_y_var = "Antal.nettodagar", 
                                 skickad_x_grupp = "Kön",
                                 x_axis_lutning = 0,
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
  }
  
  if(antal_nettodagar_linje==TRUE){
    
    diagramtitel <- paste0("Vård av barn, förändring i antal uttagna nettodagar i ", ValdGeografi)
    diagramfilnamn <- paste0("Vab_antal_linje", ValdGeografi, ".png")
    ifelse(j==1,objektnamn <- paste0("Vab_antal_linje", ValdGeografi),objektnamn <-c(objektnamn,paste0("Vab_antal_linje", ValdGeografi)))
    j=j+1
    # Skapar diagram för i Dalarna där män jämförs med kvinnor.
    gg_obj <- SkapaLinjeDiagram(skickad_df = tf_foraldrapenning_lan_ar_df%>% 
                                  mutate(Kön=tolower(Kön)), 
                                 skickad_x_var = "år", 
                                 skickad_y_var = "Antal.nettodagar", 
                                 skickad_x_grupp = "Kön",
                                 manual_color = diagramfarger("kon"),
                                 berakna_index = TRUE,
                                 x_axis_lutning = 0,
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 diagram_facet = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  if(antal_nettodagar_kommun_stapel==TRUE){
    
    diagramtitel <- paste0("Vård av barn, antal uttagna nettodagar i ", ValdGeografi)
    diagramfilnamn <- paste0("Vab_antal_kommun", ValdGeografi, ".png")
    ifelse(j==1,objektnamn <- paste0("Vab_antal_kommun", ValdGeografi),objektnamn <-c(objektnamn,paste0("Vab_antal_kommun", ValdGeografi)))
    j=j+1
    # Skapar diagram för i Dalarna där män jämförs med kvinnor.
    gg_obj <- SkapaStapelDiagram(skickad_df = tf_foraldrapenning_kommun_ar_df%>% 
                                   mutate(Kön=tolower(Kön)), 
                                skickad_x_var = "Kommun", 
                                skickad_y_var = "Antal.nettodagar", 
                                skickad_x_grupp = "Kön",
                                manual_x_axis_text_vjust=1,
                                manual_x_axis_text_hjust=1,
                                manual_color = diagramfarger("kon"),
                                berakna_index = FALSE,
                                diagram_titel = diagramtitel,
                                diagram_capt =  diagram_capt,
                                diagram_facet = FALSE,
                                x_axis_lutning = 45,
                                x_axis_sort_value = TRUE,
                                output_mapp = output_mapp,
                                filnamn_diagram = diagramfilnamn,
                                skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  names(gg_list) <- c(objektnamn)
  return(gg_list)
}

