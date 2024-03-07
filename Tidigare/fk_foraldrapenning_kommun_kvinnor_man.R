library(openxlsx)
library(tidyverse)
library(here)

# Laddar in de funktioner som används för att skapa diagram
source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)

#test_list <- diag_foraldrapenning_kommun(skapa_fil=FALSE)
diag_foraldrapenning_kommun <- function(region_vekt = "20", 
                                 output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                 skapa_fil = TRUE,
                                 antal_mottagare_stapel=TRUE,
                                 andel_mottagare_stapel=TRUE){
  
  # ========================================== Inställningar ============================================
  # Text till diagram
  diagram_capt <- "Källa: Försäkringskassan. Bearbetning: Samhällsanalys, Region Dalarna."
  # 
  # region_vekt = "20"
  # output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/"
  # skapa_fil = FALSE
  
  ValdGeografi <- hamtaregion_kod_namn(region_vekt)$region
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  i=1 # Räknare som används för att lägga till objekt i listan
  j=1 # Räknare som används för att namnge objekt
  
  #==========================================================================================================
  
  # Läser in data från Excel
  foraldrapenning_df <-read.xlsx(here("indata","/","Foraldrapenning_antal.xlsx"),sheet=1)
  
  # Filtrerar på samtliga kommuner och bara kvinnor respektive män
  foraldrapenning_kommun_ar_df <- foraldrapenning_df %>%
    filter(Kommun!="Samtliga kommuner",Kommun!="Riket") %>%
      filter(Kön!="Kvinnor och män") %>%
        filter(År==max(foraldrapenning_df$År)) %>% 
          rename("år"='År')
  
  # Tar bort kommun-nummer
  foraldrapenning_kommun_ar_df$Kommun<-substr(foraldrapenning_kommun_ar_df$Kommun,6,nchar(foraldrapenning_kommun_ar_df$Kommun))
  
  if(antal_mottagare_stapel==TRUE){
    
    diagramtitel <- paste0("Antal mottagare av föräldrapenning i ", ValdGeografi)
    diagramfilnamn <- paste0("Foraldrapenning_antal_kommun", ValdGeografi, ".png")
    ifelse(j==1,objektnamn <- paste0("Foraldrapenning_antal_kommun", ValdGeografi),objektnamn <-c(objektnamn,paste0("Foraldrapenning_antal_kommun", ValdGeografi)))
    j=j+1
    # Skapar diagram för i Dalarna där män jämförs med kvinnor.
    gg_obj <- SkapaStapelDiagram(skickad_df = foraldrapenning_kommun_ar_df%>% 
                                   mutate(Kön=tolower(Kön)), 
                                 skickad_x_var = "Kommun", 
                                 skickad_y_var = "Antal.mottagare", 
                                 skickad_x_grupp = "Kön",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = diagramfarger("kon"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 diagram_facet = FALSE,
                                 berakna_index = FALSE,
                                 x_axis_sort_value = TRUE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  if(andel_mottagare_stapel==TRUE){
    
    diagramtitel <- paste0("Andel som mottar föräldrapenning i ", ValdGeografi)
    diagramfilnamn <- paste0("Foraldrapenning_andel_kommun", ValdGeografi, ".png")
    ifelse(j==1,objektnamn <- paste0("Foraldrapenning_andel_kommun", ValdGeografi),objektnamn <-c(objektnamn,paste0("Foraldrapenning_andel_kommun", ValdGeografi)))
    j=j+1
    # Skapar diagram för i Dalarna där män jämförs med kvinnor.
    gg_obj <- SkapaStapelDiagram(skickad_df = foraldrapenning_kommun_ar_df%>% 
                                   mutate(Kön=tolower(Kön)), 
                                 skickad_x_var = "Kommun", 
                                 skickad_y_var = "Andel.nettodagar.per.kön", 
                                 skickad_x_grupp = "Kön",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = diagramfarger("kon"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 diagram_facet = FALSE,
                                 berakna_index = FALSE,
                                 geom_position_stack = TRUE,
                                 x_axis_sort_value = TRUE,
                                 x_axis_sort_grp = 1,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }

  names(gg_list) <- c(objektnamn)
  return(gg_list)
}

