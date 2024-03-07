library(openxlsx)
library(tidyverse)
library(here)

# Laddar in de funktioner som används för att skapa diagram
source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)

#test_list <- diag_forsakringskassan_kommun(skapa_fil=FALSE)
diag_forsakringskassan_kommun <- function(region_vekt = "20", 
                                  output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                  skapa_fil = TRUE,
                                  ohalsotal_stapel=TRUE,
                                  sjukpenningtal_stapel=TRUE,
                                  antal_per_forsakrad_stapel=TRUE,
                                  stress_stapel=TRUE){
  
  # ========================================== Inställningar ============================================
  # Text till diagram
  diagram_capt <- "Källa: Försäkringskassan.\n Bearbetning: Samhällsanalys, Region Dalarna."
  diagram_capt_ohälsa <- "Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Ohälsotalet: hur många dagar under en tolvmånadersperiod\nFörsäkringskassan betalar ut ersättning för nedsatt arbetsförmåga\n i förhållande till antalet försäkrade i åldrarna 16-64 år."
  diagram_capt_sjukpenning<-"Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Sjukpenningtalet är antalet dagar med sjukpenning och rehabiliteringspenning\nsom har betalats ut under en 12-månaders period.Den summan delas med antalet försäkrade i\nSverige som är i åldrarna 16–64 år."
  # region_vekt = "20"
  # output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/"
  # skapa_fil = FALSE
  
  ValdGeografi <- hamtaregion_kod_namn(region_vekt)$region
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  i=1 # Räknare som används för att lägga till objekt i listan
  j=1 # Räknare som används för att namnge objekt
  
  #==========================================================================================================
  
  # Läser in data från Excel
  ohalsotal_df <-read.xlsx(here("indata","/","Ohalsotalet.xlsx"),sheet=1)
  sjukpenningtal_df <-read.xlsx(here("indata","/","Sjukpenningtal.xlsx"),sheet=1)
  antal_per_forsakrad_df <- read.xlsx(here("indata","/","Antal_per_forsakrade.xlsx"),sheet=1)
  antal_stress_df <- read.xlsx(here("indata","/","Antal_stress.xlsx"),sheet=1)

  # Filtrerar på samtliga län,december varje år och bara kvinnor respektive män
  ohalsotal_kommun_ar_df <- ohalsotal_df %>%
    filter(Kommun!="Samtliga kommuner",Kommun!="Riket") %>%
      filter(Månad==12) %>% 
        filter(Kön!="Kvinnor och män")
  
  # Vill bara titta på det senaste året i samplet
  ohalsotal_kommun_ar_df <- ohalsotal_kommun_ar_df[ohalsotal_kommun_ar_df$År==max(ohalsotal_kommun_ar_df$År),]
  
  sjukpenningtal_kommun_ar_df <- sjukpenningtal_df %>%
    filter(Kommun!="Samtliga kommuner",Kommun!="Riket") %>%
      filter(Månad==12) %>% 
        filter(Kön!="Kvinnor och män") 
  
  # Vill bara titta på det senaste året i samplet
  sjukpenningtal_kommun_ar_df <- sjukpenningtal_kommun_ar_df[sjukpenningtal_kommun_ar_df$År==max(sjukpenningtal_kommun_ar_df$År),]
  
  antal_per_forsakrad_kommun_ar_df <- antal_per_forsakrad_df %>%
    filter(Kommun!="Samtliga kommuner",Kommun!="Riket") %>%
      filter(Månad==12) %>% 
        filter(Kön!="Kvinnor och män")
  
  # Vill bara titta på det senaste året i samplet
  antal_per_forsakrad_kommun_ar_df <- antal_per_forsakrad_kommun_ar_df[antal_per_forsakrad_kommun_ar_df$År==max(antal_per_forsakrad_kommun_ar_df$År),]
  
  antal_stress_kommun_ar_df <- antal_stress_df %>%
    filter(Kommun!="Samtliga kommuner",Kommun!="Riket") %>%
      filter(Månad==12) %>% 
        filter(Kön!="Kvinnor och män")
  
  # Vill bara titta på det senaste året i samplet
  antal_stress_kommun_ar_df <- antal_stress_kommun_ar_df[antal_stress_kommun_ar_df$År==max(antal_stress_kommun_ar_df$År),]
  
  # Måste ändra antalet sjukfall (stress) till integer, inte character
  antal_stress_kommun_ar_df$Antal.pågående.sjukfall <-as.integer(antal_stress_kommun_ar_df$Antal.pågående.sjukfall)
  
  # Tar bort kommun-nummer
  ohalsotal_kommun_ar_df$Kommun<-substr(ohalsotal_kommun_ar_df$Kommun,6,nchar(ohalsotal_kommun_ar_df$Kommun))
  sjukpenningtal_kommun_ar_df$Kommun<-substr(sjukpenningtal_kommun_ar_df$Kommun,6,nchar(sjukpenningtal_kommun_ar_df$Kommun))
  antal_per_forsakrad_kommun_ar_df$Kommun<-substr(antal_per_forsakrad_kommun_ar_df$Kommun,6,nchar(antal_per_forsakrad_kommun_ar_df$Kommun))
  antal_stress_kommun_ar_df$Kommun<-substr(antal_stress_kommun_ar_df$Kommun,6,nchar(antal_stress_kommun_ar_df$Kommun))
  
  if(ohalsotal_stapel==TRUE){
    
    diagramtitel <- paste0("Ohälsotalet i ", ValdGeografi," ",max(ohalsotal_kommun_ar_df$År))
    diagramfilnamn <- paste0("ohalsotal_kommun", ValdGeografi, ".png")
    ifelse(j==1,objektnamn <- paste0("ohalsotal_kommun_", ValdGeografi),objektnamn <-c(objektnamn,paste0("ohalsotal_kommun_", ValdGeografi)))
    j=j+1
    # Skapar diagram för i Dalarna där män jämförs med kvinnor.
    gg_obj <- SkapaStapelDiagram(skickad_df = ohalsotal_kommun_ar_df%>% 
                                   mutate(Kön=tolower(Kön)), 
                                 skickad_x_var = "Kommun", 
                                 skickad_y_var = "Ohälsotalet", 
                                 skickad_x_grupp = "Kön",
                                 manual_x_axis_text_vjust = 1, 
                                 manual_x_axis_text_hjust = 1,
                                 x_axis_lutning = 45,
                                 manual_color = diagramfarger("kon"),
                                 x_axis_sort_value = TRUE,
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt_ohälsa,
                                 diagram_facet = FALSE,
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  if(sjukpenningtal_stapel==TRUE){
    
    diagramtitel <- paste0("Sjukpenningtalet i ", ValdGeografi," ",max(ohalsotal_kommun_ar_df$År))
    diagramfilnamn <- paste0("Sjukpenningtal_kommun", ValdGeografi, ".png")
    ifelse(j==1,objektnamn <- paste0("sjukpenningtal_kommun_", ValdGeografi),objektnamn <-c(objektnamn,paste0("sjukpenningtal_kommun_", ValdGeografi)))
    j=j+1
    # Skapar diagram för i Dalarna där män jämförs med kvinnor.
    gg_obj <- SkapaStapelDiagram(skickad_df = sjukpenningtal_kommun_ar_df%>% 
                                   mutate(Kön=tolower(Kön)), 
                                 skickad_x_var = "Kommun", 
                                 skickad_y_var = "Sjukpenningtal", 
                                 skickad_x_grupp = "Kön",
                                 manual_x_axis_text_vjust = 1, 
                                 manual_x_axis_text_hjust = 1,
                                 x_axis_lutning = 45,,
                                 manual_color = diagramfarger("kon"),
                                 x_axis_sort_value = TRUE,
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt_sjukpenning,
                                 diagram_facet = FALSE,
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1  
  }
  
  if(antal_per_forsakrad_stapel==TRUE){
    
    diagramtitel <- paste0("Antal sjukskrivna per 1000 försäkrade i ", ValdGeografi," ",max(ohalsotal_kommun_ar_df$År))
    diagramfilnamn <- paste0("Antal_per_tusen_kommun", ValdGeografi, ".png")
    ifelse(j==1,objektnamn <- paste0("Antal_per_tusen_kommun_", ValdGeografi),objektnamn <-c(objektnamn,paste0("Antal_per_tusen_kommun_", ValdGeografi)))
    j=j+1
    
    gg_obj <- SkapaStapelDiagram(skickad_df = antal_per_forsakrad_kommun_ar_df%>% 
                                   mutate(Kön=tolower(Kön)), 
                                 skickad_x_var = "Kommun", 
                                 skickad_y_var = "Antal.pågående.sjukfall.per.1000", 
                                 skickad_x_grupp = "Kön",
                                 manual_x_axis_text_vjust = 1, 
                                 manual_x_axis_text_hjust = 1,
                                 x_axis_lutning = 45,
                                 manual_color = diagramfarger("kon"),
                                 manual_y_axis_title="Pågående sjukfall per 1000 försäkrade",
                                 x_axis_sort_value = TRUE,
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
  
  if(stress_stapel==TRUE){
    
    diagramtitel <- paste0("Antal pågående sjukfall kopplade till stress i ", ValdGeografi," ",max(ohalsotal_kommun_ar_df$År))
    diagramfilnamn <- paste0("Antal_stress_kommun", ValdGeografi, ".png")
    ifelse(j==1,objektnamn <- paste0("Antal_stress_kommun_", ValdGeografi),objektnamn <-c(objektnamn,paste0("Antal_stress_kommun_", ValdGeografi)))
    j=j+1
    # Skapar diagram för i Dalarna där män jämförs med kvinnor.
    gg_obj <- SkapaStapelDiagram(skickad_df = antal_stress_kommun_ar_df%>% 
                                   mutate(Kön=tolower(Kön)), 
                                 skickad_x_var = "Kommun", 
                                 skickad_y_var ="Antal.pågående.sjukfall", 
                                 skickad_x_grupp = "Kön",
                                 manual_x_axis_text_vjust = 1, 
                                 manual_x_axis_text_hjust = 1,
                                 x_axis_lutning = 45,
                                 manual_y_axis_title="Pågående sjukfall",
                                 manual_color = diagramfarger("kon"),
                                 x_axis_sort_value = TRUE,
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
  names(gg_list) <- c(objektnamn)
  return(gg_list)
}

