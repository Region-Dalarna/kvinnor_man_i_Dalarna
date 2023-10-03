library(openxlsx)
library(tidyverse)
library(here)

# Laddar in de funktioner som används för att skapa diagram
source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)

#test_list <- diag_forsakringskassan(skapa_fil=FALSE)
diag_forsakringskassan <- function(region_vekt = "20", 
                                  output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                  skapa_fil = TRUE,
                                  ohalsotal_stapel=TRUE,
                                  sjukpenningtal_stapel=TRUE,
                                  antal_per_forsakrad_stapel=TRUE,
                                  stress_stapel=TRUE){

# ========================================== Inställningar ============================================
# Text till diagram
diagram_capt_ohälsa <- "Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Ohälsotalet: hur många dagar under en tolvmånadersperiod\nFörsäkringskassan betalar ut ersättning för nedsatt arbetsförmåga\ni förhållande till antalet försäkrade i åldrarna 16-64 år."
diagram_capt_sjukpenning<-"Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring:Sjukpenningtalet är antalet dagar med sjukpenning och rehabiliteringspenning\nsom har betalats ut under en 12-månaders period.Den summan delas med antalet försäkrade i\nSverige som är i åldrarna 16–64 år."
diagram_capt<-"Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna."
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
antal_stress_df <- read.xlsx(here("indata","/","Antal_stress.xlsx"),sheet=1) # https://www.forsakringskassan.se/statistik-och-analys/statistikdatabas#!/sjuk/sjp-pagaende-sjukfall-diagnos-f43

# Filtrerar på samtliga län,december varje år och bara kvinnor respektive män
ohalsotal_lan_ar_df <- ohalsotal_df %>%
  filter(Kommun=="Samtliga kommuner") %>%
    filter(Månad==12) %>% 
      filter(Kön!="Kvinnor och män")
    
sjukpenningtal_lan_ar_df <- sjukpenningtal_df %>%
  filter(Kommun=="Samtliga kommuner") %>%
    filter(Månad==12) %>% 
      filter(Kön!="Kvinnor och män") 
      
antal_per_forsakrad_lan_ar_df <- antal_per_forsakrad_df %>%
  filter(Kommun=="Samtliga kommuner") %>%
    filter(Månad==12) %>% 
      filter(Kön!="Kvinnor och män")

antal_stress_lan_ar_df <- antal_stress_df %>%
  filter(Kommun=="Samtliga kommuner") %>%
    filter(Månad==12) %>% 
      filter(Kön!="Kvinnor och män")

# Måste ändra antalet sjukfall (stress) till integer, inte character
antal_stress_lan_ar_df$Antal.pågående.sjukfall <-as.integer(antal_stress_lan_ar_df$Antal.pågående.sjukfall)

if(ohalsotal_stapel==TRUE){
  
  diagramtitel <- paste0("Ohälsotalet i ", ValdGeografi)
  diagramfilnamn <- paste0("ohalsotal", ValdGeografi, ".png")
  ifelse(j==1,objektnamn <- paste0("ohalsotal", ValdGeografi),objektnamn <-c(objektnamn,paste0("ohalsotal", ValdGeografi)))
  j=j+1
  # Skapar diagram för i Dalarna där män jämförs med kvinnor.
  gg_obj <- SkapaStapelDiagram(skickad_df = ohalsotal_lan_ar_df %>% 
                                 mutate(Kön=tolower(Kön)), 
                               skickad_x_var = "År", 
                               skickad_y_var = "Ohälsotalet", 
                               skickad_x_grupp = "Kön",
                               # manual_x_axis_text_vjust=1.2,
                               x_axis_lutning = 0,
                               manual_color = diagramfarger("kon"),
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
  
  diagramtitel <- paste0("Sjukpenningtalet i ", ValdGeografi)
  diagramfilnamn <- paste0("Sjukpenningtal", ValdGeografi, ".png")
  ifelse(j==1,objektnamn <- paste0("sjukpenningtal", ValdGeografi),objektnamn <-c(objektnamn,paste0("sjukpenningtal", ValdGeografi)))
  j=j+1
  # Skapar diagram för i Dalarna där män jämförs med kvinnor.
  gg_obj <- SkapaStapelDiagram(skickad_df = sjukpenningtal_lan_ar_df%>% 
                                 mutate(Kön=tolower(Kön)), 
                               skickad_x_var = "År", 
                               skickad_y_var = "Sjukpenningtal", 
                               skickad_x_grupp = "Kön",
                               x_axis_lutning = 0,
                               manual_color = diagramfarger("kon"),
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
  
  diagramtitel <- paste0("Antal sjukskrivna per 1000 försäkrade i ", ValdGeografi)
  diagramfilnamn <- paste0("Antal_per_tusen", ValdGeografi, ".png")
  ifelse(j==1,objektnamn <- paste0("Antal_per_tusen", ValdGeografi),objektnamn <-c(objektnamn,paste0("Antal_per_tusen", ValdGeografi)))
  j=j+1
  # Skapar diagram för i Dalarna där män jämförs med kvinnor.
  gg_obj <- SkapaStapelDiagram(skickad_df = antal_per_forsakrad_lan_ar_df%>% 
                                 mutate(Kön=tolower(Kön)), 
                               skickad_x_var = "År", 
                               skickad_y_var = "Antal.pågående.sjukfall.per.1000", 
                               skickad_x_grupp = "Kön",
                               x_axis_lutning = 0,
                               manual_color = diagramfarger("kon"),
                               manual_y_axis_title="Antal pågående sjukfall per 1000 försäkrade",
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
  
  diagramtitel <- paste0("Antal pågående sjukfall kopplade till stress i ", ValdGeografi)
  diagramfilnamn <- paste0("Antal_stress", ValdGeografi, ".png")
  ifelse(j==1,objektnamn <- paste0("Antal_stress", ValdGeografi),objektnamn <-c(objektnamn,paste0("Antal_stress", ValdGeografi)))
  j=j+1
  # Skapar diagram för i Dalarna där män jämförs med kvinnor.
  gg_obj <- SkapaStapelDiagram(skickad_df = antal_stress_lan_ar_df%>% 
                                 mutate(Kön=tolower(Kön)), 
                               skickad_x_var = "År", 
                               skickad_y_var ="Antal.pågående.sjukfall", 
                               skickad_x_grupp = "Kön",
                               x_axis_lutning = 0,
                               manual_y_axis_title="Antal pågående sjukfall",
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
names(gg_list) <- c(objektnamn)
return(gg_list)
}

