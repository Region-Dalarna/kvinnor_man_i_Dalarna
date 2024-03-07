library(openxlsx)
library(tidyverse)

# Laddar in de funktioner som används för att skapa diagram
source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)

#test_list <- diag_ek_bistand(skapa_fil=FALSE)

diag_ek_bistand <-function(region_vekt = "20", 
                                 output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                 skapa_fil = TRUE){
  
  # ========================================== Inställningar ============================================
  # Text till diagram
  diagram_capt <- "Källa: Socialstyrelsen. Bearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Utbetalt ekonomiskt bistånd till ensamhushåll (exklusive intoduktionsersättning)"
  
  # valda_farger <-c(rgb(169,208,142, maxColorValue = 255),
  #                  rgb(112,173,71, maxColorValue = 255))
  # region_vekt = "20" 
  # output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/"
  # skapa_fil = FALSE
  
  ValdGeografi <- hamtaregion_kod_namn(region_vekt)$region
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  i=1 # Räknare som används för att lägga till objekt i listan
  #==========================================================================================================
  # Läser in data från Excel
  ek_bistand_df <-read.xlsx(here("indata","/","ek_bistand.xlsx"),sheet=1)
  
  # Manipulerar dataset
  # Ändrar till ett enklare namn för utrikes födda
  ek_bistand_df[ek_bistand_df=="En av registerledare/sambo utrikesfödd"]<- "Utrikes födda"
  
  # Ändrar namnet på två av variablerna och filtrerar först på 2000-talet sedan på min mitten och slutår
  ek_bistand_underlag_df <- ek_bistand_df %>% 
    rename("Bakgrund"='Inrikes-.eller.utrikesfödda.hushåll') %>% 
      rename("Utbetalt bistånd, tkr" ='Dalarnas.län') %>% 
        filter(as.integer(ek_bistand_df$År)>1999)
  
  # Beräknar år som ligger i mitten av sample
  medel_ar=2000+round((max(as.integer( ek_bistand_underlag_df$År))-min(as.integer( ek_bistand_underlag_df$År)))/2)
  
  # Väljer ut det första, mittersta och sista året att i datasettet (används i figuren)
  ek_bistand_underlag_df <- ek_bistand_underlag_df %>% 
    filter(År==min(År)|År==as.character(medel_ar)|År==max(År))
          

  diagramtitel <- paste0("Ekonomiskt bistånd i ", ValdGeografi)
  diagramfilnamn <- paste0("ekonomiskt_bistand", ValdGeografi, ".png")
  objektnamn <- paste0("ekonomiskt_bistand", ValdGeografi)

  # Skapar diagram för medianinkomsten i Dalarna där män jämförs med kvinnor.
  gg_obj <- SkapaStapelDiagram(skickad_df = ek_bistand_underlag_df, 
                             skickad_x_var = "Hushållstyp", 
                             skickad_y_var = "Utbetalt bistånd, tkr", 
                             skickad_x_grupp = "År",
                             manual_x_axis_text_vjust=0.5,
                             manual_color = diagramfarger("gron_fyra")[1:3],
                             diagram_titel = diagramtitel,
                             diagram_capt =  diagram_capt,
                             diagram_facet = TRUE,
                             facet_scale = "fixed",
                             facet_grp = "Bakgrund",
                             facet_legend_bottom = TRUE,
                             berakna_index = FALSE,
                             output_mapp = output_mapp,
                             filnamn_diagram = diagramfilnamn,
                             skriv_till_diagramfil = skapa_fil)

  gg_list[[i]] <-gg_obj
  i=i+1
  names(gg_list) <- c(objektnamn)
  return(gg_list)
}
