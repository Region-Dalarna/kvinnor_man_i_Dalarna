pacman::p_load(tidyverse)

source("G:/skript/hamta_data/func_gymnasieantagningen.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)

#test_list=diag_gymnasieantagning(skapa_fil=FALSE,antal_program=TRUE)
diag_gymnasieantagning <- function(region_vekt="20",
                                   skapa_fil=TRUE,
                                   output_mapp="G:/skript/jon/Slask/",
                                   valda_program=c("Bygg- och anläggningsprogrammet"),
                                   andel_kon=TRUE,
                                   antal_program=FALSE){
  
  # ========================================== Inställningar ============================================
  # Text till diagram
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."
  valda_farger <-c(rgb(169,208,142, maxColorValue = 255),
                   rgb(112,173,71, maxColorValue = 255))
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  i=1 # Räknare som används för att lägga till objekt i listan
  objektnamn<-c()

  # Läser in data 
  gymnant_df <- las_in_data_gymnasieantagningen()

# beräkna kön per gymnasieprogram för senaste år
  kon_pgr <- gymnant_df %>%
    filter(!is.na(program),
           ar == max(ar)) %>%
    group_by(ar, program) %>%
    summarise(antal = sum(Ant_Tot, na.rm = TRUE),
              killar_antal = sum(Ant_Män, na.rm = TRUE),
              tjejer_antal = sum(Ant_Kv, na.rm = TRUE)) %>%
    mutate(tjejer = round((tjejer_antal / antal)*100,1),
           killar = round((killar_antal / antal)*100,1))

# Pivoterar data
  kon_chart <- kon_pgr %>%
    pivot_longer(c(tjejer, killar), names_to = "kon", values_to = "andel")
  
  if(andel_kon==TRUE){
    diagramtitel <- paste0("Andel antagna per kön och gymnasieprogram i Dalarna år ", max(kon_chart$ar))
    diagramfilnamn <- "kon_andel_programtyp.png"
    objektnamn <- c(objektnamn,"kon_andel_programtyp")
  
  # skapa diagram på andel i gymnasieprogram
    gg_obj<- SkapaStapelDiagram(skickad_df = kon_chart,
                               skickad_x_var = "program",
                               skickad_y_var = "andel",
                               skickad_x_grupp = "kon",
                               manual_color = rev(diagramfarger("kon")),
                               diagram_liggande = TRUE,
                               geom_position_stack = TRUE,
                               x_axis_lutning = 0,
                               manual_y_axis_title = "procent",
                               x_axis_sort_value = TRUE,
                               x_axis_sort_grp = 1,
                               fokusera_varden = list(list(geom = "rect", ymin=40, ymax=60, xmin=0, xmax=Inf, alpha=0.2, fill="grey20"),
                                                      list(geom = "text", x = 0.3, y = 50, size = 2, fontface = "bold", angle = 0,label = "Gymnasieprogram med jämn könsbalans", color ="grey60")),
                               diagram_titel = diagramtitel,
                               diagram_capt = "Källa: Gymnasieantagningen, Dalarnas kommunförbund\nBearbetning: Samhällsanalys, Region Dalarna",
                               filnamn_diagram = diagramfilnamn,
                               output_mapp = output_mapp,
                               legend_vand_ordning = TRUE,
                               skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  if(antal_program==TRUE){
    diagramtitel <- paste0("Antal antagna på gymnasieprogram i Dalarna")
    diagramfilnamn <- "antagna_programtyp.png"
    objektnamn <- c(objektnamn,"antagna_programtyp")
  
    # skapa diagram på andel i gymnasieprogram
    gg_obj<- SkapaStapelDiagram(skickad_df = gymnant_df %>% 
                                  filter(program%in%valda_program),
                                skickad_x_var = "ar",
                                skickad_y_var = "Ant_Tot",
                                skickad_x_grupp = "program",
                                manual_color = diagramfarger("gron_sex")[4:6],
                                diagram_liggande = FALSE,
                                geom_position_stack = TRUE,
                                x_axis_lutning = 0,
                                manual_y_axis_title = "Antagna",
                                diagram_titel = diagramtitel,
                                diagram_capt = "Källa: Gymnasieantagningen, Dalarnas kommunförbund\nBearbetning: Samhällsanalys, Region Dalarna",
                                filnamn_diagram = diagramfilnamn,
                                output_mapp = output_mapp,
                                legend_vand_ordning = FALSE,
                                skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
  i=i+1
  }
  
  names(gg_list)<-c(objektnamn)
  return(gg_list)
}
