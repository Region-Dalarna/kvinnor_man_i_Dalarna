####################################
### Etableringstid för nyanlända ###
####################################
# Källa: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AA__AA0003__AA0003B/IntGr1LanKonUtb/

# Läser in nödvändiga bibliotek med pacman
if (!require("pacman")) install.packages("pacman")
p_load(pxweb,
       openxlsx)

# Skript som behövs
source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")

#test_list=diag_etablering(skapa_fil=FALSE)

diag_etablering <- function(region_vekt = "20",
                            output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                            spara_data = TRUE,
                            filnamn = "etableringstid.xlsx" ){
  
  # ========================================== Inställningar ============================================
  # Text till diagram
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."
  #skapa_fil=FALSE

  # ========================================== Läser in data ============================================
  pxweb_query_list <- 
    list("Region"=c(region_vekt),
         "Kon"=c("*"),
         "UtbNiv"=c("*"),
         "BakgrVar"=c("INT010","INT020","INT030","INT040"),
         "ContentsCode"=c("0000001X"),
         "Tid"=c("*"))
  
  # Download data 
  etablering <- 
    pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/AA/AA0003/AA0003B/IntGr1LanKonUtb",
              query = pxweb_query_list) %>% 
      as.data.frame(., column.name.type = "text", variable.value.type = "text") %>% 
        mutate("bakgrundsvariabel" =  word(bakgrundsvariabel,2,3))
  
  # Sparar data till Excel
  if (spara_data==TRUE){
    flik_lista=lst("etableringstid"= etablering)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  }

  # # Skapar en faktorvariabel för att få tid sedan etablering i "rätt" ordning i figuren
  # etablering$bakgrundsvariabel <- factor(etablering$bakgrundsvariabel, levels = c("0-1 år","2-3 år",
  #                                                                                 "4-9 år","10- år"))
  # 
  # if(diag_etablering_gen==TRUE){
  #   diagramtitel <- paste0("Andel förvärvsarbetande 20-64 år bland utrikes födda i ",ValdGeografi," ",max(etablering$år)," uppdelat på vistelsetid i Sverige")
  #   diagramtitel <- str_wrap(diagramtitel)
  #   diagramfilnamn <- paste0("etablering_lan.png")
  #   objektnamn <- c(objektnamn,paste0("etablering_lan"))
  #   
  #   # Skapar diagram där etableringstiden jämförs mellan män och kvinnor, oavsett utbildning
  #   gg_obj <- SkapaStapelDiagram(skickad_df =etablering %>%
  #                                  filter(år==max(etablering$år)) %>% 
  #                                  filter(utbildningsnivå=="samtliga utbildningsnivåer"),
  #                                skickad_x_var = "bakgrundsvariabel", 
  #                                skickad_y_var = "Andel förvärvsarbetande (ny definition från och med 2019)", 
  #                                skickad_x_grupp = "kön",
  #                                # manual_x_axis_text_vjust=0.9,
  #                                manual_color = diagramfarger("kon"),
  #                                diagram_titel = diagramtitel,
  #                                diagram_capt =  diagram_capt,
  #                                manual_x_axis_title = "Vistelsetid",
  #                                diagram_facet = FALSE,
  #                                x_axis_sort_value = FALSE,
  #                                x_axis_lutning = 0, 
  #                                diagram_liggande = FALSE,
  #                                geom_position_stack = FALSE,
  #                                manual_y_axis_title="procent",
  #                                berakna_index = FALSE,
  #                                output_mapp = output_mapp,
  #                                filnamn_diagram = diagramfilnamn,
  #                                skriv_till_diagramfil = skapa_fil)
  #   
  #   gg_list[[i]] <-gg_obj
  #   i=i+1
  # }
  # 
  # if(diag_etablering_utb){
  #   
  #   diagramtitel <- paste0("Andel förvärvsarbetande 20-64 år bland utrikes födda i ",ValdGeografi," ",max(etablering$år)," uppdelat på vistelsetid i Sverige")
  #   diagramtitel <- str_wrap(diagramtitel)
  #   diagramfilnamn <- paste0("etablering_lan_utb.png")
  #   objektnamn <- c(objektnamn,paste0("etablering_lan_utb"))
  #   # Skapar diagram för medianinkomsten i Dalarna där män jämförs med kvinnor.
  #   gg_obj <- SkapaStapelDiagram(skickad_df =etablering %>%
  #                                  filter(år==max(etablering$år)) %>% 
  #                                  filter(utbildningsnivå %in% c("utbildningsnivå: förgymnasial utbildning","utbildningsnivå: eftergymnasial utbildning")),
  #                                skickad_x_var = "bakgrundsvariabel", 
  #                                skickad_y_var = "Andel förvärvsarbetande (ny definition från och med 2019)", 
  #                                skickad_x_grupp = "kön",
  #                                # manual_x_axis_text_vjust=0.9,
  #                                manual_x_axis_title = "Vistelsetid",
  #                                manual_color = diagramfarger("kon"),
  #                                diagram_titel = diagramtitel,
  #                                diagram_capt =  diagram_capt,
  #                                diagram_facet = TRUE,
  #                                facet_grp = "utbildningsnivå",
  #                                facet_legend_bottom = TRUE,
  #                                facet_scale = "fixed",
  #                                x_axis_sort_value = FALSE,
  #                                x_axis_lutning = 0, 
  #                                diagram_liggande = FALSE,
  #                                geom_position_stack = FALSE,
  #                                manual_y_axis_title="procent",
  #                                berakna_index = FALSE,
  #                                output_mapp = output_mapp,
  #                                filnamn_diagram = diagramfilnamn,
  #                                skriv_till_diagramfil = skapa_fil)
  #   
  #   gg_list[[i]] <-gg_obj
  #   i=i+1
  # }
  # 
  # names(gg_list)<-c(objektnamn)
  # return(gg_list)
  
}
