

#test_list <- diag_chefer(skapa_fil = FALSE)

diag_chefer<-function(region_vekt = "20", 
                      output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                      filnamn = "chefer.xlsx",
                      spara_data = TRUE){
  
  ## =================================================================================================================
  # Skript som laddar hem data för andel chefer från SCB och skapar ett diagram
  # Källa: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AA__AA0003__AA0003X/IntGr1LanKonUtb/
  # Data kontrollerat 2025-01-07. Uppdaterades senast (av SCB) 2024-12-23. Finns data till 2022 (2023 står som NA).
  # =================================================================================================================
  # Läser in nödvändiga bibliotek med pacman
  
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         openxlsx)
  
  # Funktioner som behövs
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  # Data som inte uppdateras (fram till 2021 - RAMS)
  url_tidigare <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AA/AA0003/AA0003X/IntGr1LanKonUtb"
  
  # Variabler som skall tas ut
  varlista <-  list(Region=c(region_vekt),
                    Kon=c("1","2"),
                    UtbNiv=c("000","F","3","EU","US"),
                    BakgrVar=c("tot20-64"),
                    ContentsCode=c("0000001Y"),
                    Tid=c("*"))
  
  # Uttag av data
  chefer_RAMS_df <- pxweb_get(url = url_tidigare,query = varlista) %>% 
    as.data.frame(column.name.type = "text", variable.value.type = "text") %>% 
    rename(Andel = `Andel i chefsposition, procent`)
  
  # Data som uppdateras (från 2022 - BAS)
  url_ny <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AA/AA0003/AA0003B/IntGr1LanUtbBAS"
  
  # Variabler som skall tas ut
  varlista <-  list(Region=c(region_vekt),
                    Kon=c("1","2"),
                    UtbNiv=c("000","F","3","EU","US"),
                    BakgrVar=c("TOT"),
                    ContentsCode=c("000007KF"),
                    Tid=c("*"))
  
  # Uttag av data
  chefer_bas_df <- pxweb_get(url = url_ny,query = varlista) %>% 
    as.data.frame(column.name.type = "text", variable.value.type = "text") %>% 
    rename(Andel = `Andel i chefsposition`)
  
  chefer_df <- rbind(chefer_RAMS_df,chefer_bas_df) %>% 
    filter(!is.na(Andel))
  
  # Andrar namnet på vissa kategorier
  chefer_df[chefer_df=="utbildningsnivå: förgymnasial utbildning"] <- "förgymnasial utbildning"
  chefer_df[chefer_df=="utbildningsnivå: gymnasial utbildning"] <- "gymnasial utbildning"
  chefer_df[chefer_df=="utbildningsnivå: eftergymnasial utbildning"] <- "eftergymnasial utbildning"
  
  if(diag_senaste_ar){
  
    diagram_capt <- c("Källa: SCB:s öppna statistikdatabas, BAS\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Andel chefer av sysselsatta med ett klassificerat yrke (20-65 år) uppdelat på utbildningsnivå")
    
    diagramtitel <- paste0("Andel chefer år ",max(chefer_df$år)," i ",skapa_kortnamn_lan(unique(chefer_df$region)))
    diagramfilnamn <- paste0("andel_chefer_",skapa_kortnamn_lan(unique(chefer_df$region)),".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = chefer_df %>% 
                                       filter(år == max(år),
                                              utbildningsnivå != "utbildningsnivå: uppgift saknas"),
                                     skickad_x_var = "utbildningsnivå",
                                     skickad_y_var = "Andel",
                                     skickad_x_grupp = "kön",
                                     manual_x_axis_text_vjust=1,
                                     manual_x_axis_text_hjust=1,
                                     manual_color = diagramfarger("kon"),
                                     diagram_titel = diagramtitel,
                                     diagram_capt =  diagram_capt[1],
                                     stodlinjer_avrunda_fem = TRUE,
                                     diagram_facet = FALSE,
                                     x_axis_sort_value = TRUE,
                                     manual_y_axis_title="procent",
                                     output_mapp = outputmapp,
                                     filnamn_diagram = diagramfilnamn,
                                     skriv_till_diagramfil = spara_figur)
  
  }
  
  if(diag_linje){
    diagram_capt <- c("Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Förändring i andelen chefer av sysselsatta med ett klassificerat yrke för samtliga utbildningsnivåer.\nFram till och med 2021, 20-64 år och data från RAMS. Från 2022, 20-65 år och data från BAS.")
    diagramtitel <- paste0("Förändring i andel chefer i ",skapa_kortnamn_lan(unique(chefer_df$region)))
    diagramfilnamn <- paste0("andel_chefer_linje_",skapa_kortnamn_lan(unique(chefer_df$region)),".png")
    
    gg_obj <- SkapaLinjeDiagram(skickad_df =chefer_df %>% 
                                            filter(år >"2000",
                                                   utbildningsnivå == "samtliga utbildningsnivåer"),
                                          skickad_x_var = "år",
                                          skickad_y_var = "Andel",
                                          skickad_x_grupp = "kön",
                                          x_axis_lutning = 45,
                                          manual_color = diagramfarger("kon"),
                                          diagram_titel = diagramtitel,
                                          diagram_capt =  diagram_capt[2],
                                          stodlinjer_avrunda_fem = TRUE,
                                          diagram_facet = FALSE,
                                          berakna_index = TRUE,
                                          output_mapp = outputmapp,
                                          filnamn_diagram = diagramfilnamn,
                                          skriv_till_diagramfil = spara_figur)
  }

}
