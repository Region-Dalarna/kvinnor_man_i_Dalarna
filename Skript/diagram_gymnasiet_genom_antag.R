diag_gymn_genomstromning_antagning <- function(outputmapp = "G:/skript/jon/Slask/",
                                               region_vekt = "20",
                                               diag_genomstromning = TRUE,
                                               diag_gymantagning = TRUE,
                                               start_ar_genomstromning = "2015", # Finns från 2015
                                               spara_figur = FALSE,
                                               returnera_data = TRUE){
  
  
  # ========================================== Info ============================================
  # Arbetslöshet från 2008 och framåt. Enbart för Dalarna (1 diagram)
  # https://arbetsformedlingen.se/statistik/sok-statistik/tidigare-statistik
  # Välj Inskrivna arbetslösa, som andel av registerbaserad arbetskraft 2008 - maj 2023
  # Data uppdaterades senast 20240226. Uppdateras tyvärr inte längre. Har inte hittat något alternativ
  # ========================================== Info ============================================
  # Skript som anropar Peters funktion för att ladda hem data för gymnasieantagning och sparar till Excel
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         rKolada)
  
  # Funktioner som behövs (hämtas från Git-Hub)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  gg_list <- list()
  
  # ========================================== Läser in data ============================================
  
  if(diag_genomstromning == TRUE){
    
    # genomstromning_df <- get_values(
    #   kpi = c("N60960"),
    #   municipality = c("0020"),
    #   period = start_ar_genomstromning:2100
    # )
    
    genomstromning_df <- hamta_kolada_df(kpi_id = "N60960",
                                        valda_kommuner = region_vekt,
                                        valda_ar = start_ar_genomstromning:2100)

    if(returnera_data == TRUE){
      assign("genomstromning_df", genomstromning_df, envir = .GlobalEnv)
    }
    
    
    diagram_capt = "Källa: Skolverket (via Kolada)\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Antal folkbokförda elever i regionen som började på gymnasium för fyra år sedan\nsom fått examen eller studiebevis inom fyra år,inkl. IM dividerat med antal folkbokförda elever i länet\nsom började på gymnasium för fyra år sedan, inkl. IM."
    
    diagramtitel <- paste0("Andel gymnasieelever med examen eller studiebevis inom 4 år i ", unique(genomstromning_df$region))
    diagramfilnamn <- paste0("genomstromning_",unique(genomstromning_df$region),".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = genomstromning_df %>% 
                                   filter(kon!="Totalt"),
                                 skickad_x_var = "ar",
                                 skickad_y_var = "varde",
                                 skickad_x_grupp = "kon",
                                 diagram_titel = diagramtitel,
                                 diagram_capt = diagram_capt,
                                 x_axis_lutning = 0,
                                 manual_color = diagramfarger("kon"),
                                 manual_y_axis_title = "procent",
                                 manual_x_axis_title = "",
                                 procent_0_100_10intervaller = TRUE,
                                 stodlinjer_avrunda_fem = TRUE,
                                 output_mapp = outputmapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    
  }
  
  if(diag_gymantagning == TRUE){
    
    source("G:/skript/hamta_data/func_gymnasieantagningen.R", encoding = "utf-8", echo = FALSE)
    
    gymnasieantagning_df <- las_in_data_gymnasieantagningen()
    
    gymnasieantagning_df <- gymnasieantagning_df %>% 
      group_by(ar,program) %>% 
      summarize(Män=sum(Ant_Män),
                Kvinnor=sum(Ant_Kv))
    
    # Pivoterar data för att enkelt kunna dela upp på kvinnor och män
    gymnasieantagning_df<-pivot_longer(gymnasieantagning_df,3:4,names_to="Kon",values_to = "Antagna")
    
    # Ändrar ett av utbildningsnamnen
    gymnasieantagning_df[gymnasieantagning_df == "Flygteknikutbildningen, Marinteknikutbildningen, Sjöfartsutbildningen, Tågteknikutbildningen, Utbildningen samiska näringar eller Yrkesdansarutbildningen"] <- "Flygteknikutbildningen mfl."
    
    gymnasieantagning_df <- gymnasieantagning_df %>% 
      group_by(ar,program) %>% 
      mutate(andel = (Antagna/sum(Antagna))*100-0.001)
    
    if(returnera_data == TRUE){
      assign("gymnasieantagning_df", gymnasieantagning_df, envir = .GlobalEnv)
    }
    
    diagram_capt <- "Källa: Gymnasieantagningen, Dalarnas kommunförbund\nBearbetning:Samhällsanalys, Region Dalarna\nDiagramförklaring: Antagna i början av september"
    diagram_titel <- paste0("Antagna gymnasieelever i Dalarna ", max(gymnasieantagning_df$ar)," per program")
    diagramfilnamn <- "gymnasieantagning_alla.png"
    
    gg_obj <- SkapaStapelDiagram(skickad_df = gymnasieantagning_df %>%
                                                  filter(ar == max(.$ar)),
                                                skickad_x_var = "program",
                                                skickad_y_var = "andel",
                                                skickad_x_grupp = "Kon",
                                                manual_color = diagramfarger("kon"),
                                                diagram_titel = diagram_titel,
                                                x_axis_sort_value = TRUE,
                                                x_axis_sort_grp = 2,
                                                fokusera_varden = list(list(geom = "rect", ymin=40, ymax=60, xmin=0, xmax=Inf, alpha=0.2, fill="grey20")),
                                                diagram_capt = diagram_capt,
                                                #procent_0_100_10intervaller = TRUE,
                                                manual_y_axis_title = "procent",
                                                x_axis_lutning = 0,
                                                diagram_liggande = TRUE,
                                                geom_position_stack = TRUE,
                                                berakna_index = FALSE,
                                                output_mapp = outputmapp,
                                                filnamn_diagram = diagramfilnamn,
                                                skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    
  }
  
  
  return(gg_list)
  
}
