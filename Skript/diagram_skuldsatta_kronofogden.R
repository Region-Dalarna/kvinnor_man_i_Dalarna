

diag_kronofogden <-function(region_vekt = "20", # Välj ett län eller en kommun
                            output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                            diag_langsiktiga_skulder = TRUE, # Om diagram för långsiktigt skuldsatta ska skapas
                            diag_skulder = TRUE, # Om diagram för skuldsatta ska skapas
                            spara_figur = TRUE,
                            returnera_data = FALSE ){
  
  # ======================================================================================================
  # Två diagram för skuldsatta hos Kronofogden, det ena för långsiktigt skuldsatta (över 20 år), 
  # det andra för kortsiktigt
  # Källa: Kronofogden.
  # Data hämtades senast 2025-07-15. För info om hur hämtning går till, se under respektive if-sats nedan.
  # ======================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         openxlsx,
         readxl)
  
  # Laddar in de funktioner som används för att skapa diagram
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  diagram_capt <- c("Källa: Kronofogden.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Antal personer med skulder under indrivning hos Kronofogden",
                    "Källa: Kronofogden.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Skuldsatta personer som funnits i Kronofogdens register i mer än 20 år")
  
  gg_list <- list()
  
  valt_lan <- toupper(skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt)$region))
  
  
  if(diag_langsiktiga_skulder){
  
    # Skuldsatta mer än 20 år - https://kronofogden.entryscape.net/catalog/2/datasets/196
    # För att ladda ned data, klistra in länken ovan i webbläsaren så hämtas data automatiskt. Funkar för tillfället inte att hämta direkt via R
    folder_path <- "G:/skript/projekt/data/kvinnor_man/Kronofogden/"
    files <- list.files(
      path = folder_path,
      pattern = "evighetsgaldenarer",
      full.names = TRUE,
      ignore.case = TRUE
    )
    
    file_info <- file.info(files)
    latest_file <- rownames(file_info)[which.max(file_info$mtime)]
    cat("Senast uppdaterade fil är:", latest_file, "\n")
    
    # Uppdaterad i slutat av februari 2024
    langsiktiga_skulder_df <-read.csv2(latest_file,encoding="latin1") %>% 
      mutate(Kön = case_when(
        Kön == "Kvinna" ~ "kvinnor",
        Kön == "Man" ~ "män"),
        Antal.personer = strtoi(Antal.personer, base=0L),
        År = as.character(År))
    
    # Vill enbart fokusera på Dalarna och struntar initialt i kommunnivå
    langsiktiga_skulder_df_sum <- langsiktiga_skulder_df %>%
      filter(Län == valt_lan) %>% 
       group_by(År,Län,Kön)  %>% 
        summarize(Antal_skuldsatta=sum(Antal.personer)) %>% 
          ungroup()
    
    if(returnera_data == TRUE){
      assign("langsiktiga_skulder_df_sum", langsiktiga_skulder_df_sum, envir = .GlobalEnv)
    }
    
    diagramtitel <- paste0("Antal med skulder under minst 20 år till Kronofogden i ",skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt)$region))
    diagramfilnamn <- paste0("Antal_langsiktigt_skuldsatta_",skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt)$region),".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = langsiktiga_skulder_df_sum,
                                          skickad_x_var = "År",
                                          skickad_y_var = "Antal_skuldsatta",
                                          skickad_x_grupp = "Kön",
                                          x_axis_lutning = 0,
                                          manual_color = diagramfarger("kon"),
                                          diagram_titel = diagramtitel,
                                          diagram_capt =  diagram_capt[2],
                                          stodlinjer_avrunda_fem = TRUE,
                                          manual_y_axis_title = "Antal långsiktigt skuldsatta",
                                          output_mapp = output_mapp,
                                          filnamn_diagram = diagramfilnamn,
                                          skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  
  }
  
 
  
  if(diag_skulder){
   
    # Antal personer med skulder under indrivning hos Kronofogden https://kronofogden.entryscape.net/catalog/2/datasets/3
    # För att ladda ned data, klistra in länken ovan i webbläsaren så hämtas data automatiskt. Funkar för tillfället inte att hämta direkt via R
  
    folder_path <- "G:/skript/projekt/data/kvinnor_man/Kronofogden/"
    files <- list.files(
      path = folder_path,
      pattern = "skuldsatta",
      full.names = TRUE,
      ignore.case = TRUE
    )
    
    file_info <- file.info(files)
    latest_file <- rownames(file_info)[which.max(file_info$mtime)]
    cat("Senast uppdaterade fil är:", latest_file, "\n")
    
    skulder_df <-read.csv2(latest_file,encoding="latin1") %>% 
      mutate(År = as.character(År))
    
    valt_lan <- toupper(skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt)$region))
    
    # Vill enbart fokusera på Dalarna och struntar initialt i kommunnivå
    skulder_df_sum <- skulder_df %>%
      filter(Län == valt_lan) %>%
        group_by(År,Län,Kön) %>%
          summarize(Antal_skuldsatta=sum(Antal.skuldsatta),
                    Total_skuld=sum(Skuldbelopp),
                    Skuld_per_person=Total_skuld/Antal_skuldsatta) %>% 
            ungroup()
    
    if(returnera_data == TRUE){
      assign("skulder_df_sum", skulder_df_sum, envir = .GlobalEnv)
    }
  
    diagramtitel <- paste0("Antal med skulder till Kronofogden i ",skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt)$region))
    diagramfilnamn <- paste0("Antal_skuldsatta_",skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt)$region),".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = skulder_df_sum,
                                         skickad_x_var = "År",
                                         skickad_y_var = "Antal_skuldsatta",
                                         skickad_x_grupp = "Kön",
                                         x_axis_lutning = 0,
                                         manual_color = diagramfarger("kon"),
                                         diagram_titel = diagramtitel,
                                         diagram_capt =  diagram_capt[1],
                                         stodlinjer_avrunda_fem = TRUE,
                                         manual_y_axis_title = "Antal med skulder hos Kronofogden",
                                         output_mapp = output_mapp,
                                         filnamn_diagram = diagramfilnamn,
                                         skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  
  }
  
  if(diag_andel_skuldsatta){
    
    # Antal personer med skulder under indrivning hos Kronofogden https://kronofogden.entryscape.net/catalog/2/datasets/3
    # För att ladda ned data, klistra in länken ovan i webbläsaren så hämtas data automatiskt. Funkar för tillfället inte att hämta direkt via R
    
    folder_path <- "G:/skript/projekt/data/kvinnor_man/Kronofogden/"
    files <- list.files(
      path = folder_path,
      pattern = "skuldsatta",
      full.names = TRUE,
      ignore.case = TRUE
    )
    
    file_info <- file.info(files)
    latest_file <- rownames(file_info)[which.max(file_info$mtime)]
    cat("Senast uppdaterade fil är:", latest_file, "\n")
    
    skulder_df <-read.csv2(latest_file,encoding="latin1") %>% 
      mutate(År = as.character(År))
    
    valt_lan <- toupper(skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt)$region))
    
    # Vill enbart fokusera på Dalarna och struntar initialt i kommunnivå
    skulder_df_sum <- skulder_df %>%
      filter(Län == valt_lan) %>%
        group_by(År,Län,Kön) %>%
          summarize(Antal_skuldsatta=sum(Antal.skuldsatta),
                    Total_skuld=sum(Skuldbelopp),
                    Skuld_per_person=Total_skuld/Antal_skuldsatta) %>% 
            ungroup()
    
    if(returnera_data == TRUE){
      assign("skulder_df_sum", skulder_df_sum, envir = .GlobalEnv)
    }
    
    diagramtitel <- paste0("Antal med skulder till Kronofogden i ",skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt)$region))
    diagramfilnamn <- paste0("Antal_skuldsatta_",skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt)$region),".png")
    
    gg_obj <- SkapaStapelDiagram(skickad_df = skulder_df_sum,
                                 skickad_x_var = "År",
                                 skickad_y_var = "Antal_skuldsatta",
                                 skickad_x_grupp = "Kön",
                                 x_axis_lutning = 0,
                                 manual_color = diagramfarger("kon"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt[1],
                                 stodlinjer_avrunda_fem = TRUE,
                                 manual_y_axis_title = "Antal med skulder hos Kronofogden",
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    
  }
  
  return(gg_list)
}
