diag_kronofogden <-function(region_vekt = "20", # Välj ett län eller en kommun
                            output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                            diag_langsiktiga_skulder = TRUE, # Om diagram för långsiktigt skuldsatta ska skapas
                            diag_skulder = TRUE, # Om diagram för skuldsatta ska skapas,
                            diag_andel_skuldsatta = TRUE,
                            spara_figur = FALSE,
                            returnera_data = FALSE ){
  
  # ======================================================================================================
  # Tre diagram för skuldsatta hos Kronofogden, det ena för långsiktigt skuldsatta (över 20 år), 
  # det andra för kortsiktigt
  # Källa: Kronofogden.
  # Data hämtades senast 2026-07-06. För info om hur hämtning går till, se under respektive if-sats nedan.
  # Förbättringspotential: När det går att komma åt Kronofogdens API direkt från R-Studio, kan latest_file
  # i read.csv2-funktionen enkelt ersättas med adressen till Kronofogdens API.
  # Har uppdaterat skript med PXweb2r för hämtning av data
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
      mutate(Kön = case_when(
        Kön == "Kvinna" ~ "kvinnor",
        Kön == "Man" ~ "män")) %>%
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
    
    #url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101A/BefolkningNy"
    if (!requireNamespace("pxweb2r", quietly = TRUE)) remotes::install_github("FaluPeppe/pxweb2r")
    
    
    befolkning_df <- pxweb2r::pxweb2_get_data(
      table = "TAB5557",
      query = list(
        Region       = region_vekt,
        Civilstand   = "totalt, samtliga civilstånd",
        Alder        = "*",
        Kon          = c("1", "2"),
        ContentsCode = "Folkmängd",
        Tid          = "*"
      ), quiet = TRUE) %>%
      filter(!str_detect(ålder, regex("^tot", ignore_case = TRUE))) %>%
      filter(str_detect(ålder, "^\\d+\\+? år$")) %>%     # bara enskilda åldersår: "0 år", "17 år", "100+ år"
      mutate(ålder = as.integer(readr::parse_number(ålder))) %>%
      mutate(Ålder = case_when(
        between(ålder, 0, 17)  ~ "0-17 år",
        between(ålder, 18, 25) ~ "18-25 år",
        between(ålder, 26, 34) ~ "26-34 år",
        between(ålder, 35, 44) ~ "35-44 år",
        between(ålder, 45, 54) ~ "45-54 år",
        between(ålder, 55, 64) ~ "55-64 år",
        ålder > 64             ~ "65+ år")) %>%
      group_by(kön, år, Ålder) %>%
      summarize(antal_personer = sum(value), .groups = "drop") %>%
      rename("År" = år)
    
    max_år_SCB <- max(befolkning_df$År)
    
    # Vill enbart fokusera på Dalarna och struntar initialt i kommunnivå
    skuldsatta_df <- skulder_df %>%
      filter(Län == valt_lan,
             År ==  max_år_SCB) %>%
      mutate(Kön = case_when(
        Kön == "Kvinna" ~ "kvinnor",
        Kön == "Man" ~ "män")) %>%
      rename("kön" = Kön) %>% 
      group_by(År,Ålder,kön) %>%
      summarize(Antal_skuldsatta=sum(Antal.skuldsatta),
                Total_skuld=sum(Skuldbelopp),
                Skuld_per_person=Total_skuld/Antal_skuldsatta) %>% 
      ungroup()

    skulder_andel_df <- skuldsatta_df %>%
      left_join(befolkning_df,by = c("År","Ålder","kön")) %>% 
      mutate(skulder_andel_proc = (Antal_skuldsatta/antal_personer)*100)
    
    if(returnera_data == TRUE){
      assign("skulder_andel_df", skulder_andel_df, envir = .GlobalEnv)
    }
    
    diagram_capt <- "Källa: Kronofogden och SCB.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Andel av befolkningen i en viss åldersgrupp med skulder under indrivning hos Kronofogden."
    
    diagramtitel <- paste0("Andel med skulder till Kronofogden i ",skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt)$region)," år ",unique(skulder_andel_df$År))
    diagramfilnamn <- paste0("Andel_skuldsatta_",skapa_kortnamn_lan(hamtaregion_kod_namn(region_vekt)$region),".png")
    
    # Skapar diagram för medianinkomsten i Dalarna där män jämförs med kvinnor.
    gg_obj <- SkapaStapelDiagram(skickad_df = skulder_andel_df,
                                 skickad_x_var = "Ålder",
                                 skickad_y_var = "skulder_andel_proc",
                                 skickad_x_grupp = "kön",
                                 x_axis_lutning = 0,
                                 manual_color = diagramfarger("kon"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 stodlinjer_avrunda_fem = TRUE,
                                 manual_y_axis_title = "procent",
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = spara_figur)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    
  }
  
  return(gg_list)
}
