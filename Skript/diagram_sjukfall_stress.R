diag_sjukfall_stress <- function(region_vekt = "20", # Enbart ett län åt gången, inte Sverige
                                 output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                 spara_diagrambildfil = FALSE,
                                 spara_dataframe_till_global_environment = FALSE){
  
  ## =================================================================================================================
  # Skript som ett diagram för sjukfall kopplade till stress
  # Används i första hand i rapporten "Kvinnor och män i Dalarna"
  # Skapad av Jon Frank 2025-07-07
  # =============================================== Uttag ===============================================
  
  # # Läser in nödvändiga bibliotek med pacman
  if (!require("pacman")) install.packages("pacman")
  p_load(openxlsx,
         here)
  
  # Funktioner som behövs
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  # Adresser till data
  path = c("https://www.forsakringskassan.se/api/sprstatistikrapportera/public/v1/sjp-pagaende-sjukfall-diagnos-f43/SJPPagSjukfallDiagnosF43.xlsx")
  
  # Med Peters nya skript
  gg_list = list()
    
    stress_df <- hamta_excel_dataset_med_url(path,skippa_rader = 2) %>% 
      filter(substr(Län,1,2) %in% region_vekt) %>%
        rename(Antal = `Antal pågående sjukfall`,
               Andel_procent = `Andel pågående sjukfall (%)`) %>%  
          select(-kolumnnamn) %>% 
            mutate(Län = str_replace(Län, "^[^\\p{L}]*", ""),
                   Kommun = str_replace(Kommun, "^[^\\p{L}]*", ""),
                   Antal = as.numeric(Antal)) %>% 
              mutate(Kommun = ifelse(Kommun ==  hamtaregion_kod_namn(region_vekt) %>% .$region, "Samtliga kommuner",  Kommun)) %>% 
                filter(Kommun=="Samtliga kommuner")
    
    # För att kunna skriva ut i caption hur många månader som det senaste året består av
    senaste_manad <- stress_df %>% mutate(månad_namn = format(as.Date(paste0(År,"-", Månad, "-01")), "%B")) %>% .$månad_namn %>% unique() %>% first()
    
    # Bearbetar data
    stress_df <- stress_df %>%
      group_by(År,Län,Kön) %>% 
        summarize(Antal_medel=mean(Antal))
    
    # Omvandla kolumnnamn
    
    if(spara_dataframe_till_global_environment) {
      assign("stress_df", stress_df, envir = .GlobalEnv)
    }
    
    # Ohälsotal tidsserie
    diagram_capt<-glue("Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Data för {max(stress_df$År)} till och med {senaste_manad}.")
    diagramtitel <- paste0("Genomsnittligt antal pågående sjukfall kopplade till stress i " ,skapa_kortnamn_lan(unique(stress_df$Län)))
    diagramfilnamn <- paste0("Antal_stress_",skapa_kortnamn_lan(unique(stress_df$Län)),".png")

    gg_obj <- SkapaStapelDiagram(skickad_df = stress_df %>% 
                                   filter(Kön != "Kvinnor och män") %>% 
                                   mutate(Kön = tolower(Kön)), 
                                 skickad_x_var = "År", 
                                 skickad_y_var ="Antal_medel", 
                                 skickad_x_grupp = "Kön",
                                 x_axis_lutning = 45,
                                 manual_x_axis_text_hjust = 1,
                                 manual_x_axis_text_vjust = 1,
                                 manual_y_axis_title = "",
                                 manual_color = diagramfarger("kon"),
                                 stodlinjer_avrunda_fem = TRUE,
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = spara_diagrambildfil)
    
    gg_list <- c(gg_list, list(gg_obj))
    names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  
  return(gg_list)
  
}
