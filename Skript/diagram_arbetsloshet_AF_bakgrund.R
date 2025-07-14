diag_arbetsloshet_2008_senastear <- function(output_mapp = "G:/skript/jon/Slask/",
                                             spara_figur = TRUE,
                                             returnera_data = TRUE){
  
  
  # ========================================== Info ============================================
  # Arbetslöshet från 2008 och framåt. Enbart för Dalarna (1 diagram)
  # https://arbetsformedlingen.se/statistik/sok-statistik/tidigare-statistik
  # Välj Inskrivna arbetslösa, som andel av registerbaserad arbetskraft 2008 - maj 2023
  # Data uppdaterades senast 20240226. Uppdateras tyvärr inte längre. Har inte hittat något alternativ
  # ========================================== Info ============================================
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(openxlsx,
                 here,
                 tidyverse)
  
  # Funktioner som behövs (hämtas från Git-Hub)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  gg_list <- list()
  
  # ========================================== Läser in data ============================================
  # Läser in data från Excel (ursprung arbetsförmedlingen). Skall användas i flera projekt varför hela sökvägen läses in (blir fel annars)
  arbetslosa_Dalarna_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/Arbetslöshet_2008_senaste_ar_240226.xlsx",sheet = "Andel",startRow = 9) %>% 
    mutate(Region = "Dalarna")
  
  # Lägger till en variabel region
  #arbetslosa_Dalarna_df$Region<-"Dalarna"
  
  # Bearbetar data - Tar bara ut för inrikes och utrikes födda
  arbetslosa_utskrift_df <- arbetslosa_Dalarna_df %>% 
    separate(PERIOD,c("Ar","Manad"),"-") %>%
      select(c(Ar,Manad,Region,Inrikes.kvinnor,Inrikes.män,Utrikes.kvinnor,Utrikes.män)) %>% 
        pivot_longer(4:7,names_to = "Grupp",values_to = "Arbetslöshet") %>%
          separate(Grupp,c("Grupp","Kon"),sep = "\\.") %>% 
            group_by(Ar,Region,Grupp,Kon) %>% 
              summarize(Arbetslöshet = mean(Arbetslöshet)*100) %>% 
                mutate(Grupp = ifelse(Grupp == "Inrikes","Inrikes födda","Utrikes födda")) %>% 
                  ungroup()
  
  if(returnera_data == TRUE){
    assign("arbetslosa_af_tidsserie_df", arbetslosa_utskrift_df, envir = .GlobalEnv)
  }
  
  
  diagram_capt <- "Källa: Arbetsförmedlingen.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Månadsdata. Diagrammet visar medelvärdet för året"
  
  diagramtitel <- paste0("Arbetslöshet 16-64 år i ",unique(arbetslosa_utskrift_df$Region))
  diagramfilnamn <- paste0("arbetsloshet_tidsserie_bakgrund_",unique(arbetslosa_utskrift_df$Region),".png")
  
  gg_obj <- SkapaStapelDiagram(skickad_df = arbetslosa_utskrift_df, 
                                            skickad_x_var = "Ar",
                                            skickad_y_var = "Arbetslöshet",
                                            skickad_x_grupp = "Kon",
                                            manual_x_axis_text_vjust=1,
                                            manual_x_axis_text_hjust=1,
                                            manual_color = diagramfarger("kon"),
                                            diagram_titel = diagramtitel,
                                            diagram_capt =  diagram_capt,
                                            diagram_facet = TRUE,
                                            facet_legend_bottom = TRUE,
                                            stodlinjer_avrunda_fem = TRUE,
                                            facet_grp = "Grupp",
                                            facet_scale = "fixed",
                                            x_axis_lutning = 45,
                                            manual_y_axis_title = "procent",
                                            output_mapp = output_mapp,
                                            filnamn_diagram = diagramfilnamn,
                                            skriv_till_diagramfil = spara_figur)
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  
  return(gg_list)
  
}
