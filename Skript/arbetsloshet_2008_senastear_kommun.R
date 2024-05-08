# Arbetslöshet från 2008 och framåt
# https://arbetsformedlingen.se/statistik/sok-statistik/tidigare-statistik
# Välj Inskrivna arbetslösa, som andel av registerbaserad arbetskraft 2008 - maj 2023
# Data uppdaterades senast 20240503
# Hämtade data för smedjebacken
pacman::p_load(openxlsx,
               here,
               tidyverse)

# Funktioner som behövs (hämtas från Git-Hub)
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

#test_list=diag_antal_varslade(skapa_fil=FALSE)
diag_arbetsloshet_2008_senastear_kommun <- function(region_vekt = "20",
                                             output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                             filnamn = "arbetslöshet_08_senastear_bearbetad.xlsx",
                                             spara_data = TRUE){
  
  # ========================================== Läser in data ============================================
  # Läser in data från Excel (ursprung arbetsförmedlingen). Skall användas i flera projekt varför hela sökvägen läses in (blir fel annars)
  arbetslosa_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/Arbetslöshet_2008_senaste_ar_Smedjebacken_240503.xlsx",sheet = "Andel",startRow = 9)
  
  # Lägger till en variabel region
  arbetslosa_df$Region <- "Smedjebacken"
  
  # Bearbetar data - Tar bara ut för inrikes och utrikes födda
  arbetslosa_utskrift_df <- arbetslosa_df %>% 
    separate(PERIOD,c("Ar","Manad"),"-") %>%
      select(c(Ar,Manad,Region,Inrikes.kvinnor,Inrikes.män,Utrikes.kvinnor,Utrikes.män)) %>% 
        pivot_longer(4:7,names_to = "Grupp",values_to = "Arbetslöshet") %>%
          separate(Grupp,c("Grupp","Kon"),sep = "\\.") %>% 
            group_by(Ar,Region,Grupp,Kon) %>% 
              summarize(Arbetslöshet = mean(Arbetslöshet)*100) %>% 
                mutate(Grupp = ifelse(Grupp == "Inrikes","Inrikes födda","Utrikes födda"))
    
  # Sparar data till Excel
  if (spara_data==TRUE){
    flik_lista=lst("Arbetslöshet_bearbetad"= arbetslosa_utskrift_df)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  }
  
}
