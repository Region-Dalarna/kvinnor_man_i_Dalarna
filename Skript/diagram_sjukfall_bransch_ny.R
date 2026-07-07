diag_sjukfall_bransch <- function(region_vekt = "20", # Enbart ett län åt gången, inte Sverige
                                 output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                 spara_diagrambildfil = FALSE,
                                 spara_dataframe_till_global_environment = FALSE){
  
  ## =================================================================================================================
  # Startade sjukfall per bransch i Sverige, 1 diagram. Används i "Kvinnor och män i Dalarna"
  # 
  # Skapad av Jon Frank 2026-07-06
  # =============================================== Uttag ===============================================
  
  # # Läser in nödvändiga bibliotek med pacman
  if (!require("pacman")) install.packages("pacman")
  p_load(openxlsx,
         here)
  
  # Funktioner som behövs
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  
  # Adresser till data
  path = c("https://www.forsakringskassan.se/api/sprstatistikrapportera/public/v1/sjp-bransch-sektor/SJPSEKTORBRANSCHBRA1LAN.xlsx")
  
  # Med Peters nya skript
  gg_list = list()
  
  antal_sjukfall_bransch_df <- hamta_excel_dataset_med_url(path,skippa_rader = 2) %>% 
    filter(Län == "Riket",
           År == max(År),
           `Bransch nivå 1` != "Samtliga branscher",
           Kön != "Kvinnor och män") %>%
      rename(Bransch = `Bransch nivå 1`,
             Antal_startade_sjukfall = `Antal startade sjukfall per 1000 förvärvsarbetande`) %>%  
        select(År,Kön,Bransch,Län,Antal_startade_sjukfall) %>% 
          mutate(Län = skapa_kortnamn_lan(Län,byt_ut_riket_mot_sverige = TRUE),
                 Antal_startade_sjukfall = as.numeric(Antal_startade_sjukfall))

  # Ändrar namn på branscher
  antal_sjukfall_bransch_df$Bransch <- case_when(antal_sjukfall_bransch_df$Bransch == "A-Jordbruk, skogsbruk och fiske" ~ "Jord- och skogsbruk",
                                                 antal_sjukfall_bransch_df$Bransch == "B-Utvinning av mineral" ~ "Utvinning",
                                                 antal_sjukfall_bransch_df$Bransch == "C-Tillverkning" ~ "Tillverkning",
                                                 antal_sjukfall_bransch_df$Bransch == "D-Försörjning av el, gas, värme och kyla" ~ "Elförsörjning m.m.",
                                                 antal_sjukfall_bransch_df$Bransch == "E-Vattenförsörjning; avloppsrening, avfallshantering och sanering" ~ "Vattenförsörjning, avfallshantering m.m.",
                                                 antal_sjukfall_bransch_df$Bransch == "F-Byggverksamhet" ~ "Bygg",
                                                 antal_sjukfall_bransch_df$Bransch == "G-Handel; reparation av motorfordon och motorcyklar" ~ "Handel",
                                                 antal_sjukfall_bransch_df$Bransch == "H-Transport och magasinering" ~ "Transport",
                                                 antal_sjukfall_bransch_df$Bransch == "I-Hotell- och restaurangverksamhet" ~ "Hotell- och restaurang",
                                                 antal_sjukfall_bransch_df$Bransch == "J-Informations- och kommunikationsverksamhet" ~ "IT och kommunikation",
                                                 antal_sjukfall_bransch_df$Bransch == "K-Finans- och försäkringsverksamhet" ~ "Finans- och försäkring",
                                                 antal_sjukfall_bransch_df$Bransch == "L-Fastighetsverksamhet" ~ "Fastighet",
                                                 antal_sjukfall_bransch_df$Bransch == "M-Verksamhet inom juridik, ekonomi, vetenskap och teknik" ~ "Juridik, ekonomi m.m.",
                                                 antal_sjukfall_bransch_df$Bransch == "N-Uthyrning, fastighetsservice, resetjänster och andra stödtjänster" ~ "Diverse stödtjänster",
                                                 antal_sjukfall_bransch_df$Bransch == "O-Offentlig förvaltning och försvar; obligatorisk socialförsäkring" ~ "Offentlig förvaltning",
                                                 antal_sjukfall_bransch_df$Bransch == "P-Utbildning" ~ "Utbildning",
                                                 antal_sjukfall_bransch_df$Bransch == "Q-Vård och omsorg; sociala tjänster" ~ "Vård och omsorg",
                                                 antal_sjukfall_bransch_df$Bransch == "R-Kultur, nöje och fritid" ~ "Kultur m.m.",
                                                 antal_sjukfall_bransch_df$Bransch == "S-Annan serviceverksamhet" ~ "Annan serviceverksamhet")

  if(spara_dataframe_till_global_environment) {
    assign("startade_sjukfall_bransch_df", antal_sjukfall_bransch_df, envir = .GlobalEnv)
  }
  
  
  diagram_capt <- "Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna."
  diagramtitel <- paste0("Antal startade sjukfall per 1000 förvärvsarbetande i Sverige år ",max(antal_sjukfall_bransch_df$År))
  diagramfilnamn <- paste0("Startade_sjukfall_bransch.png")
  
  gg_obj <- SkapaStapelDiagram(skickad_df = antal_sjukfall_bransch_df %>% 
                                 mutate(Bransch = str_wrap(Bransch, width = 30)),
                               skickad_x_var = "Bransch",
                               skickad_y_var = "Antal_startade_sjukfall",
                               skickad_x_grupp = "Kön",
                               manual_color = diagramfarger("kon"),
                               diagram_titel = diagramtitel,
                               diagram_capt =  diagram_capt,
                               manual_x_axis_text_vjust = 1,
                               manual_x_axis_text_hjust = 1,
                               manual_y_axis_title = "",
                               x_axis_sort_value = TRUE,
                               x_axis_lutning = 45,
                               stodlinjer_avrunda_fem = TRUE,
                               output_mapp = output_mapp,
                               filnamn_diagram = diagramfilnamn,
                               skriv_till_diagramfil = spara_diagrambildfil)
  
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  
  return(gg_list)
  
}
