diag_sjukfall_bransch <- function(output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                  spara_figur = FALSE,
                                  spara_dataframe_till_global_environment = FALSE){
  
  # ========================================== Inställningar ============================================
  # Startade sjukfall per bransch i Sverige, 1 diagram. Används i "Kvinnor och män i Dalarna"
  #
  # Källa för manuellt hämtad data: https://www.forsakringskassan.se/statistik-och-analys/sjuk/statistik-inom-omradet-sjuk---sjukpenning-och-rehabiliteringspenning
  # Välj sjukfrånvaro per bransch och sektor, 2010-
  # Läser in nödvändiga bibliotek med pacman
  # Senast hämtad data - 3e: juli 2025
  # Enligt Försäkringskassan uppdateras data i juni varje år. https://www.forsakringskassan.se/statistik-och-analys/publiceringsplan-statistik-och-analys
  #==========================================================================================================
  
  if (!require("pacman")) install.packages("pacman")
  p_load(tidyverse,
         readxl)
  
  folder_path <- "G:/skript/projekt/data/kvinnor_man/"
  files <- list.files(
    path = folder_path,
    pattern = "antal.*sjukfall.*bransch",
    full.names = TRUE,
    ignore.case = TRUE
  )
  
  file_info <- file.info(files)
  latest_file <- rownames(file_info)[which.max(file_info$mtime)]
  cat("Senast uppdaterade fil är:", latest_file, "\n")

  # Antal startade sjukfall per 1000 förvärvsarbetande
  antal_sjukfall_bransch_df <-read_xlsx(latest_file,sheet = 2)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8")
  gg_list = list()
  
  # Pivoterar data för att få den på rätt sätt
  antal_sjukfall_bransch_df <- antal_sjukfall_bransch_df %>% 
    pivot_longer(!SNI2007 & !År, names_to = "Kon", values_to = "Antal_startade_sjukfall") %>% 
    filter(Kon!="Samtliga")  
  
  # Ändrar namn på branscher
  antal_sjukfall_bransch_df$SNI2007 <- case_when(antal_sjukfall_bransch_df$SNI2007 == "A - Jordbruk, skogsbruk och fiske" ~ "Jord- och skogsbruk",
                                                 antal_sjukfall_bransch_df$SNI2007 == "B - Utvinning av mineral" ~ "Utvinning",
                                                 antal_sjukfall_bransch_df$SNI2007 == "C - Tillverkning" ~ "Tillverkning",
                                                 antal_sjukfall_bransch_df$SNI2007 == "D - Försörjning av el, gas, värme och kyla" ~ "Elförsörjning m.m.",
                                                 antal_sjukfall_bransch_df$SNI2007 == "F - Byggverksamhet" ~ "Bygg",
                                                 antal_sjukfall_bransch_df$SNI2007 == "G - Handel; reparation av motorfordon och motorcyklar" ~ "Handel",
                                                 antal_sjukfall_bransch_df$SNI2007 == "H - Transport och magasinering" ~ "Transport",
                                                 antal_sjukfall_bransch_df$SNI2007 == "I - Hotell- och restaurangverksamhet" ~ "Hotell- och restaurang",
                                                 antal_sjukfall_bransch_df$SNI2007 == "J - Informations- och kommunikationsverksamhet" ~ "IT och kommunikation",
                                                 antal_sjukfall_bransch_df$SNI2007 == "K - Finans- och försäkringsverksamhet" ~ "Finans- och försäkring",
                                                 antal_sjukfall_bransch_df$SNI2007 == "L - Fastighetsverksamhet" ~ "Fastighet",
                                                 antal_sjukfall_bransch_df$SNI2007 == "M - Verksamhet inom juridik, ekonomi, vetenskap och teknik" ~ "Juridik, ekonomi m.m.",
                                                 antal_sjukfall_bransch_df$SNI2007 == "N - Uthyrning, fastighetsservice, resetjänster och andra stödtjänster" ~ "Diverse stödtjänster",
                                                 antal_sjukfall_bransch_df$SNI2007 == "O - Offentlig förvaltning och försvar; obligatorisk socialförsäkring" ~ "Offentlig förvaltning",
                                                 antal_sjukfall_bransch_df$SNI2007 == "P - Utbildning" ~ "Utbildning",
                                                 antal_sjukfall_bransch_df$SNI2007 == "Q - Vård och omsorg; sociala tjänster" ~ "Vård och omsorg",
                                                 antal_sjukfall_bransch_df$SNI2007 == "R - Kultur, nöje och fritid" ~ "Kultur m.m.",
                                                 antal_sjukfall_bransch_df$SNI2007 == "S - Annan serviceverksamhet" ~ "Annan serviceverksamhet")
  
  if(spara_dataframe_till_global_environment) {
    assign("startade_sjukfall_bransch_df", antal_sjukfall_bransch_df, envir = .GlobalEnv)
  }
  
  
  diagram_capt <- "Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna."
  diagramtitel <- paste0("Antal startade sjukfall per 1000 förvärvsarbetande i Sverige år ",max(antal_sjukfall_bransch_df$År))
  diagramfilnamn <- paste0("Startade_sjukfall_bransch.png")
  
  gg_obj <- SkapaStapelDiagram(skickad_df = antal_sjukfall_bransch_df,
                                             skickad_x_var = "SNI2007",
                                             skickad_y_var = "Antal_startade_sjukfall",
                                             skickad_x_grupp = "Kon",
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
                                             skriv_till_diagramfil = spara_figur)
  
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  
  return(gg_list)
  
}