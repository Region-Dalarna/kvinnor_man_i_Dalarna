# Bearbetar data från nedladdat Exceldokument (se flik 1 i Antal startade sjukfall bransch.xlsx för mer info) och spar ett nytt Exceldokument
# Källa: https://www.forsakringskassan.se/statistik-och-analys/sjuk/statistik-inom-omradet-sjuk---sjukpenning-och-rehabiliteringspenning
# Välj sjukfrånvaro per bransch och sektor, 2010-
# Läser in nödvändiga bibliotek med pacman
# Senast kontrollerad: 2024-11-13 (data). Finns fram till 2023.
# Kontrollerad igen 2025-01-16 - ingen ny data
# Enligt Försäkringskassan uppdateras data i juni varje år. https://www.forsakringskassan.se/statistik-och-analys/publiceringsplan-statistik-och-analys
if (!require("pacman")) install.packages("pacman")
p_load(here,
       openxlsx,
       tidyverse)

# source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
# test <- hamta_fk_json_dataset_med_url("https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM0209/AM0209A/KSjuSNI2007")
#test_list <- diag_sjukpenning_bransch(skapa_fil=FALSE)
diag_sjukfall_bransch <- function(output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                     filnamn = "startade_sjukfall_bransch_bearbetad", # Utan filändelse. Datum och filändelse läggs till i skriptet.
                                     spara_data = TRUE){
  
  # ========================================== Inställningar ============================================
  # Text till diagram
  diagram_capt <- "Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna."

  #==========================================================================================================
  
  # Läser in data från Excel
  # Antal startade sjukfall per 1000 förvärvsarbetande
  antal_sjukfall_bransch_df <-read.xlsx("G:/skript/projekt/data/kvinnor_man/Antal startade sjukfall bransch_2025_07_03.xlsx",sheet = 2)

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
  
  filnamn <- paste(filnamn,format(Sys.Date(),"_%Y_%b_%d"),".xlsx",sep="")

  # Sparar data till Excel
  if (spara_data==TRUE){
    flik_lista=lst("Bransch" = antal_sjukfall_bransch_df)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  }
 
}
