# Bearbetar data från nedladdat Exceldokument (se flik 1 i Antal startade sjukfall bransch.xlsx för mer info) och spar ett nytt Exceldokument
# Källa: https://www.forsakringskassan.se/statistik-och-analys/sjuk/statistik-inom-omradet-sjuk---sjukpenning-och-rehabiliteringspenning
# Välj sjukfrånvaro per bransch och sektor, 2010-
# Läser in nödvändiga bibliotek med pacman
# Senast kontrollerad: 2024-02-23 (data)
if (!require("pacman")) install.packages("pacman")
p_load(here,
       openxlsx,
       tidyverse)

#test_list <- diag_sjukpenning_bransch(skapa_fil=FALSE)
diag_sjukfall_bransch <- function(output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                     filnamn = "startade_sjukfall_bransch_bearbetad.xlsx",
                                     spara_data = TRUE){
  
  # ========================================== Inställningar ============================================
  # Text till diagram
  diagram_capt <- "Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna."

  #==========================================================================================================
  
  # Läser in data från Excel
  # Antal startade sjukfall per 1000 förvärvsarbetande
  antal_sjukfall_bransch_df <-read.xlsx("G:/skript/projekt/data/kvinnor_man/Antal startade sjukfall bransch.xlsx",sheet = 2)

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

  # Sparar data till Excel
  if (spara_data==TRUE){
    flik_lista=lst("Bransch" = antal_sjukfall_bransch_df)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  }
 
}
