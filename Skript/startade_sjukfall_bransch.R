# Bearbetar data från nedladdat Exceldokument (se dokumentet för mer info) och spar ett nytt Exceldokument
# Läser in nödvändiga bibliotek med pacman
if (!require("pacman")) install.packages("pacman")
p_load(here,
       openxlsx,
       tidyverse)

# Laddar in de funktioner som används för att skapa diagram
# source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
# source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)

#test_list <- diag_sjukpenning_bransch(skapa_fil=FALSE)
diag_sjukpenning_bransch <- function(region_vekt = "20", 
                                     output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                     spara_data = TRUE){
  
  # ========================================== Inställningar ============================================
  # Text till diagram
  diagram_capt <- "Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna."

  #==========================================================================================================
  
  # Läser in data från Excel
  # Antal startade sjukfall per 1000 förvärvsarbetande
  antal_sjukfall_bransch_df <-read.xlsx(here("indata","/","Antal startade sjukfall bransch.xlsx"),sheet=2)
  # Antal sjukskrivningsdagar per förvärvsarbetande
  antal_sjukskrivningsdagar_df <-read.xlsx(here("indata","/","Antal startade sjukfall bransch.xlsx"),sheet=3)
  
  # Pivoterar data för att få den på rätt sätt
  antal_sjukfall_bransch_df <- antal_sjukfall_bransch_df %>% 
    pivot_longer(!SNI2007 & !År, names_to = "Kon", values_to = "Antal_startade_sjukfall") %>% 
    filter(Kon!="Samtliga")  
  
  antal_sjukskrivningsdagar_df <- antal_sjukskrivningsdagar_df %>% 
    pivot_longer(!SNI2007 & !År, names_to = "Kon", values_to = "Antal_sjukskrivningsdagar") %>% 
    filter(Kon!="Samtliga")
  
  antal_sjukfall_bransch_df$Kon <- tolower(antal_sjukfall_bransch_df$Kon)
  antal_sjukskrivningsdagar_df$Kon <- tolower(antal_sjukskrivningsdagar_df$Kon)
  
  # Ändrar branscher
  # Sjukfall
  antal_sjukfall_bransch_df[antal_sjukfall_bransch_df=="A - Jordbruk, skogsbruk och fiske"] <- "Jord- och skogsbruk"
  antal_sjukfall_bransch_df[antal_sjukfall_bransch_df=="B - Utvinning av mineral"] <- "Utvinning"
  antal_sjukfall_bransch_df[antal_sjukfall_bransch_df=="C - Tillverkning"] <- "Tillverkning"
  antal_sjukfall_bransch_df[antal_sjukfall_bransch_df=="D - Försörjning av el, gas, värme och kyla"] <- "Elförsörjning m.m."
  antal_sjukfall_bransch_df[antal_sjukfall_bransch_df=="F - Byggverksamhet"] <- "Bygg"
  antal_sjukfall_bransch_df[antal_sjukfall_bransch_df=="G - Handel; reparation av motorfordon och motorcyklar"] <- "Handel"
  antal_sjukfall_bransch_df[antal_sjukfall_bransch_df=="H - Transport och magasinering"] <- "Transport"
  antal_sjukfall_bransch_df[antal_sjukfall_bransch_df=="I - Hotell- och restaurangverksamhet"] <- "Hotell- och restaurang"
  antal_sjukfall_bransch_df[antal_sjukfall_bransch_df=="J - Informations- och kommunikationsverksamhet"] <- "IT och kommunikation"
  antal_sjukfall_bransch_df[antal_sjukfall_bransch_df=="K - Finans- och försäkringsverksamhet"] <- "Finans- och försäkring"
  antal_sjukfall_bransch_df[antal_sjukfall_bransch_df=="L - Fastighetsverksamhet"] <- "Fastighet"
  antal_sjukfall_bransch_df[antal_sjukfall_bransch_df=="M - Verksamhet inom juridik, ekonomi, vetenskap och teknik"] <- "Juridik, ekonomi m.m."
  antal_sjukfall_bransch_df[antal_sjukfall_bransch_df=="N - Uthyrning, fastighetsservice, resetjänster och andra stödtjänster"] <- "Diverse stödtjänster"
  antal_sjukfall_bransch_df[antal_sjukfall_bransch_df=="O - Offentlig förvaltning och försvar; obligatorisk socialförsäkring"] <- "Offentlig förvaltning"
  antal_sjukfall_bransch_df[antal_sjukfall_bransch_df=="P - Utbildning"] <- "Utbildning"
  antal_sjukfall_bransch_df[antal_sjukfall_bransch_df=="Q - Vård och omsorg; sociala tjänster"] <- "Vård och omsorg"
  antal_sjukfall_bransch_df[antal_sjukfall_bransch_df=="R - Kultur, nöje och fritid"] <- "Kultur m.m."
  antal_sjukfall_bransch_df[antal_sjukfall_bransch_df=="S - Annan serviceverksamhet"] <- "Annan serviceverksamhet"
  
  # Sjukskrivningsdagar
  antal_sjukskrivningsdagar_df[ antal_sjukskrivningsdagar_df=="A - Jordbruk, skogsbruk och fiske"] <- "Jord- och skogsbruk"
  antal_sjukskrivningsdagar_df[ antal_sjukskrivningsdagar_df=="B - Utvinning av mineral"] <- "Utvinning"
  antal_sjukskrivningsdagar_df[ antal_sjukskrivningsdagar_df=="C - Tillverkning"] <- "Tillverkning"
  antal_sjukskrivningsdagar_df[ antal_sjukskrivningsdagar_df=="D - Försörjning av el, gas, värme och kyla"] <- "Elförsörjning m.m."
  antal_sjukskrivningsdagar_df[ antal_sjukskrivningsdagar_df=="F - Byggverksamhet"] <- "Bygg"
  antal_sjukskrivningsdagar_df[ antal_sjukskrivningsdagar_df=="G - Handel; reparation av motorfordon och motorcyklar"] <- "Handel"
  antal_sjukskrivningsdagar_df[ antal_sjukskrivningsdagar_df=="H - Transport och magasinering"] <- "Transport"
  antal_sjukskrivningsdagar_df[ antal_sjukskrivningsdagar_df=="I - Hotell- och restaurangverksamhet"] <- "Hotell- och restaurang"
  antal_sjukskrivningsdagar_df[ antal_sjukskrivningsdagar_df=="J - Informations- och kommunikationsverksamhet"] <- "IT och kommunikation"
  antal_sjukskrivningsdagar_df[ antal_sjukskrivningsdagar_df=="K - Finans- och försäkringsverksamhet"] <- "Finans- och försäkring"
  antal_sjukskrivningsdagar_df[ antal_sjukskrivningsdagar_df=="L - Fastighetsverksamhet"] <- "Fastighet"
  antal_sjukskrivningsdagar_df[ antal_sjukskrivningsdagar_df=="M - Verksamhet inom juridik, ekonomi, vetenskap och teknik"] <- "Juridik, ekonomi m.m."
  antal_sjukskrivningsdagar_df[ antal_sjukskrivningsdagar_df=="N - Uthyrning, fastighetsservice, resetjänster och andra stödtjänster"] <- "Diverse stödtjänster"
  antal_sjukskrivningsdagar_df[ antal_sjukskrivningsdagar_df=="O - Offentlig förvaltning och försvar; obligatorisk socialförsäkring"] <- "Offentlig förvaltning"
  antal_sjukskrivningsdagar_df[ antal_sjukskrivningsdagar_df=="P - Utbildning"] <- "Utbildning"
  antal_sjukskrivningsdagar_df[ antal_sjukskrivningsdagar_df=="Q - Vård och omsorg; sociala tjänster"] <- "Vård och omsorg"
  antal_sjukskrivningsdagar_df[ antal_sjukskrivningsdagar_df=="R - Kultur, nöje och fritid"] <- "Kultur m.m."
  antal_sjukskrivningsdagar_df[ antal_sjukskrivningsdagar_df=="S - Annan serviceverksamhet"] <- "Annan serviceverksamhet"
  
  if(startade_sjukfall==TRUE){
    
    diagramtitel <- paste0("Antal startade sjukfall per 1000 förvärvsarbetande i Sverige ",max(antal_sjukfall_bransch_df$År))
    diagramfilnamn <- paste0("Startade_sjukfall_bransch", ValdGeografi, ".png")
    ifelse(j==1,objektnamn <-"Startade_sjukfall_bransch",objektnamn <-c(objektnamn,"Startade_sjukfall_bransch"))
    j=j+1
    
    gg_obj <- SkapaStapelDiagram(skickad_df = antal_sjukfall_bransch_df[antal_sjukfall_bransch_df$År==2020,], 
                                 skickad_x_var = "SNI2007", 
                                 skickad_y_var = "Antal_startade_sjukfall", 
                                 skickad_x_grupp = "Kon",
                                 manual_color = diagramfarger("kon"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 diagram_facet = FALSE,
                                 manual_x_axis_text_vjust = 1, 
                                 manual_x_axis_text_hjust = 1,
                                 facet_grp="Kon",
                                 manual_y_axis_title = "Antal startade sjukfall per 1000 förvärvsarbetande",
                                 diagram_liggande = FALSE,
                                 x_axis_sort_value = TRUE,
                                 x_axis_lutning = 45,
                                 facet_scale = "fixed",
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  if(sjukskrivningsdagar==TRUE){
    
    diagramtitel <- paste0("Antal sjukskrivningsdagar per förvärvsarbetande i Sverige " ,max(antal_sjukskrivningsdagar_df$År))
    diagramfilnamn <- paste0("Sjukskrivningsdagar_bransch", ValdGeografi, ".png")
    ifelse(j==1,objektnamn <-"Sjukskrivningsdagar_bransch",objektnamn <-c(objektnamn,"Sjukskrivningsdagar_bransch"))
    j=j+1
    
    gg_obj <- SkapaStapelDiagram(skickad_df = antal_sjukskrivningsdagar_df[antal_sjukskrivningsdagar_df$År==2020,], 
                                 skickad_x_var = "SNI2007", 
                                 skickad_y_var = "Antal_sjukskrivningsdagar", 
                                 skickad_x_grupp = "Kon",
                                 manual_color = diagramfarger("kon"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 manual_y_axis_title = "Antal sjukskrivningsdagar",
                                 diagram_facet = FALSE,
                                 facet_grp="Kon",
                                 diagram_liggande = TRUE,
                                 x_axis_sort_value = TRUE,
                                 x_axis_lutning = 0,
                                 facet_scale = "fixed",
                                 berakna_index = FALSE,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  names(gg_list) <- c(objektnamn)
  return(gg_list)
}
