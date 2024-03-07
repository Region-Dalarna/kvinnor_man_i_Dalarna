# Skript som laddar hem data och skriver ut figurer kopplat till skuldsättning. Källa: Kronofogden
library(tidyverse)
library(dplyr)
library(tidyr)
library(stringi)

# Laddar in de funktioner som används för att skapa diagram
source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)

#test_list <- diag_kronofogden(skapa_fil = FALSE)

diag_kronofogden <-function(region_vekt = "20", 
                            output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                            skapa_fil = TRUE,
                            Diagram_skuldsatta=TRUE,
                            Diagram_skuldsatta_lange=TRUE){
# ========================================== Inställningar ============================================
# Text till diagram
diagram_capt <- c("Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Antal personer med skulder under indrivning hos Kronofogden",
                  "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Skuldsatta personer som funnits i Kronofogdens register i mer än 20 år")

ValdGeografi <- hamtaregion_kod_namn(region_vekt)$region

gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
i=1 # Räknare som används för att lägga till objekt i listan
j=1 # Räknare som används för att namnge objekt
# ======================================================================================================

# Skuldsatta mer än 20 år - https://kronofogden.entryscape.net/catalog/2/datasets/196
langsiktiga_skulder_df <-read.csv2("https://kronofogden.entryscape.net/store/2/resource/201")

# Antal personer med skulder under indrivning hos Kronofogden https://kronofogden.entryscape.net/catalog/2/datasets/3
skulder_df <-read.csv2("https://kronofogden.entryscape.net/store/2/resource/27")

# Ändrar till kvinnor och män (istället för Kvinna/Man)
langsiktiga_skulder_df[langsiktiga_skulder_df=="Kvinna"]<-"kvinnor"
langsiktiga_skulder_df[langsiktiga_skulder_df=="Man"]<-"män"

# Ändrar till kvinnor och män (istället för Kvinna/Man)
skulder_df[skulder_df=="Kvinna"]<-"kvinnor"
skulder_df[skulder_df=="Man"]<-"män"

# Gör om antal personer till en integer
langsiktiga_skulder_df$Antal.personer <- strtoi(langsiktiga_skulder_df$Antal.personer, base=0L)

# Gör om årtal till en character
langsiktiga_skulder_df$År <- as.character(langsiktiga_skulder_df$År)

skulder_df$År <- as.character(skulder_df$År)

# Vill enbart fokusera på Dalarna och struntar initialt i kommunnivå
langsiktiga_skulder_df_sum <- langsiktiga_skulder_df %>% 
  filter(Län=="DALARNA") %>% 
    group_by(År,Län,Kön) %>% 
      summarize(Antal_skuldsatta=sum(Antal.personer))

# Vill enbart fokusera på Dalarna och struntar initialt i kommunnivå
skulder_df_sum <- skulder_df %>% 
  filter(Län=="DALARNA") %>% 
    group_by(År,Län,Kön) %>% 
      summarize(Antal_skuldsatta=sum(Antal.skuldsatta),
                Total_skuld=sum(Skuldbelopp),
                Skuld_per_person=Total_skuld/Antal_skuldsatta)
# Gör om skuldbelopp till miljarder
miljard <- 1000000000
skulder_df_sum$skuld_miljard <-skulder_df_sum$Total_skuld/miljard 

if(Diagram_skuldsatta==TRUE){
diagramtitel <- paste0("Antal skuldsatta i Dalarnas län")
diagramfilnamn <- paste0("antal_skuldsatta_Dalarna.png")
ifelse(j==1,objektnamn <-"Antal_skuldsatta",objektnamn <-c(objektnamn,"Antal_skuldsatta"))
j=j+1

# Skapar diagram för medianinkomsten i Dalarna där män jämförs med kvinnor.
gg_obj <- SkapaStapelDiagram(skickad_df = skulder_df_sum, 
                             skickad_x_var = "År", 
                             skickad_y_var = "Antal_skuldsatta", 
                             skickad_x_grupp = "Kön",
                             x_axis_lutning = 0,
                             manual_color = diagramfarger("kon"),
                             diagram_titel = diagramtitel,
                             diagram_capt =  diagram_capt[1],
                             diagram_facet = FALSE,
                             manual_y_axis_title = "Antal skuldsatta",
                             berakna_index = FALSE,
                             output_mapp = output_mapp,
                             filnamn_diagram = diagramfilnamn,
                             skriv_till_diagramfil = skapa_fil)

gg_list[[i]] <-gg_obj
i=i+1
}
if(Diagram_skuldsatta==TRUE){
  diagramtitel <- paste0("Antal långsiktigt skuldsatta i Dalarnas län")
  diagramfilnamn <- paste0("Antal_langsiktigt_skuldsatta.png")
  ifelse(j==1,objektnamn <-"Antal_langsiktigt_skuldsatta",objektnamn <-c(objektnamn,"Antal_langsiktigt_skuldsatta"))
  j=j+1
  
  # Skapar diagram för medianinkomsten i Dalarna där män jämförs med kvinnor.
  gg_obj <- SkapaStapelDiagram(skickad_df = langsiktiga_skulder_df_sum, 
                               skickad_x_var = "År", 
                               skickad_y_var = "Antal_skuldsatta", 
                               skickad_x_grupp = "Kön",
                               x_axis_lutning = 0,
                               manual_color = diagramfarger("kon"),
                               diagram_titel = diagramtitel,
                               diagram_capt =  diagram_capt[2],
                               diagram_facet = FALSE,
                               manual_y_axis_title = "Antal långsiktigt skuldsatta",
                               berakna_index = FALSE,
                               output_mapp = output_mapp,
                               filnamn_diagram = diagramfilnamn,
                               skriv_till_diagramfil = skapa_fil)
  
  gg_list[[i]] <-gg_obj
  i=i+1
}
names(gg_list)<-objektnamn
return(gg_list)
}
