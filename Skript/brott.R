# Laddar hem data för olika typer av brott från Kolada
if (!require("pacman")) install.packages("pacman")
p_load(tidyverse,
       rKolada,
       openxlsx)

# Funktioner som behövs
source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")

hamta_data_brott <- function(region_vekt = "20",
                             output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                             spara_data = TRUE,
                             filnamn = "brott.xlsx"){
 
  # Relativt begränsad tillgång till data för kvinnofridskränkning
  kvinnofridskrankning_df <- get_values(
    kpi = c("N07402"),
    municipality = c(hamtakommuner(lan = region_vekt,tamedriket = FALSE,tamedlan = FALSE)),
    period = 1998:2100) %>% 
    rename("år" = year)
  
  sexualbrott_df <- get_values(
    kpi = c(" N07537"),
    municipality = c(hamtakommuner(lan = region_vekt,tamedriket = FALSE,tamedlan = FALSE)),
    period = 2015:2100) %>% 
    rename("år" = year)
  
  misshandel_df <- get_values(
    kpi = c(" N07401"),
    municipality = c(hamtakommuner(lan = region_vekt,tamedriket = FALSE,tamedlan = FALSE)),
    period = 2009:2100) %>% 
    rename("år" = year)
  
  valdsbrott_df <- get_values(
    kpi = c("N07403"),
    municipality = c(hamtakommuner(lan = region_vekt,tamedriket = FALSE,tamedlan = FALSE)),
    period = 1980:2100) %>% 
    rename("år" = year)
  
  # Sparar data till Excel
  if (spara_data==TRUE){
    flik_lista=lst("Kvinnofridskränkning" = kvinnofridskrankning_df,
                   "Sexualbrott" = sexualbrott_df,
                   "Misshandel" = misshandel_df,
                   "Våldsbrott" = valdsbrott_df)
    
    write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  }

}



# diagram_titel <- "Kvinnofridskränkning per 100 000 invånare i Dalarnas kommuner"
# diagramfil <- "sexualbrott.png"
# 
# SkapaStapelDiagram(skickad_df = kvinnofridskrankning_df %>% 
#                      filter(år %in% c(2015,(min(år)+(round((max(år) - min(år))/2,0))),max(år))),
#                    skickad_x_var = "municipality", 
#                    skickad_y_var = "value",
#                    skickad_x_grupp = "år",
#                    diagram_titel = diagram_titel,
#                    diagram_capt = "diagram_capt",
#                    manual_color = diagramfarger("rus_sex"),
#                    manual_y_axis_title = "",
#                    manual_x_axis_text_vjust = 1,
#                    manual_x_axis_text_hjust = 1,
#                    output_mapp = "",
#                    x_axis_sort_value = TRUE,
#                    x_axis_sort_grp = 3,
#                    vand_sortering = TRUE,
#                    filnamn_diagram = diagramfil)
# 
# 
# diagram_titel <- "Antal sexualbrott per 100 000 invånare i Dalarnas kommuner"
# diagramfil <- "sexualbrott.png"
# 
# SkapaStapelDiagram(skickad_df = sexualbrott_df %>% 
#                      filter(år %in% c(2015,(min(år)+(round((max(år) - min(år))/2,0))),max(år))),
#                    skickad_x_var = "municipality", 
#                    skickad_y_var = "value",
#                    skickad_x_grupp = "år",
#                    diagram_titel = diagram_titel,
#                    diagram_capt = "diagram_capt",
#                    manual_y_axis_title = "",
#                    manual_color = diagramfarger("rus_sex"),
#                    manual_x_axis_text_vjust = 1,
#                    manual_x_axis_text_hjust = 1,
#                    output_mapp = "",
#                    x_axis_sort_value = TRUE,
#                    x_axis_sort_grp = 3,
#                    vand_sortering = TRUE,
#                    filnamn_diagram = diagramfil)
# 
# diagram_titel <- "Anmälda våldsbrott per 100 000 invånare i Dalarnas kommuner"
# diagramfil <- "valdsbrott.png"
# 
# SkapaStapelDiagram(skickad_df = valdsbrott_df %>% 
#                      filter(år %in% c(min(år),(min(år)+(round((max(år) - min(år))/2,0))),max(år))),
#                    skickad_x_var = "municipality", 
#                    skickad_y_var = "value",
#                    skickad_x_grupp = "år",
#                    diagram_titel = diagram_titel,
#                    diagram_capt = "diagram_capt",
#                    manual_y_axis_title = "",
#                    manual_color = diagramfarger("rus_sex"),
#                    manual_x_axis_text_vjust = 1,
#                    manual_x_axis_text_hjust = 1,
#                    output_mapp = "",
#                    x_axis_sort_value = TRUE,
#                    x_axis_sort_grp = 3,
#                    vand_sortering = TRUE,
#                    filnamn_diagram = diagramfil)
# 
# diagram_titel <- "Anmälda misshandelsbrott per 100 000 invånare i Dalarnas kommuner"
# diagramfil <- "valdsbrott.png"
# 
# SkapaStapelDiagram(skickad_df = misshandel_df %>% 
#                      filter(år %in% c(min(år),(min(år)+(round((max(år) - min(år))/2,0))),max(år))),
#                    skickad_x_var = "municipality", 
#                    skickad_y_var = "value",
#                    skickad_x_grupp = "år",
#                    diagram_titel = diagram_titel,
#                    diagram_capt = "diagram_capt",
#                    manual_color = diagramfarger("rus_sex"),
#                    manual_y_axis_title = "",
#                    manual_x_axis_text_vjust = 1,
#                    manual_x_axis_text_hjust = 1,
#                    output_mapp = "",
#                    x_axis_sort_value = TRUE,
#                    x_axis_sort_grp = 3,
#                    vand_sortering = TRUE,
#                    filnamn_diagram = diagramfil)


