## =================================================================================================================
# Skript som laddar hem data för andel chefer från SCB och skapar ett diagram
# Källa: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AA__AA0003__AA0003B/IntGr1LanKonUtb/
# =================================================================================================================
# Läser in nödvändiga bibliotek med pacman
if (!require("pacman")) install.packages("pacman")
p_load(pxweb,
       openxlsx)

# Funktioner som behövs
source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")

#test_list <- diag_chefer(skapa_fil = FALSE)

diag_chefer<-function(region_vekt = "20", 
                      output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                      filnamn = "chefer.xlsx",
                      spara_data = TRUE){
  
  # =============================================== API-uttag ===============================================
  # "Adresser" till SCBs databas
  url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AA/AA0003/AA0003B/IntGr1LanKonUtb"

  # Variabler som skall tas ut
  varlista <-  list(Region=c(region_vekt),
                    Kon=c("1","2"),
                    UtbNiv=c("000","F","3","EU","US"),
                    BakgrVar=c("tot20-64"),
                    ContentsCode=c("0000001Y"),
                    Tid=c("*"))
  
  # Uttag av data
  chefer_df <- pxweb_get(url = url,query = varlista) %>% 
    as.data.frame(column.name.type = "text", variable.value.type = "text") %>% 
      rename(Andel = `Andel i chefsposition, procent`)
  
  # Andrar namnet på vissa kategorier
  chefer_df[chefer_df=="utbildningsnivå: förgymnasial utbildning"] <- "förgymnasial utbildning"
  chefer_df[chefer_df=="utbildningsnivå: gymnasial utbildning"] <- "gymnasial utbildning"
  chefer_df[chefer_df=="utbildningsnivå: eftergymnasial utbildning"] <- "eftergymnasial utbildning"
  
  # Sparar data till Excel
  if (spara_data==TRUE){
    flik_lista=lst("Chefer" = chefer_df)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  }
  
  # if (andel_chefer==TRUE){
  #   
  #   diagramtitel <- paste0("Andel chefer ",max(chefer_df$år)," i ",ValdGeografi)
  #   diagramfilnamn <- paste0("andel_chefer.png")
  #   objektnamn <- c(objektnamn,"andel_chefer")
  #   
  #   # Skapar diagram för andel chefer 2021.
  #   gg_obj <- SkapaStapelDiagram(skickad_df =chefer_df[chefer_df$år==max(chefer_df$år) & chefer_df$utbildningsnivå!="utbildningsnivå: uppgift saknas",], 
  #                                skickad_x_var = "utbildningsnivå", 
  #                                skickad_y_var = "Andel i chefsposition, procent", 
  #                                skickad_x_grupp = "kön",
  #                                manual_x_axis_text_vjust=1,
  #                                manual_x_axis_text_hjust=1,
  #                                manual_color = diagramfarger("kon"),
  #                                diagram_titel = diagramtitel,
  #                                diagram_capt =  diagram_capt[1],
  #                                diagram_facet = FALSE,
  #                                x_axis_sort_value = TRUE,
  #                                manual_y_axis_title="procent",
  #                                berakna_index = FALSE,
  #                                output_mapp = output_mapp,
  #                                filnamn_diagram = diagramfilnamn,
  #                                skriv_till_diagramfil = skapa_fil)
  #   
  #   gg_list[[i]] <-gg_obj
  #   i=i+1
  # }
  # 
  # if(andel_chefer_linje==TRUE){
  #   diagramtitel <- paste0("Förändring i andel chefer 2001-",max(chefer_df$år)," i ",ValdGeografi)
  #   diagramfilnamn <- paste0("andel_chefer_linje.png")
  #   objektnamn <- c(objektnamn,"andel_chefer_linje")
  #   
  #   # Skapar diagram för andel chefer 2021.
  #   gg_obj <- SkapaLinjeDiagram(skickad_df =chefer_df[chefer_df$år!="2000" & chefer_df$utbildningsnivå=="samtliga utbildningsnivåer",], 
  #                               skickad_x_var = "år", 
  #                               skickad_y_var = "Andel i chefsposition, procent", 
  #                               skickad_x_grupp = "kön",
  #                               x_axis_lutning=0,
  #                               manual_color = diagramfarger("kon"),
  #                               diagram_titel = diagramtitel,
  #                               diagram_capt =  diagram_capt[2],
  #                               diagram_facet = FALSE,
  #                               #manual_y_axis_title="procent",
  #                               berakna_index = TRUE,
  #                               output_mapp = output_mapp,
  #                               filnamn_diagram = diagramfilnamn,
  #                               skriv_till_diagramfil = skapa_fil)
  #   
  #   gg_list[[i]] <-gg_obj
  #   i=i+1
  # }
  # names(gg_list) <- c(objektnamn)
  # return(gg_list)
  
}
