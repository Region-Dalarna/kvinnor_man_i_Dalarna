## =================================================================================================================
# Skript som laddar hem data för mediannkomst från SCB
# =================================================================================================================
if (!require("pacman")) install.packages("pacman")
pacman::p_load(pxweb,
               openxlsx)

# Laddar in de funktioner som används för att skapa diagram
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

#test_list <- diag_medianinkomst(skapa_fil = FALSE)

diag_medianinkomst<-function(region_vekt = "20",
                             output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                             filnamn = "medianinkomst.xlsx",
                             alder = c("20-64"), # Finns bland annat 65+ mfl, använd pxvardelist("https://api.scb.se/OV0104/v1/doris/sv/ssd/HE/HE0110/HE0110A/SamForvInk1","Alder")
                             spara_data = TRUE){
  
 
  # =============================================== API-uttag ===============================================
  # "Adresser" till SCBs databas
  url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/HE/HE0110/HE0110A/SamForvInk1"

  # Variabler som skall tas ut
  varlista <-  list("Region"=hamtakommuner(region_vekt),
                    "Kon"=c("1","2","1+2"),
                    "Alder" = alder,
                    "Inkomstklass"=c("TOT"),
                    "ContentsCode"=c("HE0110J8"),
                    "Tid"=c("*"))
  
  # Uttag av data
  px_uttag <- pxweb_get(url = url,query = varlista) 
    #as.data.frame(column.name.type = "text", variable.value.type = "text")
  
  medianinkomst_df <- as.data.frame(px_uttag) %>% 
    cbind(regionkod = as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
            select(Region)) %>% 
      rename(regionkod = Region)%>%
        relocate(regionkod, .before = region)
  
  if (spara_data==TRUE){
    flik_lista=lst("medianinkomst"= medianinkomst_df)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  }
  
  # # Summerar på år
  # medianinkomst_df <- medianinkomst_df %>% 
  #   group_by(region,kön, år) %>% 
  #   summarise("medianinkomst,tkr" = sum(`Medianinkomst, tkr`))
  # 
  # # DF som enbart fokuserar på total inkomst (dvs. inte uppdelat på kvinnor och män)
  # medianinkomst_df_total <- medianinkomst_df %>% filter (kön=="totalt")
  # 
  # #DF där uppdelning är på kön
  # medianinkomst_kon_df <- medianinkomst_df %>% filter (region!="Riket" & kön!="totalt")
  
  # if(medianinkomst_stapel==TRUE){
  #   
  #   diagram_typ <- "medianinkomst_total"
  #   diagramfilnamn <- paste0("medianinkomst_total_", ValdGeografi, ".png")
  #   objektnamn <- c(objektnamn,diagram_typ)
  #   # Skapar diagram för medianinkomst i valt län vs riket (ej uppdelat på kön)
  #   gg_obj <- SkapaStapelDiagram(skickad_df = medianinkomst_df_total, 
  #                                skickad_x_var = "år", 
  #                                skickad_y_var = "medianinkomst,tkr", 
  #                                skickad_x_grupp = "region",
  #                                manual_color = diagramfarger("gron_sex")[5:6],
  #                                diagram_titel = "Medianinkomst",
  #                                diagram_capt =  diagram_capt,
  #                                diagram_facet = FALSE,
  #                                dataetiketter = FALSE,
  #                                berakna_index = TRUE,
  #                                manual_x_axis_text_vjust = 0, 
  #                                manual_x_axis_text_hjust = 0.5,
  #                                x_axis_lutning = 45,
  #                                output_mapp = output_mapp,
  #                                filnamn_diagram = diagramfilnamn,
  #                                skriv_till_diagramfil = skapa_fil)
  #   
  #   gg_list[[i]] <-gg_obj
  #   i=i+1
  # } 
  # if(medianinkomst_linje==TRUE){
  #   
  #   diagram_typ <- "medianinkomst_linje"
  #   diagramfilnamn <- paste0("medianinkomst_total_forandring", ValdGeografi, ".png")
  #   objektnamn <- c(objektnamn,diagram_typ)
  #   
  #   # Skapar diagram för medianinkomst i valt län vs riket (ej uppdelat på kön)
  #   gg_obj <- SkapaLinjeDiagram(skickad_df = medianinkomst_df_total, 
  #                               skickad_x_var = "år", 
  #                               skickad_y_var = "medianinkomst,tkr", 
  #                               skickad_x_grupp = "region",
  #                               #manual_x_axis_text_vjust=1.2,
  #                               skickad_filter_OR_var = "region",
  #                               manual_color = diagramfarger("gron_sex")[5:6],
  #                               diagram_titel = "Medianinkomst",
  #                               diagram_capt =  diagram_capt,
  #                               diagram_facet = FALSE,
  #                               berakna_index = TRUE,
  #                               x_axis_lutning = 45,
  #                               output_mapp = output_mapp,
  #                               filnamn_diagram = diagramfilnamn,
  #                               skriv_till_diagramfil = skapa_fil)
  #   
  #   gg_list[[i]] <-gg_obj
  #   i=i+1
  # } 
  # if(medianinkomst_stapel_kon==TRUE){
  #   
  #   diagramtitel <- paste0("Medianinkomst (20-64 år) i ", ValdGeografi)
  #   diagram_typ <- "medianinkomst_kon"
  #   diagramfilnamn <- paste0("medianinkomst_kon_total_", ValdGeografi, ".png")
  #   objektnamn <- c(objektnamn,diagram_typ)
  #   
  #   
  #   gg_obj <- SkapaStapelDiagram(skickad_df = medianinkomst_kon_df, 
  #                                skickad_x_var = "år", 
  #                                skickad_y_var = "medianinkomst,tkr", 
  #                                skickad_x_grupp = "kön",
  #                                manual_x_axis_text_vjust=1,
  #                                manual_x_axis_text_hjust=0.5,
  #                                x_axis_lutning = 0,
  #                                manual_color = diagramfarger("kon"),
  #                                diagram_titel = diagramtitel,
  #                                diagram_capt =  diagram_capt,
  #                                diagram_facet = FALSE,
  #                                berakna_index = FALSE,
  #                                output_mapp = output_mapp,
  #                                filnamn_diagram = diagramfilnamn,
  #                                skriv_till_diagramfil = skapa_fil)
  #   
  #   gg_list[[i]] <-gg_obj
  #   i=i+1
  # } 
  # 
  # if(medianinkomst_linje_kon==TRUE){
  #   diagramtitel <- paste0("Förändring i medianinkomst (20-64 år) i ", ValdGeografi)
  #   diagramfilnamn <- paste0("medianinkomst_kon_total_forandring_", ValdGeografi, ".png")
  #   diagram_typ <- "medianinkomst_linje_kon"
  #   objektnamn <- c(objektnamn,diagram_typ)
  #   
  #   # Skapar diagram för medianinkomst i valt län vs riket (ej uppdelat på kön)
  #   gg_obj <- SkapaLinjeDiagram(skickad_df = medianinkomst_kon_df, 
  #                               skickad_x_var = "år", 
  #                               skickad_y_var = "medianinkomst,tkr", 
  #                               skickad_x_grupp = "kön",
  #                               #skickad_filter_OR_var = "region",
  #                               x_axis_lutning = 0,
  #                               manual_color = diagramfarger("kon"),
  #                               diagram_titel = diagramtitel,
  #                               diagram_capt =  diagram_capt,
  #                               diagram_facet = FALSE,
  #                               berakna_index = TRUE,
  #                               output_mapp = output_mapp,
  #                               filnamn_diagram = diagramfilnamn,
  #                               skriv_till_diagramfil = skapa_fil)
  #   
  #   gg_list[[i]] <-gg_obj
  #   i=i+1
  # }
  # names(gg_list) <-  c(objektnamn)
  # return(gg_list)
}

