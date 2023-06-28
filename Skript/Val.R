## =================================================================================================================
# Skript som laddar hem data för ledamöter i riksdag och fullmäktige från SCB
# =================================================================================================================
# Läser in nödvändiga bibliotek med pacman
if (!require("pacman")) install.packages("pacman")
p_load(pxweb,
       openxlsx)

# Funktioner som behövs
source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")

#test_list <- diag_val(skapa_fil = FALSE)

diag_val<-function(region_vekt = "20", 
                   output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                   spara_data = TRUE,
                   filnamn = "val.xlsx"){
  
   # =============================================== API-uttag ===============================================
  # Riksdag /ME0107C/Riksdagsledamoter, "ME0107A6"
  # Landsting /ME0107B/Ltledamoter, "ME0107A5"
  # Kommunfullmäktige /ME0107A/Kfledamoter ,"ME0107A4"
  
  # Skapar två vektorer som används för att ploca ut data via pxweb (i en loop)
  url_vec <- c("/ME0107C/Riksdagsledamoter","/ME0107B/Ltledamoter","/ME0107A/Kfledamoter")
  contentscode_vec <- c( "ME0107A6","ME0107A5","ME0107A4")
  
  # Funkar i praktiken bara för Dalarnas län eftersom vissa koder skrivs LG och vissa L. Oklart varför
  # Skapar en lista med regioner som skall anropas. För riksdag och region tas alla ut medan vara kommuner för valt län tas ut i sista fallet
  regioner <- lst("*",paste0(region_vekt,"LG"),hamtakommuner(lan=region_vekt,tamedlan=FALSE,tamedriket=FALSE,allakommuner=FALSE))
  
  # "Adresser" till SCBs databas
  url1 <- "https://api.scb.se"
  url2 <- "/OV0104/v1/doris/sv/ssd/ME/ME0107"
  
  for(val in 1:length(url_vec) ){
    
    url3 <-paste0(url1,url2,url_vec[val])
    
    # Variabler som skall tas ut
    varlista <-  list("Region"=c(regioner[[val]]),
                      "Parti"=c("*"),
                      "Kon"=c("1","2"),
                      "ContentsCode"=c(contentscode_vec[val]),
                      "Tid"=c("*"))
    px_uttag <- pxweb_get(url = url3,query = varlista)
    
    if (val==1) 
    {
      riksdagsval_df <-as.data.frame(px_uttag, column.name.type = "text", variable.value.type = "text")
    } else if (val==2){
      regionval_df <-as.data.frame(px_uttag, column.name.type = "text", variable.value.type = "text")
    } else {
      kommunval_df <- as.data.frame(px_uttag, column.name.type = "text", variable.value.type = "text")
    }
    
  }
  
  # Sparar data till Excel
  if (spara_data==TRUE){
    flik_lista=lst("Riksdagsval" = riksdagsval_df,"Regionval" = regionval_df, "Kommunval" = kommunval_df)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  }
  
  # if(riksdagsval_stapel==TRUE){
  #   # Summerar riksdagsledamöter på kön och valår
  #   riksdagsval_summering_df <- riksdagsval_df %>%
  #     filter(region=="Totalt för riket") %>%
  #     group_by(kön,valår) %>%
  #     summarise("Antal" = sum(`Riksdagsledamöter`))
  # 
  # 
  #   diagramtitel <- paste0("Riksdagsledamöter per valår")
  #   diagramfilnamn <- paste0("Riksdagsledamöter",".png")
  #   ifelse(j==1,objektnamn <- paste0("Riksdagsledamöter"),objektnamn <- c(objektnamn,paste0("Riksdagsledamöter")))
  #   j=j+1
  #   # Skapar diagram för medianinkomsten i Dalarna där män jämförs med kvinnor.
  #   gg_obj <- SkapaStapelDiagram(skickad_df = riksdagsval_summering_df,
  #                                skickad_x_var = "valår",
  #                                skickad_y_var = "Antal",
  #                                skickad_x_grupp = "kön",
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
  # if(regionval_stapel==TRUE){
  #   # Summerar regionfullmäktigeledamöter på kön och  valår
  #   regionval_summering_df <- regionval_df %>%
  #     group_by(kön,valår) %>%
  #     summarise("Antal" = sum(`Regionfullmäktigledamöter`))
  # 
  #   diagramtitel <- paste0("Regionfullmäktigeledamöter per valår i ", ValdGeografi)
  #   diagramfilnamn <- paste0("regionfullmaktige_", ValdGeografi, ".png")
  #   #objektnamn <- c(objektnamn,paste0("regionfullmaktige_", ValdGeografi))
  #   ifelse(j==1,objektnamn <-paste0("regionfullmaktige_", ValdGeografi),objektnamn <- c(objektnamn,paste0("regionfullmaktige_", ValdGeografi)))
  #   j=j+1
  #   # Linjediagram där utvecklingen för kvinnors och mäns löner jämförs
  #   gg_obj <- SkapaStapelDiagram(skickad_df = regionval_summering_df,
  #                                skickad_x_var = "valår",
  #                                skickad_y_var = "Antal",
  #                                skickad_x_grupp = "kön",
  #                                x_axis_lutning = 0,
  #                                #skickad_filter_OR_var = "region",
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
  # if(kommunval_stapel==TRUE){
  #   # Summerar riksdagsledamöter på år
  #   kommunval_summering_df <- kommunval_df %>%
  #     filter(valår==max(kommunval_df$valår))%>%
  #     group_by(region,kön) %>%
  #     summarise("Antal" = sum(`Kommunfullmäktigledamöter`))
  # 
  #   diagramtitel <- paste0("Kommunfullmäktigeledamöter i ", ValdGeografi," ",max(kommunval_df$valår))
  #   diagramfilnamn <- paste0("kommunfullmäktige_", ValdGeografi, ".png")
  #   #objektnamn <- c(objektnamn,paste0("kommunfullmäktige_", ValdGeografi))
  #   ifelse(j==1,objektnamn <- paste0("kommunfullmäktige_", ValdGeografi),objektnamn <- c(objektnamn,paste0("kommunfullmäktige_", ValdGeografi)))
  #   j=j+1
  #   # Linjediagram där utvecklingen för kvinnors och mäns löner jämförs
  #   gg_obj <- SkapaStapelDiagram(skickad_df = kommunval_summering_df,
  #                                skickad_x_var = "region",
  #                                skickad_y_var = "Antal",
  #                                skickad_x_grupp = "kön",
  #                                manual_x_axis_text_vjust = 1,
  #                                manual_x_axis_text_hjust = 1,
  #                                #skickad_filter_OR_var = "region",
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
  # names(gg_list) <-  c(objektnamn)
  # return(gg_list)
}

