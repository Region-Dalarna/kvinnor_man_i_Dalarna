diag_ledamoter_val <- function(outputmapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                              spara_dataframe_till_global_environment = FALSE,
                              spara_figur = TRUE,
                              typ_av_val = c("riksdag","region","kommun")){
  
  # =================================================================================================================
  # Skript som skriver ut tre diagram för respektive val (riksdag, region och kommun) med antal ledamöter per kön.
  # Källa för data: https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__ME__ME0107/
  # Funkar enbart för Dalarna för tillfället. Detta eftersom vissa koder för regionval skrivs LG och vissa L. Oklart varför. Se under if("region" %in% typ_av_val) nedan
  # =============================================== API-uttag ===============================================
  
  # Läser in nödvändiga bibliotek med pacman
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         openxlsx)
  
  gg_list <- list()
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\n Bearbetning: Samhällsanalys, Region Dalarna."
  
  
  # Funktioner som behövs
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  
  # Riksdag /ME0107C/Riksdagsledamoter, "ME0107A6"
  # Landsting /ME0107B/Ltledamoter, "ME0107A5"
  # Kommunfullmäktige /ME0107A/Kfledamoter ,"ME0107A4"
  
  url_vec <- c()
  contentscode_vec <- c()
  regioner <- list()
  
  if("riksdag" %in% typ_av_val){
    url_vec <- c(url_vec,"/ME0107C/Riksdagsledamoter")
    contentscode_vec <- c(contentscode_vec,"ME0107A6")
    regioner <- c(regioner,list("*"))
  }
  
  if("region" %in% typ_av_val){
    url_vec <- c(url_vec,"/ME0107B/Ltledamoter")
    contentscode_vec <- c(contentscode_vec,"ME0107A5")
    regioner <- c(regioner,list(paste0("20","LG")))
  }
  
  if("kommun" %in% typ_av_val){
    url_vec <- c(url_vec,"/ME0107A/Kfledamoter")
    contentscode_vec <- c(contentscode_vec,"ME0107A4")
    regioner <- c(regioner,list(hamtakommuner(lan="20",tamedlan=FALSE,tamedriket=FALSE,allakommuner=FALSE)))
  }

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
    
    if (url_vec[val]=="/ME0107C/Riksdagsledamoter") 
    {
      riksdagsval_df <-as.data.frame(px_uttag, column.name.type = "text", variable.value.type = "text") %>%
        filter(region=="Totalt för riket") %>%
          group_by(kön,valår) %>%
            summarise("Antal" = sum(Antal))
      
      if(spara_dataframe_till_global_environment) {
        assign("riksdagsval_df", riksdagsval_df, envir = .GlobalEnv)
      }
      
      diagramtitel <- paste0("Riksdagsledamöter per valår i Dalarna")
      diagramfilnamn <- paste0("Riksdagsledamoter",".png")
      
      gg_obj <- SkapaStapelDiagram(skickad_df = riksdagsval_df,
                                   skickad_x_var = "valår",
                                   skickad_y_var = "Antal",
                                   skickad_x_grupp = "kön",
                                   x_axis_lutning = 0,
                                   manual_color = diagramfarger("kon"),
                                   diagram_titel = diagramtitel,
                                   diagram_capt =  diagram_capt,
                                   stodlinjer_avrunda_fem = TRUE,
                                   output_mapp = outputmapp,
                                   filnamn_diagram = diagramfilnamn,
                                   skriv_till_diagramfil = spara_figur)    
      
      gg_list <- c(gg_list, list(gg_obj))
      names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
      
    } else if (url_vec[val]=="/ME0107B/Ltledamoter"){
      
      regionval_df <-as.data.frame(px_uttag, column.name.type = "text", variable.value.type = "text") %>%
        group_by(kön,valår) %>%
          summarise("Antal" = sum(Antal))
      
      if(spara_dataframe_till_global_environment) {
        assign("regionval_df", regionval_df, envir = .GlobalEnv)
      }
      
      diagramtitel <- paste0("Regionfullmäktigeledamöter per valår i Dalarna")
      diagramfilnamn <- paste0("Regionfullaktigeledamoter",".png")
      
      gg_obj <- SkapaStapelDiagram(skickad_df = regionval_df,
                                   skickad_x_var = "valår",
                                   skickad_y_var = "Antal",
                                   skickad_x_grupp = "kön",
                                   x_axis_lutning = 0,
                                   manual_color = diagramfarger("kon"),
                                   diagram_titel = diagramtitel,
                                   diagram_capt =  diagram_capt,
                                   stodlinjer_avrunda_fem = TRUE,
                                   output_mapp = outputmapp,
                                   filnamn_diagram = diagramfilnamn,
                                   skriv_till_diagramfil = spara_figur)    
      
      gg_list <- c(gg_list, list(gg_obj))
      names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
      
    } else {
      kommunval_df <- as.data.frame(px_uttag, column.name.type = "text", variable.value.type = "text") %>%
        filter(valår==max(valår))%>%
          group_by(region,valår,kön) %>%
            summarise("Antal" = sum(`Antal`))
      
      if(spara_dataframe_till_global_environment) {
        assign("kommunval_df", kommunval_df, envir = .GlobalEnv)
      }
      
      diagramtitel <- paste0("Kommunfullmäktigeledamöter i Dalarna ", max(kommunval_df$valår))
      diagramfilnamn <- paste0("Kommunfullmaktigeledamoter",".png")
      
      gg_obj <- SkapaStapelDiagram(skickad_df = kommunval_df,
                                   skickad_x_var = "region",
                                   skickad_y_var = "Antal",
                                   skickad_x_grupp = "kön",
                                   x_axis_lutning = 45,
                                   manual_color = diagramfarger("kon"),
                                   manual_x_axis_text_vjust = 1,
                                   manual_x_axis_text_hjust = 1,
                                   diagram_titel = diagramtitel,
                                   diagram_capt =  diagram_capt,
                                   stodlinjer_avrunda_fem = TRUE,
                                   output_mapp = outputmapp,
                                   filnamn_diagram = diagramfilnamn,
                                   skriv_till_diagramfil = spara_figur)    
      
      gg_list <- c(gg_list, list(gg_obj))
      names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
    }
    
  }
  
  return(gg_list)

}

