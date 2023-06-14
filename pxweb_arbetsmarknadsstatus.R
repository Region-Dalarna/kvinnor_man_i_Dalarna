## =================================================================================================================
# Skript som laddar hem data för arbetsmarknadsstatus från SCB och skriver ut 6 diagram
# =================================================================================================================

library(pxweb)
library(httr)
library(askpass)
library(dplyr)
library(tidyr)
library(stringi)
library(svDialogs)
library(tidyverse)

# Laddar in de funktioner som används för att skapa diagram
source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)

#test_list <- diag_arbetsloshet(skapa_fil = FALSE)

diag_arbetsloshet <-function(region_vekt = "20", 
                             output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                             skapa_fil = TRUE,
                             diag_lan = TRUE,
                             diag_kommun = TRUE,
                             diag_arbetslosthet = TRUE,
                             diag_arbetskraftsdeltagande = TRUE,
                             diag_sysselsattningsgrad = TRUE){
  
  # ========================================== Inställningar ============================================
  # Text till diagram
  diagram_capt <- c("Källa: SCB:s öppna statistikdatabas, befolkningens arbetsmarknadsstatus (BAS).\nBearbetning: Samhällsanalys, Region Dalarna.\n",
                    "Källa: SCB:s öppna statistikdatabas, befolkningens arbetsmarknadsstatus (BAS).\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Andel i befolkningen som är i arbetskraften (arbetskraftsdeltagande).",
                    "Källa: SCB:s öppna statistikdatabas, befolkningens arbetsmarknadsstatus (BAS).\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Andelen av befolkningen som är sysselsatt (sysselsättningsgrad).")
  
  ValdGeografi <- hamtaregion_kod_namn(region_vekt)$region
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  i=1 # Räknare som används för att lägga till objekt i listan
  j=1 # Räknare som används för att namnge objekt
  #==========================================================================================================
  # För att komma förbi proxyn
  set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090, username = Sys.getenv("userid"), password = Sys.getenv("pwd")))
  set_config(config(ssl_verifypeer = 0L))
  # =============================================== API-uttag ===============================================
  # "Adresser" till SCBs databas
  url1 <- "https://api.scb.se"
  url2 <- "/OV0104/v1/doris/sv/ssd/AM/AM0210/AM0210A/ArbStatusAr"
  url3 <- paste0(url1, url2)
  
  # Variabler som skall tas ut
  varlista <-  list("Region"=c("*"),
                    "Kon"=c("1","2","1+2"),
                    "Alder"=c("20-64"),
                    "Fodelseregion"=c("in","ut","tot"),
                    "ContentsCode"=c("000006J1","000006IY","000006J6"),
                    "Tid"=c("*"))
  
  # Uttag av data
  px_uttag <- pxweb_get(url = url3,query = varlista)
  
  # Konverterar data till en Data Frame och tar med regionkod
  arbetsmarknadsstatus_df <- as.data.frame(px_uttag) %>% 
    cbind(regionkod = as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
            select(Region))
  
  arbetsmarknadsstatus_df <- arbetsmarknadsstatus_df %>% 
    rename(regionkod = Region)%>%
      relocate(regionkod, .before = region)

  # Tar bort län i länsnamnet
  arbetsmarknadsstatus_df$region <-skapa_kortnamn_lan(arbetsmarknadsstatus_df$region)
  
  # # Byter variabelnamn så att de inte längre innehåller å, ä och ö
  # arbetsmarknadsstatus_df<-arbetsmarknadsstatus_df %>% 
  #   rename("kon"=kön,"alder"=ålder,"fodelseregion"=födelseregion,"ar"=år,"arbetsloshet"=arbetslöshet,"sysselsattningsgrad"=sysselsättningsgrad)
  # 
  # I de första diagrammen skall vi enbart fokusera på riket och län.
  # Dessutom vill vi bara ha uppdelat på kön och bakgrund, inte totalt också.
  arbetsmarknadsstatus_df_lan <- arbetsmarknadsstatus_df %>% 
    filter(regionkod %in% hamtaAllaLan(tamedriket = TRUE),kön!="totalt",födelseregion!="totalt")
  
  # I de andra figurerna vill vi istället fokusera på vald regions kommuner
  arbetsmarknadsstatus_df_kommun <- arbetsmarknadsstatus_df %>%
    filter(regionkod %in% hamtakommuner(lan=region_vekt,tamedlan=TRUE,tamedriket = FALSE),kön!="totalt",födelseregion!="totalt")
      
  # Plockar ut de regioner där data saknas för någon grupp. Dessa tas sedan bort från dataset
  # Inte relevant längre, men sparar kodsnutten för eventuell framtida användning
  # temp_list=list()
  # j=1
  # while (j <= length(arbetslösa_df$region)){
  #   if(is.na(arbetslösa_df$Procent[j])){
  #     temp_list <-append(temp_list,arbetslösa_df$region[j])
  #   }
  #   j=j+1
  # }
  # 
  # arbetslösa_df <- arbetslösa_df %>% 
  #   filter(!(region%in%temp_list) )
  # 
  # rm(temp_list)
  
  # Skapar en tom lista som skall innehålla variabelnamn
  list_variabler <- list()
  # Lägger till variablnamn i listan baserat på vilka variabler användaren väljer
  if(diag_arbetslosthet==TRUE) list_variabler <- c(list_variabler,"arbetslöshet")
  if(diag_arbetskraftsdeltagande==TRUE) list_variabler <- c(list_variabler,"arbetskraftsdeltagande")
  if(diag_sysselsattningsgrad==TRUE) list_variabler <- c(list_variabler,"sysselsättningsgrad")
  # Om användaren har satt alla variabelnamn till false så väljs arbetslöshet automatiskt (annars kraschar programmet)
  if(length(list_variabler)==0){
    print("Minst en variabel måste väljas. Variabel sätts automatiskt till arbetslöshet")
    list_variabler <- c(list_variabler,"arbetslöshet")
  }
  # Om användaren inte har valt antingen län eller kommun så väljs län automatiskt (annars kraschar programmet)
  if(diag_lan==FALSE & diag_kommun==FALSE){
    print("Antingen län eller kommun (eller båda) måste väljas. Sätter automatistkt till län")
    diag_lan = TRUE
  }
  
  if(diag_lan==TRUE){
    k=1
    while(k <= length(list_variabler)){
      
      # Diverse parametrar som behövs för diagrammet
      switch(list_variabler[[k]],
             "arbetslöshet" = {caption<-diagram_capt[[1]]},
             "arbetskraftsdeltagande"= {caption<-diagram_capt[[2]]},
             {
               caption<-diagram_capt[[3]]
             }
      )
      diagramtitel <- paste0(list_variabler[[k]]," i åldersgruppen 20-64 år ",max(arbetsmarknadsstatus_df$år))
      # Ändrar första bokstaven i rubriken till stor bokstav
      diagramtitel<-stringr::str_to_sentence(diagramtitel)
      diagramfilnamn <- paste0(list_variabler[[k]],"_lan",".png")
      ifelse(j==1,objektnamn <-paste0(list_variabler[[k]],"_lan"),objektnamn <-c(objektnamn,paste0(list_variabler[[k]],"_lan")))
      j=j+1
      
      # Skapar diagram 
      gg_obj <- SkapaStapelDiagram(skickad_df = arbetsmarknadsstatus_df_lan[arbetsmarknadsstatus_df_lan$år==max(arbetsmarknadsstatus_df_lan$år),], 
                                   skickad_x_var = "region", 
                                   skickad_y_var = list_variabler[[k]], 
                                   skickad_x_grupp = "kön",
                                   manual_y_axis_title = "procent",
                                   manual_x_axis_text_vjust=1,
                                   manual_x_axis_text_hjust=1,
                                   manual_color = diagramfarger("kon"),
                                   diagram_titel = diagramtitel,
                                   diagram_capt =  caption,
                                   diagram_facet = TRUE,
                                   facet_grp = "födelseregion",
                                   facet_scale = "fixed",
                                   facet_legend_bottom = TRUE,
                                   x_axis_sort_value = TRUE,
                                   x_axis_lutning = 45,
                                   berakna_index = FALSE,
                                   output_mapp = output_mapp,
                                   filnamn_diagram = diagramfilnamn,
                                   skriv_till_diagramfil = skapa_fil)
      
      gg_list[[i]] <-gg_obj
      i=i+1
      k=k+1
    }
  }
  
  if(diag_kommun==TRUE){
    k=1
    while(k <= length(list_variabler)){
      
      # Diverse parametrar som behövs för diagrammet
      switch(list_variabler[[k]],
             "arbetslöshet" = {caption<-diagram_capt[[1]]},
             "arbetskraftsdeltagande"= {caption<-diagram_capt[[2]]},
             {
               caption<-diagram_capt[[3]]
             }
      )
      diagramtitel <- paste0(list_variabler[[k]]," i åldersgruppen 20-64 år ",max(arbetsmarknadsstatus_df$år))
      # Ändrar första bokstaven i rubriken till stor bokstav
      diagramtitel<-stringr::str_to_sentence(diagramtitel)
      diagramfilnamn <- paste0(list_variabler[[k]],"_kommun",".png")
      ifelse(j==1,objektnamn <-paste0(list_variabler[[k]],"_kommun"),objektnamn <-c(objektnamn,paste0(list_variabler[[k]],"_kommun")))
      j=j+1
      
      # Skapar diagram 
      gg_obj <- SkapaStapelDiagram(skickad_df = arbetsmarknadsstatus_df_kommun[arbetsmarknadsstatus_df_kommun$år==max(arbetsmarknadsstatus_df_kommun$år),], 
                                   skickad_x_var = "region", 
                                   skickad_y_var = list_variabler[[k]], 
                                   skickad_x_grupp = "kön",
                                   manual_y_axis_title = "procent",
                                   manual_x_axis_text_vjust=1,
                                   manual_x_axis_text_hjust=1,
                                   manual_color = diagramfarger("kon"),
                                   diagram_titel = diagramtitel,
                                   diagram_capt =  caption,
                                   diagram_facet = TRUE,
                                   facet_grp = "födelseregion",
                                   facet_scale = "fixed",
                                   facet_legend_bottom = TRUE,
                                   x_axis_sort_value = TRUE,
                                   x_axis_lutning = 45,
                                   berakna_index = FALSE,
                                   output_mapp = output_mapp,
                                   filnamn_diagram = diagramfilnamn,
                                   skriv_till_diagramfil = skapa_fil)
      
      gg_list[[i]] <-gg_obj
      i=i+1
      k=k+1
    }
  }
  names(gg_list)<-objektnamn
  return(gg_list)
}

