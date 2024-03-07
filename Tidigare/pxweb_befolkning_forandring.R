# Historisk data och prognos för folkmängden i Dalarna
pacman::p_load(pxweb,httr,tidyverse,openxlsx)

source("G:/skript/func/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_text.R", encoding = "utf-8", echo = FALSE)

#test_list <- diag_befolkning(skapa_fil=FALSE)

diag_befolkning <-function(region_vekt = "20", 
                           output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                           skapa_fil = TRUE,
                           diag_befolkning_linjediagram=TRUE){
  
  # ========================================== Inställningar ============================================
  
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna"
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  j=1
  objektnamn=c() # Skapar en tom vektor som skall innehålla namn på figurer
  #==========================================================================================================
  # För att komma förbi proxyn
  set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090))
  set_config(config(ssl_verifypeer = 0L))
  
  # =============================================== API-uttag ===============================================

  url_folkmangd <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"

  
  varlista <- list(
    Region = c("00",region_vekt),
    Civilstand=c("*"),
    Kon = '*',
    Alder = '*',
    ContentsCode = "BE0101N1",
    Tid = '*')
  
  px_uttag <- pxweb_get(url = url_folkmangd,
                        query = varlista) 
  
  # Lägg API-uttaget i px_df, lägg på ytterligare ett uttag men med koder istället för klartext,
  # välj ut bara regionkolumnen i det andra uttaget, döp om den till regionkod och lägg den först av kolumnerna
  
  px_df <- as.data.frame(px_uttag) %>% 
    cbind(as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>%
            select(Region)) %>% 
    rename(regionkod = Region) %>% relocate(regionkod, .before = region)
  
  # I de fall där det finns en kategori total ålder skall dessa tas bort - blir dubbelräkning annars
  if("totalt ålder" %in% unique(px_df$ålder)) px_df<- px_df[px_df$ålder!="totalt ålder",]
  
  # Ändrar ålder på alla 100+ till 100. För att word-funktionen nedan skall funka
  if("100+ år" %in% unique(px_df$ålder)) px_df[px_df=="100+ år"]<-"100"
  
  # Mellanslag och år tas bort
  px_df$ålder<-as.integer(word(px_df$ålder))
  
  # Folkmängd 
  px_df_folkmangd <- px_df %>%
    group_by(år,region) %>% 
      summarize(`Folkmängd`=sum(`Folkmängd`))

  if(diag_befolkning_linjediagram==TRUE){
    diagram_titel <- paste0("Befolkningsförändring ",min(px_df_folkmangd$år)," - ",max(px_df_folkmangd$år), " (basår 1968)")
    diagram_typ <- "befolkning_forandring"
    diagramfil <- paste0(diagram_typ, ".png")
    objektnamn <- c(objektnamn,diagram_typ)
    
    gg_obj <- SkapaLinjeDiagram(skickad_df = px_df_folkmangd, 
                                 skickad_x_var = "år", 
                                 skickad_y_var = "Folkmängd",
                                 skickad_x_grupp = "region",
                                 manual_color = diagramfarger("gron_sex")[5:6],
                                 x_axis_lutning = 90,
                                 berakna_index = TRUE,
                                 diagram_titel = diagram_titel,
                                 diagram_capt = diagram_capt,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfil,
                                 skriv_till_diagramfil = skapa_fil)
    
    gg_list[[j]] <-gg_obj
    j=j+1
  }
  
  names(gg_list)<-c(objektnamn)
  return(gg_list)
  
}

