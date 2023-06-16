if (!require("pacman")) install.packages("pacman")
p_load(pxweb)

#test_list=diag_befolkning_alder()
diag_befolkning_alder <- function(region_vekt="20",
                                  skapa_fil=FALSE,
                                  output_mapp="G:/skript/jon/Slask/",
                                  diagram_senastear=TRUE,
                                  diagram_prognos=TRUE){
  
  # ========================================== Inställningar ============================================
  # Text till diagram
  diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."
  valda_farger <-c(rgb(169,208,142, maxColorValue = 255),
                   rgb(112,173,71, maxColorValue = 255))
  
  ValdGeografi <- hamtaregion_kod_namn(region_vekt)$region
  
  gg_list <- list()  # skapa en tom lista att lägga flera ggplot-objekt i (om man skapar flera diagram)
  i=1 # Räknare som används för att lägga till objekt i listan
  
  #==========================================================================================================  #==========================================================================================================
  # För att komma förbi proxyn
  set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090, username = Sys.getenv("userid"), password = Sys.getenv("pwd")))
  set_config(config(ssl_verifypeer = 0L))
  # =============================================== API-uttag ===============================================
  # "Adresser" till SCBs databas
  url1 <- "https://api.scb.se"
  url2 <- c("/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy","/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgOsiktRegN")
  #url2 <- "/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  url3 <- paste0(url1, url2)
  prognosar=as.character(2022:2050)
  contents_code=c("BE0101N1", "000004LG")
  data <- list()
  
  
  # Loop som används för att förbereda data för såväl befintlig data som prognos.
  for(k in 1:length(url2)){
    url3 <- paste0(url1, url2[[k]])  
    
    if(k==1){
      reg=c(hamtaAllaLan(tamedriket = FALSE))
      tid=c(hamta_senaste_tid_i_tabell(url3))
    }else{
      reg=region_vekt
      tid=as.character(2022:2050)
    } 
    # Variabler som skall tas ut
    varlista <-  list("Region"=reg,
                      "Kon"=c("1","2"),
                      "Alder"= c("*"),
                      "ContentsCode"=contents_code[[k]],
                      "Tid"=tid)
    
    # Uttag av data
    px_uttag <- pxweb_get(url = url3,query = varlista)
    
    # Konverterar data till en Data Frame
    befolkning_df <- as.data.frame(px_uttag, column.name.type = "text", variable.value.type = "text")
    
    # Av någon oklar anledning så går det inte att skriva år som något annat än "*" eller enstaka år,
    # varför jag tar ut alla år och sedan begränsar
    if(k==2){
      befolkning_df <- befolkning_df %>% 
        filter(as.integer(år)<2051)
    }
    # Städar dataset
    # Tar bort värden efter årtal
    befolkning_df$ålder <- word(befolkning_df$ålder,1)
    # Data skall inte vara total. Dessutom görs 100+ om till 100
    befolkning_df <-befolkning_df[befolkning_df$ålder != "totalt",]
    befolkning_df[befolkning_df=="100+"]<-"100"
    # Tar bort län i länsnamnen
    befolkning_df$region <- skapa_kortnamn_lan(befolkning_df$region)
    # Slutligen så skapas en variabel som avgör om befolkningen är över 65 eller inte
    befolkning_df$aldersgrupp <- ifelse(as.integer(befolkning_df$ålder)>65,"Över 65","Under 65")
    # Summerar på aldersgrupp och beräknar andelar för de två grupperna
    if(k==1){
      befolkning_sum_df <- befolkning_df %>% 
        group_by(år,region,aldersgrupp) %>% 
        summarize(Befolkning_alder=sum(Folkmängd)) %>% 
        mutate(andel_aldre=round(Befolkning_alder/sum(Befolkning_alder),3)*100)
      data[[k]]<- befolkning_sum_df} else{
        
        befolkning_sum_df <- befolkning_df %>% 
          group_by(år,aldersgrupp) %>% 
          summarize(Befolkning_alder=sum(Folkmängd)) %>% 
          mutate(andel_aldre=round(Befolkning_alder/sum(Befolkning_alder),3)*100)
        data[[k]] <- befolkning_sum_df
        
      }
    
  }
  names(data) <- c("Befolkning_2021","Befolkning_prognos")
  
  # Tar bort län i länsnamnet
  data[[1]]$region<-skapa_kortnamn_lan(data[[1]]$region)
  
  
  # skapa fokusvariabel för att fokusera på valt län och riket
  data[[1]]$fokus <- NA                      # en metod för att få bort warning messages för "Unknown or uninitialised column: `fokus`."
  data[[1]]$fokus <- 0
  data[[1]]$fokus[data[[1]]$region =="Dalarna"] <- 1
  
  if(diagram_senastear==TRUE){
    diagramtitel <- paste0("Andel av befolkningen över 65 år ",unique(data[[1]]$år))
    diagramfilnamn <- paste0("andel_over_65_Sverige.png")
    objektnamn <- paste0("andel_over_65_Sverige")
    
    gg_obj <- SkapaStapelDiagram(skickad_df =data[[1]][data[[1]]$aldersgrupp=="Över 65",], 
                                 skickad_x_var = "region", 
                                 skickad_y_var = "andel_aldre", 
                                 #skickad_x_grupp = "kön",
                                 manual_color = diagramfarger("gron_tva_fokus"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 diagram_facet = FALSE,
                                 x_axis_sort_value = TRUE,
                                 x_var_fokus = "fokus",
                                 manual_y_axis_title="procent",
                                 berakna_index = FALSE,
                                 manual_x_axis_text_vjust = 1, 
                                 manual_x_axis_text_hjust = 1,
                                 x_axis_lutning = 45,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)
    gg_obj
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  if(diagram_prognos==TRUE){
    diagramtitel <- paste0("Andel av befolkningen över 65 år i ",hamtaregion_kod_namn(region_vekt)[[2]])
    diagramfilnamn <- paste0("andel_over_65_lan.png")
    objektnamn <- c(objektnamn,paste0("andel_over_65_lan"))
    
    gg_obj <- SkapaStapelDiagram(skickad_df = data[[2]][data[[2]]$aldersgrupp=="Över 65",], 
                                 skickad_x_var = "år", 
                                 skickad_y_var = "andel_aldre",
                                 manual_color = diagramfarger("gron_fyra")[2],
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 diagram_facet = FALSE,
                                 x_axis_sort_value = FALSE,
                                 manual_y_axis_title="procent",
                                 berakna_index = FALSE,
                                 manual_x_axis_text_vjust = 1, 
                                 manual_x_axis_text_hjust = 1,
                                 x_axis_lutning = 45,
                                 output_mapp = output_mapp,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = skapa_fil)
    gg_obj
    gg_list[[i]] <-gg_obj
    i=i+1
  }
  
  names(gg_list)<-c(objektnamn)
  return(gg_list)
}

