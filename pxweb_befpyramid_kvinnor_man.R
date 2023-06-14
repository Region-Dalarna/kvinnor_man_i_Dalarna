library(pxweb)
library(httr)
library(askpass)
library(writexl)
library(data.table)
library(dplyr)
library(tidyr)
library(readr)
library(ggplot2)
library(forcats)
#library(gifski)      # lägg ihop flera png-filer till en animerad gif-fil
library(here)
#start <- Sys.time()

source("G:/skript/func/func_diagramfunktioner.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_logga_i_diagram.R", encoding = "utf-8", echo = FALSE)
source("G:/skript/func/func_filer.R", encoding = "utf-8", echo = FALSE)

#test_list <-diag_befpyramid(delaupp_aldergrp=TRUE,ar=c("1968","2021"),region=c("20"),skapa_fil=TRUE,jmf_linje="")
diag_befpyramid <- function(skapa_fil=FALSE,
                            region="20",
                            ar="2021",
                            jmf_linje="",
                            jmfr_geo="00",
                            jmfr_ar=NA,
                            delaupp_aldergrp=FALSE,
                            skapa_GIFanimering=FALSE,
                            utskriftmapp="G:/skript/jon/Slask/"){

  # ==================================== inställningar per körning ==========================================
  
  delaupp_aldergrp <- delaupp_aldergrp            # dela upp i åldersgrupper, annars kör med 1-årsgrupper
  skapa_GIFanimering <- skapa_GIFanimering         # TRUE om vi vill skapa en GIF-fil för varje geografi, OM det finns mer än ett år i ar_vekt
  skapa_fil <- skapa_fil
  
  geo_vekt <- region    # Geografiska områden som vi vill göra befolkningspyrmaider för (kommunkod, länskod eller 00 för riket)
  ar_vekt <- ar #ar_vekt <-c("2021", "2030")         # år att göra befolkningspyramider för    as.character(1968:2020)    # alla år mellan 1968 och 2020
  # c("1968", "2020")
  # as.character(1968:2028)
  
  # TRUE om vi vill jämföra med inrikes och utrikes befolkning, går före jmfr annan region
  jmfr_linje <- jmf_linje # "utr_inr"   # andra alternativ "geo" "utr_inr", "ar", "" = ingen jmfr-linje
  jmfr_geo <- jmfr_geo          #"00" #regionkod anges här, endast en, MÅSTE anges om man kör "geo" som jmfr_linje
  jmfr_ar <- jmfr_ar
  
  # här hamnar diagramfilerna
  #utskriftmapp <- here("output","/")
  utskriftmapp <- utskriftmapp
  
  # Skapar en lista
  gg_list=list()
  i=1
  # Skapar en vektor som används för att namnge ggplot-objekt
  objektnamn=list()
  # =========================================== huvud-inställningar =======================================
  
  # sökväg till logga för att kunna lägga in den i diagrammen
  logga_path <- "G:/Samhällsanalys/MallarLoggor/logo_liggande_fri_svart.png"
  diagram_capt <- "Källa: SCB\nBearbetning: Samhällsanalys, Region Dalarna"
  
  # vektor att lägga in som geo_vekt
  # locationDalarnaRiket <- "20"
  #c("00","17", "21", "20","2021","2023","2026","2029","2031","2034","2039","2061","2062","2080","2081","2082","2083","2084","2085")
  
  # färger att använda i diagrammet
  stapelfarg_kon <- diagramfarger("kon")
  # ställ in total längd på animerad gif-fil (i sekunder)
  gif_langd_sek <- 10
  
  # =============================================== API-uttag ===============================================
  
  # =============================== lite bearbetningar innan vi kör igång
  if (is.na(jmfr_linje)) jmfr_linje <- ""     # för att säkerställa att jmfr_linje får ett värde
  # man måste ange jmfr_geo om man vill jämföra geografi, annars nollställs jmfr_linje
  if (jmfr_linje == "geo" & is.na(jmfr_geo)) jmfr_linje <- NA
  if (jmfr_linje != "geo") jmfr_geo <- NA
  if (jmfr_linje == "geo" & geo_vekt == jmfr_geo) jmfr_linje <- ""
  
  # används att lägga på ett listnamn i en query för API-uttag, utfall att man har tabeller som har samma variabelnamn men 
  # vi behöver använda olika värden i dem. Om man lägger på sok_suffix + ett id så kan man använda det i brutto-querylistan
  # samt i listan över variabler som ska vara med i aktuellt uttag
  sok_suffix <- "__nr__"
  # ======================================================================
  
  # För att komma förbi proxyn
  set_config(use_proxy(url = "http://mwg.ltdalarna.se", port = 9090))
  set_config(config(ssl_verifypeer = 0L))
  
  # Lägg ihop jmfr_geo med geo_vekt för att plocka ut alla geo vi behöver
  if (jmfr_linje == "geo") geo_vekt <- c(geo_vekt, jmfr_geo)
  geo_vekt <- geo_vekt[!duplicated(geo_vekt)]  # säkerställ att inga dubletter kommer med
  
  # kolla vilket som är det senaste året för befolkningsstatistiken
  senaste_befmangd_ar <- hamta_senaste_tid_i_tabell("https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy", "år")
  
  # Om användaren vill jämföra med inrikes/utrikes födda, kolla vilket som är det första året i tabellen
  if(jmfr_linje=="utr_inr"){
    # Hämtar det tidigaste året i uttaget som rör inrikes/utrikes födda
    tidigaste_ar=hamta_tidigaste_tid_i_tabell("https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101E/InrUtrFoddaRegAlKon")
    # Kör en for-loop som ersätter år som ligger före det tidigaste året med det tidigaste året
    for(loop_obs in 1:length(ar_vekt)){
      if(ar_vekt[loop_obs]<tidigaste_ar){
        print("============================================================================")
        print(paste0("Valt årtal saknar data för inrikes/utrikes födda, ersätter ",ar_vekt[loop_obs]," med ",tidigaste_ar))
        print("============================================================================")
        ar_vekt[loop_obs]<-tidigaste_ar
      }
    }
  }
  
  if (is.na(ar_vekt)) ar_vekt <- senaste_befmangd_ar # lägg senaste året i ar_vekt om den är NA
  
  befmangd_vekt_ar <- ar_vekt[ar_vekt <= senaste_befmangd_ar]
  befprogn_vekt_ar_riket <- ar_vekt[(ar_vekt > senaste_befmangd_ar)& geo_vekt == "00"]
  befprogn_vekt_ar_ejriket <- ar_vekt[(ar_vekt > senaste_befmangd_ar)& geo_vekt != "00"]
  
  # Skapar två tomma vektorer
  befprogn_ejriket_vekt <- character(0)
  befprogn_riket_vekt <- character(0)
  # Sätter värden till de två vektorerna bara om vi vill göra en prognos (dvs befprogn_vekt har ett värde)
  if(length(befprogn_vekt_ar_riket)!=0) befprogn_riket_vekt <- geo_vekt[geo_vekt == "00"]
  if(length(befprogn_vekt_ar_ejriket)!=0) befprogn_ejriket_vekt <- geo_vekt[geo_vekt != "00"]
  
  # här fyller vi en lista med url:er till tabellerna som kan behöva användas
  # 
  tab_list <- list(url = c(
    "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy",
    "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgOsiktRegN",
    "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgOsiktDetNb",
    "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101E/InrUtrFoddaRegAlKon",
    "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefProgRegFakN",
    "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0401/BE0401A/BefolkprognRevNb"), 
    contcode = c("BE0101N1", "000004LG", "000005MQ", "000001NS", "000005RC", "000005NO"), 
    vekt_test = list(befmangd_vekt_ar, befprogn_vekt_ar_ejriket, befprogn_vekt_ar_riket,
                     befmangd_vekt_ar, befprogn_vekt_ar_ejriket, befprogn_vekt_ar_riket), 
    geo_fetch = list(geo_vekt, befprogn_ejriket_vekt, befprogn_riket_vekt,
                     geo_vekt, befprogn_ejriket_vekt, befprogn_riket_vekt), 
    tid_fetch = list(befmangd_vekt_ar, befprogn_vekt_ar_ejriket, befprogn_vekt_ar_riket,
                     befmangd_vekt_ar, befprogn_vekt_ar_ejriket, befprogn_vekt_ar_riket),
    varlista_tab = list(c("Region", "Kon", "Alder", "ContentsCode", "Tid"),
                        c("Region", "Kon", "Alder", "ContentsCode", "Tid"),
                        c("Kon", "Alder", "ContentsCode", "Tid"),
                        c("Region", "Kon", "Alder", "Fodelseregion", "ContentsCode", "Tid"),
                        c("Region", "Kon", "Alder", "InrikesUtrikes", "ContentsCode", "Tid"),
                        c("Kon", "Alder", "InrikesUtrikes", "ContentsCode", "Tid")),
    inr_utr_fodd = c(FALSE, FALSE, FALSE, TRUE, TRUE, TRUE),
    riket_tab = c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE)
  )
  # nollställ variabel som vi lägger ihop alla tabelluttag i
  px_alla <- NULL
  
  for (tabell in 1:length(tab_list[[1]])){         # loopa genom alla tabell-uttag
    #print(tabell)
    if (length(tab_list$vekt_test[[tabell]])>0){    # kontrollera att vektorn inte är tom
      if (!(tab_list$inr_utr_fodd[[tabell]] & (jmfr_linje != "utr_inr")) &      # utr_inr födda kör vi bara om jmfr_inr_utr är TRUE
          !(!tab_list$inr_utr_fodd[[tabell]] & jmfr_linje == "utr_inr")) {     # men kör inte om inr_utr_tabell är FALSE, och samma med inr födda      
        # ta bara med de år i skickade vektorn som finns i tabellen
        giltig_vekt <- hamta_giltig_tid_tabell(tab_list$url[[tabell]], "år", tab_list$varlista_tab[[tabell]])
        tid_fetch_ny <- tab_list$tid_fetch[[tabell]][tab_list$tid_fetch[[tabell]] %in% giltig_vekt]
        
        # gör en brutto-query som vi plockar bort variabler från sedan
        query_list <- list(
          Region = tab_list$geo_fetch[[tabell]],
          Kon = '*',
          Alder= '*',
          Fodelseregion ='*',
          InrikesUtrikes = c("13", "23"),
          ContentsCode = tab_list$contcode[[tabell]],
          Tid = tid_fetch_ny)
        # plocka bort rader ur query_list som inte är med i detta uttag
        query_list <- query_list[names(query_list) %in% tab_list$varlista_tab[[tabell]]]
        
        while(length(grep(sok_suffix, names(query_list)))>0){
          sok_listnamn <- names(query_list)[grep(sok_suffix, names(query_list))[1]]
          names(query_list)[grep(sok_suffix, names(query_list))[1]] <- substr(sok_listnamn, 1, unlist(gregexpr(sok_suffix, sok_listnamn))-1)
        }
        
        # hämta data
        px_uttag <- pxweb_get(url = tab_list$url[[tabell]], query = query_list) 
        px_df <- as.data.frame(px_uttag)            # lägg uttag i df
        
        
        if (sum(names(px_df) == "region") > 0) {    # tabell för regioner ej riket, lägg till regionkod och lägg först av kolumnerna
          px_df <- px_df %>% 
            cbind(regionkod = as.data.frame(px_uttag, column.name.type = "code", variable.value.type = "code") %>% 
                    select(Region)) %>% 
            rename(regionkod = Region) %>% 
            relocate(regionkod, .before = region)
        } else {
          px_df <- px_df %>% 
            mutate(regionkod = "00", region = "Riket") %>% 
            relocate(c(regionkod, region), .before = NULL)
        }
        
        # döp om Folkmängd 31 dec till bara Folkmängd
        if (sum(names(px_df) == "Folkmängd 31 dec") > 0) px_df <- px_df %>% 
          rename(Folkmängd = `Folkmängd 31 dec`) 
        
        # döp om Födelseregion till inrikes/utrikes
        if (sum(names(px_df) == "födelseregion") > 0) px_df <- px_df %>% 
          rename(`inrikes/utrikes född` = födelseregion) 
        # koda om så att alla inr utr födda-variabler och dess värden heter lika
        if (sum(names(px_df) == "inrikes/utrikes född") > 0) {
          px_df$`inrikes/utrikes född`[px_df$`inrikes/utrikes född` == "Född i Sverige"] <- "inrikes födda"
          px_df$`inrikes/utrikes född`[px_df$`inrikes/utrikes född` == "Utrikes född"] <- "utrikes födda"
        }
        px_alla <- rbind(px_alla, px_df)          # lägg ihop alla df:s
      } # slut test om inr_utr
    } # test om vektor är tom
  } # slut på for-loop för tab_list
  
  
  # =============================================== bearbeta data ==============================================
  
  # döp om den ihopslagna df:n till px_df igen
  px_df <- px_alla
  # Skapa en numerisk åldersvariabel 
  px_df$åldernum <- suppressWarnings(parse_number(px_df$ålder))
  
  if (delaupp_aldergrp){
    
    # Lägg ihop i åldersgrupper
    px_df$aldergrp <- ifelse(px_df$åldernum <= 4, "0-4 år", NA)
    px_df$aldergrp <- ifelse(px_df$åldernum >= 5 & px_df$åldernum <= 14 , "5-14 år", px_df$aldergrp)
    px_df$aldergrp <- ifelse(px_df$åldernum >= 15 & px_df$åldernum <= 24 , "15-24 år", px_df$aldergrp)
    px_df$aldergrp <- ifelse(px_df$åldernum >= 25 & px_df$åldernum <= 34 , "25-34 år", px_df$aldergrp)
    px_df$aldergrp <- ifelse(px_df$åldernum >= 35 & px_df$åldernum <= 44 , "35-44 år", px_df$aldergrp)
    px_df$aldergrp <- ifelse(px_df$åldernum >= 44 & px_df$åldernum <= 54 , "45-54 år", px_df$aldergrp)
    px_df$aldergrp <- ifelse(px_df$åldernum >= 55 & px_df$åldernum <= 64 , "55-64 år", px_df$aldergrp)
    px_df$aldergrp <- ifelse(px_df$åldernum >= 65 & px_df$åldernum <= 74 , "65-74 år", px_df$aldergrp)
    px_df$aldergrp <- ifelse(px_df$åldernum >= 75 & px_df$åldernum <= 84 , "75-84 år", px_df$aldergrp)
    px_df$aldergrp <- ifelse(px_df$åldernum >= 85 & px_df$åldernum <= 94 , "85-94 år", px_df$aldergrp)
    px_df$aldergrp <- ifelse(px_df$åldernum >= 95, "95+ år", px_df$aldergrp)
    px_df$aldergrp <- ifelse(px_df$ålder == "totalt ålder", "totalt", px_df$aldergrp)
    px_df <- px_df[px_df$aldergrp != "totalt",]
    
    # sortera åldersgrupper korrekt
    px_df$aldergrp <- factor(px_df$aldergrp, levels = c("0-4 år", "5-14 år", "15-24 år", "25-34 år", "35-44 år", "45-54 år", 
                                                        "55-64 år", "65-74 år","75-84 år", "85-94 år", "95+ år"))
  } else {
    px_df <- px_df[px_df$ålder != "totalt ålder",]
    px_df$aldergrp <- px_df$ålder
    #px_df$aldergrp <- factor(px_df$ålder)                  
    #px_df$aldergrp <- fct_reorder(px_df$aldergrp, px_df$åldernum)
    px_df$aldergrp <- ifelse(px_df$åldernum > 99 & px_df$åldernum <= 105 , "100+ år", px_df$aldergrp)
    px_df <- px_df %>% 
      group_by(across(c(-Folkmängd))) %>%
      summarize(Folkmängd = sum(Folkmängd)) %>% 
      ungroup()
    # Skapa en numerisk åldersvariabel 
    px_df$åldernum <- suppressWarnings(parse_number(px_df$aldergrp))
    px_df$aldergrp <- factor(px_df$aldergrp)                  
    px_df$aldergrp <- fct_reorder(px_df$aldergrp, px_df$åldernum)
  }
  
  
  # ===================================================== skapa diagram ====================================================
  
  aggr_df <- px_df %>% 
    select(-åldernum, -ålder) %>% 
    group_by(across(c(-Folkmängd))) %>%
    summarize(bef = sum(Folkmängd)) %>% 
    ungroup()
  
  # ta bort år ur ar_vekt som inte finns i databasen
  ar_vekt <- ar_vekt[ar_vekt %in% aggr_df$år]
  
  # sära på geo och jmfr_geo om jämförslinje för geo är valt
  if (jmfr_linje == "geo") {
    aggr_jmfr <- aggr_df[aggr_df$regionkod == jmfr_geo,]
    aggr_df <- aggr_df[aggr_df$regionkod != jmfr_geo,]
    df_list <- list(aggr_jmfr, aggr_df)
    
    for (df_item in 1:length(df_list)) {
      df_list[[df_item]] <- df_list[[df_item]] %>% 
        group_by(år, regionkod, region, kön, aldergrp) %>% 
        summarise(bef = sum(bef)) %>% 
        mutate(proc = (bef / sum(bef)*100))
    }
    aggr_jmfr <- df_list[[1]]
    aggr_df <- df_list[[2]]
    geo_vekt <- geo_vekt[geo_vekt !=jmfr_geo]
  } 
  
  # sära på inrikes och utrikes födda om jämförslinje för utr_inr är valt
  if (jmfr_linje == "utr_inr") {
    aggr_jmfr <- aggr_df[aggr_df$`inrikes/utrikes född` == "utrikes födda",]
    aggr_df <- aggr_df[aggr_df$`inrikes/utrikes född` == "inrikes födda",]
    df_list <- list(aggr_jmfr, aggr_df)
    
    for (df_item in 1:length(df_list)) {
      df_list[[df_item]] <- df_list[[df_item]] %>% 
        group_by(år, regionkod, region, kön, aldergrp) %>% 
        summarise(bef = sum(bef)) %>% 
        mutate(proc = (bef / sum(bef)*100))
    }
    aggr_jmfr <- df_list[[1]]
    aggr_df <- df_list[[2]]
  }
  
  # bestäm om vi ska skriva ut diagram med absolut antal eller procent
  if (jmfr_linje == "geo" | jmfr_linje == "utr_inr") y_var <- as.name("proc") else y_var <- as.name("bef")
  
  # skapa progress bar 
  #pb <- txtProgressBar(min = 1, max = length(geo_vekt) * length(ar_vekt), style = 3)
  #pb_rakn <- 1
  
  # funktion för formattering av etiketter
  etikett_format <- function(x){
    x <- format(abs(x), big.mark = " ", scientific = FALSE)
    if (y_var == "proc") x <- paste0(x, " %")
    return(x)
  }
  # funktion för att skriva ut var n:e etikett på en axel
  # för att användas om alla åldersgrupper skrivs ut och det blir trångt på axeln
  every_nth = function(n) {
    return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
  }

  # loopa genom varje geografi och gör en befolkningspyrmaid för varje valt år
  for (reg in 1:length(geo_vekt)) {
    # välj ut kommunen vi ska göra befpyrmaider för ur geo_vekt
    poppyr_kommun <- aggr_df %>%
      filter(regionkod == geo_vekt[reg])
    
    # ta ut maxärde till att göra y-axeln (med bef eller proc)
    if (jmfr_linje == "geo") poppyr_kommun_jmfr <- aggr_jmfr   # om vi jmfr geo ska ingen filtrering göras på geografi här (jmfr med samma geografi i alla plottade geografier)  
    if (jmfr_linje == "utr_inr") poppyr_kommun_jmfr <- aggr_jmfr %>% filter(regionkod == geo_vekt[reg])
    # om vi ska jämföra i procent, kolla maxvärdet även i jmfr-vektorn, annars ta bara maxvärdet i poppyr_kommun
    if (y_var == "proc") {
      max_vekt <- c(poppyr_kommun[[y_var]], poppyr_kommun_jmfr[[y_var]])
      komm_max <- max(max_vekt)
    } else {
      komm_max <- max(poppyr_kommun[y_var])                       # ta ut maxvärden för kommunen för alla år så att axlarna blir likadana alla år för varje kommun
    }
    
    if (skapa_GIFanimering) gif_lista <- NULL                # om vi ska göra gif-animation, nollställ giflistan för varje ny kommun
    # loopa igenom alla år som är valda, baklänges så att filerna ska skrivas i rätt årsordning (viktigt för animeringen om vi gör en sån)
    k=1
    for (ar in length(ar_vekt):1){
      
      # filtrera ut rätt år för den kommun vi valt   
      poppyr <- poppyr_kommun %>% filter(år == ar_vekt[ar])
      
      # filtrera ut rätt år för jämförelse_df
      if (jmfr_linje != "") poppyr_jmfr <- poppyr_kommun_jmfr %>% filter(år == ar_vekt[ar])
      
      # diagramtitel
      diagram_titel <- paste0("Befolkning i ", unique(poppyr$region), " år ", ar_vekt[ar])
      if (jmfr_linje == "geo") diagram_titel <- paste0(diagram_titel ,"\njämfört med ", poppyr_kommun_jmfr$region[1])
      if (jmfr_linje == "utr_inr") diagram_titel <- paste0("Inrikes födda i ", poppyr_kommun_jmfr$region[1], " år ", ar_vekt[ar],
                                                           "\njämfört med utrikes födda")
      jmfr_linje_lbl <- ifelse(jmfr_linje == "geo", poppyr_kommun$region[1], "inrikes födda")
      # Skapar ett objektnamn och lägger till lista
      objektnamn<-c(objektnamn,diagram_titel)
      # print(objektnamn)
      # k=k+1
      # print(k)
      # skapa diagram
      gg_obj <- ggplot(poppyr, aes(x = aldergrp, fill = kön,
                         y = ifelse(kön == "kvinnor", -!!y_var, !!y_var))) + 
        geom_bar(stat = "identity") +
        {if (jmfr_linje != ""){       # kör om det finns en jämförelse
          geom_line(data = poppyr_jmfr, size = 1.3,
                    aes(x = aldergrp,
                        linetype = regionkod,
                        group = kön,
                        y = ifelse(kön == "kvinnor", -!!y_var, !!y_var)))
        }} +
        scale_y_continuous(labels = etikett_format, 
                           limits = max(komm_max) * c(-1,1)) + #,
        {if (!delaupp_aldergrp) scale_x_discrete(breaks = every_nth(n = 2)) } +
        labs(y = ifelse(y_var == "bef","antal invånare", "åldersgruppernas andel av den totala befolkningen per kön"),
             caption = diagram_capt,
             title = diagram_titel) +
        scale_color_manual(name = ifelse(jmfr_linje == "", "", jmfr_linje_lbl),
                           values = stapelfarg_kon, 
                           aesthetics = c("color", "fill")) +
        { if (jmfr_linje != ""){     # lägg till legende för linje om vi har en linje med i diagrammet
          scale_linetype_manual(name = element_blank(), 
                                values = "solid",
                                labels = ifelse(jmfr_linje == "geo", poppyr_kommun_jmfr$region[1], "utrikes födda"))
        }  
        } +
        theme( axis.text.x = element_text(size = 10),
               axis.text.y = element_text(size = ifelse(delaupp_aldergrp, 10,8)),
               axis.title.y = element_blank(),
               axis.title.x = element_text(size = 8),
               #legend.text = element_text(size = 12),
               plot.caption = element_text(face = "italic",hjust = 0, vjust = 0),
               plot.caption.position = "plot",
               plot.title = element_text(hjust = 0.5, size = 20),
               legend.key = element_blank(),
               panel.background = element_rect(fill = "white"),
               panel.grid.major.y = element_line(size=0.4, colour = "lightgrey"),
               panel.grid.minor.y = element_line(size=0.4, colour = "lightgrey"),
               panel.grid.major.x = element_line(size=0.4, colour = "lightgrey"),
               panel.grid.minor.x = element_line(size=0.4, colour = "lightgrey"),
               axis.ticks = element_blank()) +
        guides(fill = guide_legend(order =1),
               linetype = guide_legend(order =2))+
        coord_flip()
      
  
      
      # Spara filen om användaren har valt detta
      if(skapa_fil==TRUE){
        # Ändra höjd och bredd på den sparade png-filen, + ange mapp och filnamn
        bredd <- 12
        hojd <- 7
        if (jmfr_linje != "") jmfr_filnamn <- ifelse(jmfr_linje == "utr_inr", "_jmfr_utr_inr", paste0("_jmfr_", poppyr_jmfr$region[1])) else jmfr_filnamn <- ""
        fullpath <- paste0(utskriftmapp, "Befpyramid ", poppyr$region[1], " ", ar_vekt[ar], jmfr_filnamn, ".png")
        if (skapa_GIFanimering) gif_filnamn <- paste0(utskriftmapp, "bef pyramid ", poppyr$region[1], " alla år.gif")
        suppressMessages(ggsave(fullpath, width = bredd, height = hojd))
        
        # Lägg till logga till diagrammet =======================================
        
        if (!is.null(logga_path)){  
          add_logo(
            plot_path = fullpath, # url or local file for the plot
            logo_path = logga_path, # url or local file for the logo
            logo_position = "bottom right", # choose a corner
            # 'top left', 'top right', 'bottom left' or 'bottom right'
            logo_scale = 15,
            #10 as default, but can change to manually make logo bigger (lägre tal = större logga)
            replace = TRUE
          )
        }
      }

      if (skapa_GIFanimering) gif_lista <- c(gif_lista, fullpath)

      #setTxtProgressBar(pb, pb_rakn)
      #pb_rakn <- pb_rakn +1
      gg_list[[i]] <-gg_obj
      i=i+1

     } # for-loop för årsvektor slut

    # skapa GIF-animering om vi har fler än ett år och om användaren valt att göra det
    if (skapa_GIFanimering) {                     # animerad gif görs om användaren valt det i inställningar
      if (length(gif_lista)>1) {                  # det måste vara minst 2 år per geografi för att en animering ska kunna göras
        del_langd <- gif_langd_sek / length(gif_lista)        # angiven maxlängd på animerad giffil delat med antal år i vektorn
        if (del_langd > 1) del_langd == 1                     # ett år visas aldrig längre än en sekund
        gifski(rev(gif_lista), gif_file = gif_filnamn, width = 2000, height = 1200, delay = del_langd)
      }  # slut på loop för att kontrollera att gif_lista innehåller fler än ett element
    } # slut på loop för att skapa GIF-animering
    
  } # for-loop för geo_vekt slut
# Namnger ggplot-objekt i listan
names(gg_list) <- objektnamn
# Returnerar listan
return(gg_list)
}
#close(pb)

#print(paste0("Det tog ", round(difftime(Sys.time(), start, units = "mins"),2), " minuter att köra skriptet."))
