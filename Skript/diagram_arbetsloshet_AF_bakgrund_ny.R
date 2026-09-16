diag_arbetsloshet_2008_senastear <- function(output_mapp = "G:/skript/jon/Slask/",
                                             spara_figur = TRUE,
                                             returnera_data = TRUE){
  
  
  # ========================================== Info ============================================
  # Arbetslöshet från 2008 och framåt. Enbart för Dalarna (1 diagram)
  # Källa: https://arbetsformedlingen.se/statistik/sok-statistik/tidigare-statistik
  #
  # ========================================== Info ============================================
  if (!require("pacman")) install.packages("pacman")
  pacman::p_load(openxlsx,
                 here,
                 tidyverse,
                 glue)
  
  # Funktioner som behövs (hämtas från Git-Hub)
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source(here("skript/","func_hamta_data_kvinnor_man.R"))
  
  gg_list <- list()
  
  # ========================================== Läser in data ============================================
  # URL från arbetsförmedlingen
  af_url <- "https://arbetsformedlingen.se/statistik/sok-statistik/tidigare-statistik-tidsserier"
  
  af_url_lankar <- read_html(af_url) %>%                  # läs in webbsidan ovan
    html_elements("a") %>%                                # hitta alla <a>-objekt
    html_attr("href") %>%                                 # ta ut alla länkar, dvs. <href>-objekt
    purrr::discard(is.na)
  
  avkodade_lankar <- af_url_lankar %>%
    URLdecode()
  
  # Väljer vilka länkar vi skall hämta data från
  af_index <- which(
    (str_detect(avkodade_lankar, "web-inskrivna-arbetslosa-andel-av-reg") |   # länkar som slutar på "reg"
       str_detect(avkodade_lankar, "web-inskrivna-arbetslosa-andel-av-bas")) & # eller "bas"
      !str_detect(avkodade_lankar, "(?i)arsgenomsnitt")                        # men uteslut "arsgenomsnitt"
  )
  
  # Skapar en lista med länkar som skall hämtas
  inlasfil <- af_url_lankar[af_index] %>%
    paste0("https://arbetsformedlingen.se", .)
  
  # Använder funktioner för att hämta data från de två källorna (en gammal och en ny)
  url_old = inlasfil[2]  # the one going up to 2022-12 (reg)
  url_new <- inlasfil[1]  # the one starting 2023-01 (bas)
  
  old_df <- process_url(url_old)
  new_df <- process_url(url_new)
  
  cutoff_datum <- "2022-12"
  cutoff_ar <- str_sub(cutoff_datum, 1, 4)
  
  # Slår ihop de två datakällorna och bearbetar data för diagrammet.
  regioner_df <- combine_old_new(old_df, new_df, cutoff = cutoff_datum) %>% 
    filter(KOM == "Dalarnas län") %>% 
    separate(PERIOD,c("Ar","Manad"),"-") %>%
    rename(Region = KOM)
  
  # Tar reda på vilka månader som faktiskt finns med för det senaste (ofta ofullständiga)
  # året, så att diagramtexten uppdateras automatiskt utan att behöva ändras för hand.
  svenska_manadsnamn <- c("januari", "februari", "mars", "april", "maj", "juni",
                          "juli", "augusti", "september", "oktober", "november", "december")
  
  senaste_ar <- max(regioner_df$Ar)
  senaste_manad_nr <- regioner_df %>%
    filter(Ar == senaste_ar) %>%
    dplyr::pull(Manad) %>%
    as.numeric() %>%
    max()
  senaste_manad_namn <- svenska_manadsnamn[senaste_manad_nr]
  
  # Plockar ut åldersintervallen (t.ex. "16-64 år"/"16-65 år") ur rubrikraderna som
  # lästes in i process_url(), istället för att återge hela rubriktexten ordagrant.
  alla_rubriker <- c(attr(old_df, "rubrik"), attr(new_df, "rubrik"))
  alder_forekomster <- str_extract(alla_rubriker, "\\d{2}-\\d{2} år") %>%
    unique() %>%
    sort()
  alder_forekomster <- alder_forekomster[!is.na(alder_forekomster)]
  
  alder_capt <- if (length(alder_forekomster) >= 2) {
    glue("Inskrivna arbetslösa i procent av arbetskraften. Till och med {cutoff_ar}, {alder_forekomster[1]}, därefter {alder_forekomster[2]}.")
  } else if (length(alder_forekomster) == 1) {
    glue("Inskrivna arbetslösa i procent av arbetskraften ({alder_forekomster[1]}).")
  } else {
    "Inskrivna arbetslösa i procent av arbetskraften."
  }
  
  andel_df <- regioner_df %>% 
    select(c(Ar,Manad,Region,`Inrikes KVI`,`Inrikes MÄN`,`Utrikes KVI`,`Utrikes MÄN`)) %>% 
    pivot_longer(4:7,names_to = "Grupp",values_to = "Arbetslöshet") %>%
    separate(Grupp, c("Grupp", "Kon"), sep = " ", extra = "merge") %>% 
    mutate(Kon = ifelse(Kon == "KVI", "Kvinnor","Män")) %>%
    group_by(Ar,Region,Grupp,Kon) %>% 
    summarize(Arbetslöshet = mean(Arbetslöshet)*100, .groups = "drop") %>% 
    mutate(Grupp = ifelse(Grupp == "Inrikes","Inrikes födda","Utrikes födda"))
  
  rm(old_df,new_df)
  
  if(returnera_data == TRUE){
    assign("arbetslosa_af_tidsserie_df", andel_df, envir = .GlobalEnv)
  }
  
  diagram_capt <- paste(c(
    "Källa: Arbetsförmedlingen.",
    "Bearbetning: Samhällsanalys, Region Dalarna.",
    "Diagramförklaring: Månadsdata. Diagrammet visar årsmedelvärden.",
    alder_capt,
    glue("Data för {senaste_ar} till och med {senaste_manad_namn}")
  ), collapse = "\n")
  
  diagramtitel <- paste0("Arbetslöshet i ",unique(andel_df$Region))
  diagramfilnamn <- paste0("arbetsloshet_tidsserie_bakgrund_",unique(andel_df$Region),".png")
  
  gg_obj <- SkapaStapelDiagram(skickad_df = andel_df, 
                               skickad_x_var = "Ar",
                               skickad_y_var = "Arbetslöshet",
                               skickad_x_grupp = "Kon",
                               manual_x_axis_text_vjust=1,
                               manual_x_axis_text_hjust=1,
                               manual_color = diagramfarger("kon"),
                               diagram_titel = diagramtitel,
                               diagram_capt =  diagram_capt,
                               diagram_facet = TRUE,
                               facet_legend_bottom = TRUE,
                               stodlinjer_avrunda_fem = TRUE,
                               facet_grp = "Grupp",
                               facet_scale = "fixed",
                               x_axis_lutning = 45,
                               manual_y_axis_title = "procent",
                               output_mapp = output_mapp,
                               filnamn_diagram = diagramfilnamn,
                               skriv_till_diagramfil = spara_figur)
  
  gg_list <- c(gg_list, list(gg_obj))
  names(gg_list)[[length(gg_list)]] <- diagramfilnamn %>% str_remove(".png")
  
  return(gg_list)
  
}