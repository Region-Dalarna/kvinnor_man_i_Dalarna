
library(tidyverse)
library(stringi)

diag_bra_ntu_kv_man <- function(sokord = c("utsatthet för misshandel", "utsatthet för sexualbrott",
                                           "utsatthet för hot", 
                                            "oro för att utsättas för misshandel", 
                                            "oro för att utsättas för våldtäkt", 
                                           "Otrygghet vid utevistelse sent på",
                                           "Avstått från någon aktivitet")) {
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R", encoding = "utf-8", echo = FALSE)
  source("https://raw.githubusercontent.com/Analytikernatverket/hamta_data/main/ntu_lan_bra.R")
  
  diag_capt <- "Källa: Nationella Trygghetsundersökningen (NTU), Brottsförebyggande rådet (BRÅ)\nBearbetning: Samhällsanalys, Region Dalarna"
  
  ntu_lista <- hamta_brott_ntu_lan_bra()
  
  valda_ntu <- ntu_lista[
    str_detect(
      names(ntu_lista),
      regex(paste(sokord, collapse = "|"), ignore_case = TRUE)
    ) &
      !str_detect(
        names(ntu_lista),
        regex("genom", ignore_case = TRUE)
      )
  ]
  
  diag_list_ntu_bra <- imap(valda_ntu, ~ {
    
    diagram_namn <- paste0(sub(",.*", "", .y) %>% 
                             str_remove(" \\(1\\)") %>%
                             str_remove("\\(%\\) ") %>%
                             str_remove(", enligt NTU \\d{4}–\\d{4}") %>%
                             str_remove("\\d{4}–\\d{4}")) %>%      # extrahera datasetnamn från värdet i rad 1 kolumn 1
      str_trim()
    
    dataset_df <- .x %>% filter(!str_detect(ar, "intervall"),
                                str_detect(kon, "Män|Kvinnor"),
                                region %in% "Dalarnas län")
    
    gg_obj <- SkapaStapelDiagram(
      skickad_df = dataset_df, 
      skickad_x_var = "ar",
      skickad_y_var = "varde",
      skickad_x_grupp = "kon",
      diagram_titel = paste0(sub(", av samtliga.*", "", .y) %>% 
                               str_remove_all(" \\(1\\)") %>%
                               str_remove(", enligt NTU \\d{4}–\\d{4}") %>%
                               str_remove(" \\d{4}–\\d{4}") %>%
                               str_remove_all("\\(%\\) ") %>%
                               str_replace("Självrapporterad utsatthet", "Självrapporterad utsatthet")
                             , " i Dalarnas län"),
      filnamn_diagram = paste0(diagram_namn, ".png"),
      manual_color = diagramfarger("kon"),
      skriv_till_diagramfil = FALSE,
      stodlinjer_avrunda_fem = TRUE,
      output_mapp = utskriftsmapp(),
      diagram_capt = diag_capt,
      x_axis_lutning = 0,
      manual_y_axis_title = "procent"
    )
    
    set_names(list(gg_obj), diagram_namn)        # returnera namngiven lista där datasetnamnet från ovan används
    
  }) %>%
    purrr::flatten()                               # kör en flatten så att vi får en lista med dataframes som är namngivna
  return(diag_list_ntu_bra)
}
