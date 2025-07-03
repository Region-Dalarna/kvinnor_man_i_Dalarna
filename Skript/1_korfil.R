if (!require("pacman")) install.packages("pacman")
p_load(here)

output_mapp_figur = here("Diagram","/")

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")


# De tio största yrkena för män respektive kvinnor
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diag_yrke_ssyk3_storsta_per_geografi.R")
yrken_konsfordelning <- diag_storsta_yrke_per_geografi(facet_ovanpa_varandra = TRUE, storre_text = TRUE,returnera_dataframe_global_environment = TRUE,antal_yrken = 10)

yrke_storst_kvinnor <- storsta_yrken_per_geografi_df %>% filter(kön == "kvinnor",Antal == max(Antal)) %>% .$yrke
yrke_storst_kvinnor_antal <- storsta_yrken_per_geografi_df %>% filter(kön == "kvinnor",Antal == max(Antal)) %>% .$Antal
yrke_nast_storst_kvinnor <- tolower(storsta_yrken_per_geografi_df %>% filter(kön == "kvinnor", yrke != yrke_storst_kvinnor) %>% filter(Antal == max(Antal))  %>%  .$yrke)

yrke_storst_man <- storsta_yrken_per_geografi_df %>% filter(kön == "män") %>% filter(Antal == max(Antal)) %>% .$yrke
yrke_storst_man_antal <- storsta_yrken_per_geografi_df %>% filter(kön == "män") %>% filter(Antal == max(Antal)) %>% .$Antal
yrke_nast_storst_man <- tolower(storsta_yrken_per_geografi_df %>% filter(kön == "män", yrke != yrke_storst_man) %>% filter(Antal == max(Antal))  %>%  .$yrke)

butikspersonal_kvinnor <- format(plyr::round_any(storsta_yrken_per_geografi_df %>% filter(kön == "kvinnor",yrke == "Butikspersonal") %>% .$Antal,100),big.mark=" ")
butikspersonal_man <- format(plyr::round_any(storsta_yrken_per_geografi_df %>% filter(kön == "män",yrke == "Butikspersonal") %>% .$Antal,100),big.mark=" ")

tio_storsta_sum_kvinnor <- format(plyr::round_any(storsta_yrken_per_geografi_df %>% filter(kön == "kvinnor") %>%  .$Antal %>% sum(),100),big.mark=" ")
tio_storsta_sum_man <- format(plyr::round_any(storsta_yrken_per_geografi_df %>% filter(kön == "män") %>%  .$Antal %>% sum(),100),big.mark=" ")

# Matchning
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diagram_matchning_lan_bakgrund.R")
gg_matchning <- diag_matchning_lan(region_vekt = "20",
                                   output_mapp_figur = output_mapp_figur,
                                   spara_figur = TRUE,
                                   returnera_data = TRUE,
                                   kon_klartext = "*")

# VaB
source(here("Skript","fp_vab_manad.R"))
gg_forsakringskassan <- diag_foraldrapenning_manad(region_vekt = "20",
                                                   output_mapp = output_mapp_figur,
                                                   diag_foraldrapenning = TRUE,
                                                   diag_vab = FALSE,
                                                   spara_diagrambildfil = TRUE,
                                                   spara_dataframe_till_global_environment = TRUE)

# source(here("Skript","diagram_arbesloshet_tidsserie_BAS.R"))
# gg_arbetsloshet_tidsserie_bas <- diag_arbetsloshet_tidsserie(spara_diagrambildfil = FALSE,
#                                                              spara_dataframe_till_global_environment = FALSE,
#                                                              output_mapp = output_mapp_figur)

# Hämtar data för långtidsarbetslöshet
långtidsarbetslöshet <- hamta_kolada_df(kpi_id = c("N03926"),
                                        valda_kommuner = "20",
                                        valda_ar = 2011:2100,
                                        konsuppdelat = TRUE) %>% 
  mutate(kon = tolower(kon))

langtidsarbetsloshet_ar_min = långtidsarbetslöshet$ar %>% min()
langtidsarbetsloshet_ar_max = långtidsarbetslöshet$ar %>% max()
langtidsarbetsloshet_kvinnor_min = gsub("\\.",",",round(långtidsarbetslöshet %>% filter(kon=="kvinnor",ar==min(ar)) %>%  .$varde,1))
langtidsarbetsloshet_kvinnor_max = gsub("\\.",",",round(långtidsarbetslöshet %>% filter(kon=="kvinnor",ar==max(ar)) %>%  .$varde,1))
langtidsarbetsloshet_man_min = gsub("\\.",",",round(långtidsarbetslöshet %>% filter(kon=="män",ar==min(ar)) %>%  .$varde,1))
langtidsarbetsloshet_man_max = gsub("\\.",",",round(långtidsarbetslöshet %>% filter(kon=="män",ar==max(ar)) %>%  .$varde,1))

# # Överrepresentation av kvinnor
source("https://raw.githubusercontent.com/Region-Dalarna/socioekonomisk_analys_nms/refs/heads/main/skript/socioek_overrep.R")
gg_overrep = skapa_overrep_diagram(spara_diagrambildfil = TRUE,
                             mapp = here("Diagram/") %>% paste0(., "/"),
                             returnera_dataframe_global_environment = TRUE,
                             diagramtitel = "Överrepresentation av manliga chefer i Dalarna",
                             ta_bort_titel = FALSE,
                             ta_bort_caption = FALSE)

overrep_max_ar = overrep_df$Ar %>% max()
overrep_min_ar = overrep_df$Ar %>% min()
# test = skapa_overrep_diagram(spara_diagrambildfil = FALSE,
#                              diag_fargvekt = NA, # För diagrammet som inte är könsuppdelat
#                              returnera_dataframe_global_environment = TRUE,
#                              diagramtitel = "Överrepresentation av manliga chefer i Dalarna",
#                              ta_bort_titel = FALSE,
#                              ta_bort_caption = FALSE)

# # ============= Överrepresentation av chefer - motsvarar diagram 32 i den tidigare rapporten
# source(here("skript","socioek_overrep.R"), encoding="UTF-8")
# overrepresentation_bransch <- funktion_upprepa_forsok_om_fel( function() {
#   skapa_overrep_diagram()
# }, hoppa_over = hoppa_over_felhantering)
# 
# overrep_ar_min <- overrep_df$Ar %>% min()
# overrep_ar_max <- overrep_df$Ar %>% max()


rmarkdown::render(
  input = 'kvinnor_man_markdown_ny.Rmd',
  output_file = 'kvinnor_man.html',
  envir = parent.frame()
)
