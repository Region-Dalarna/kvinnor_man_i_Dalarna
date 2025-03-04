if (!require("pacman")) install.packages("pacman")
p_load(here)

output_mapp_figur = here("Diagram","/")

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diagram_matchning_lan_bakgrund.R")
gg_matchning <- diag_matchning_lan(region_vekt = "20",
                                   output_mapp_figur = output_mapp_figur,
                                   spara_figur = TRUE,
                                   returnera_data = TRUE,
                                   kon_klartext = "*")

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
# source("https://raw.githubusercontent.com/Region-Dalarna/socioekonomisk_analys_nms/refs/heads/main/skript/socioek_overrep.R")
# gg_overrep = skapa_overrep_diagram(spara_diagrambildfil = TRUE,
#                              mapp = here("Diagram/") %>% paste0(., "/"))

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
  output_file = 'kvinnor_man_markdown_ny.html',
  envir = parent.frame()
)
