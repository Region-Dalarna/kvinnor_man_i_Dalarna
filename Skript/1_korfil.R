if (!require("pacman")) install.packages("pacman")
p_load(here)

output_mapp_figur = here("Diagram","/")

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

source(here("Skript","diagram_arbesloshet_tidsserie_BAS.R"))
gg_arbetsloshet_tidsserie_bas <- diag_arbetsloshet_tidsserie(spara_diagrambildfil = FALSE,
                                                             spara_dataframe_till_global_environment = FALSE,
                                                             output_mapp = output_mapp_figur)


rmarkdown::render(
  input = 'kvinnor_man_markdown_ny.Rmd',
  output_file = 'kvinnor_man_markdown_ny.html',
  envir = parent.frame()
)
