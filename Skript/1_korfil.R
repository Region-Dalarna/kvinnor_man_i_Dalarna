output_mapp_figur = "Diagram/"

source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diagram_matchning_lan_bakgrund.R")
gg_matchning <- diag_matchning_lan(region_vekt = "20",
                                   spara_figur = TRUE,
                                   returnera_data = TRUE,
                                   kon_klartext = "*")


rmarkdown::render(
  input = 'kvinnor_man_markdown_ny.Rmd',
  output_file = 'kvinnor_man_markdown_ny.html',
  envir = parent.frame()
)
