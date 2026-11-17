if (!require("pacman")) install.packages("pacman")
p_load(tidyverse,
       here)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/refs/heads/main/func_API.R")

senaste_html_filen <- list.files(here(), pattern = "\\.html$") %>% 
  .[which.max(file.info(.)$mtime)]

senaste_rmd_filen <- list.files(here(), pattern = "\\.Rmd$") %>% 
  .[which.max(file.info(.)$mtime)] 

# kopiera html-filen till 
file.copy(from = here(senaste_html_filen), to = paste0(here(), "/docs/index.html"), overwrite = TRUE)
file.copy(from = here(senaste_rmd_filen), to = paste0(here(), "/docs/index.Rmd"), overwrite = TRUE)

# kopiera html-filen till publicera rapporter och committa projektet
sokvag_lokal_repo <- "c:/gh/"
file.copy(from = here(senaste_html_filen), to = paste0(sokvag_lokal_repo, "publicera_rapporter/docs/"), overwrite = TRUE)

# commit + push till publicera rapporter
#github_commit_push(repo = "publicera_rapporter", commit_txt = "Publicera rapport")

# Publcerar till Shiny-servern

if (!requireNamespace("rddeploy", quietly = TRUE)) {
  remotes::install_github("Region-Dalarna/rdpaket", subdir = "packages/rddeploy")
}

rddeploy::analysrapport_publicera("kvinnor_man_i_Dalarna")
