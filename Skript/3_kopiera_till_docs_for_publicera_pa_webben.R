if (!require("pacman")) install.packages("pacman")
p_load(here)

senaste_html_filen <- list.files(here(), pattern = "\\.html$") %>% 
  .[which.max(file.info(.)$mtime)]

senaste_rmd_filen <- list.files(here(), pattern = "\\.Rmd$") %>% 
  .[which.max(file.info(.)$mtime)]

# kopiera html-filen till 
file.copy(from = here(senaste_html_filen), to = paste0(here(), "/docs/index.html"), overwrite = TRUE)
file.copy(from = here(senaste_rmd_filen), to = paste0(here(), "/docs/index.Rmd"), overwrite = TRUE)
