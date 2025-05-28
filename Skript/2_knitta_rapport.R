if (!require("pacman")) install.packages("pacman")
p_load(here,
       stringr)

senaste_rmd_filen <- list.files(here(), pattern = "\\.Rmd$") %>% 
  .[which.max(file.info(.)$mtime)]

renderad_html_fil <- senaste_rmd_filen %>% 
  str_replace(".Rmd", ".html")

rmarkdown::render(
    input = senaste_rmd_filen,
    output_file = renderad_html_fil,
    envir = parent.frame()
)