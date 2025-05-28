if (!require("pacman")) install.packages("pacman")
p_load(stringr,
       here)

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R", encoding = "utf-8", echo = FALSE)

repo_namn <- list.files(here()) %>% .[str_detect(., ".Rproj")] %>% str_remove(".Rproj")

datum_tid <- format(Sys.time(), "%d %b %Y kl. %H:%M")

github_commit_push(repo = repo_namn, 
                   fran_rmarkdown = TRUE,
                   commit_txt = paste0("Automatisk commit och push av repositoryt till github ", datum_tid))
