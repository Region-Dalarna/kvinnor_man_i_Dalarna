# Hämtar data för Utbildningsgrupper från 1985 och framåt
if (!require("pacman")) install.packages("pacman")
p_load(pxweb,openxlsx)

# Skript som behövs
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

#test_list=data_utbniva_85(skapa_fil=FALSE)
data_utbniva_85 <- function(region_vekt="20",
                            spara_data = TRUE,
                            output_mapp = "G:/skript/jon/Slask/",
                            filnamn = "utbildningsniva_85.xlsx"){
  
  # ========================================== Läser in data ============================================
  # Skapa en lista med information som vi vill ha hem -- skapas lättast via pxweb_interactive()
  pxweb_query_list <- 
    list("Region" = region_vekt,
         "Kon"  =c("*"),
         "Alder" = c(as.character(25:64)),
         "UtbildningsNiva" = c("*"),
         "ContentsCode" = c("UF0506A1"),
         "Tid" = c("*"))
  
  # Download data 
  px_data <- 
    pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/UF/UF0506/UF0506B/Utbildning",
              query = pxweb_query_list)
  
  # Convert to data.frame 
  px_df <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
  
  px_df$utb_niva <- case_when(
    px_df$utbildningsnivå == "förgymnasial utbildning kortare än 9 år" ~ "Förgymnasial utbildning",
    px_df$utbildningsnivå == "förgymnasial utbildning, 9 (10) år" ~ "Förgymnasial utbildning",
    px_df$utbildningsnivå == "gymnasial utbildning, högst 2 år" ~ "Gymnasial utbildning",
    px_df$utbildningsnivå == "gymnasial utbildning, 3 år" ~ "Gymnasial utbildning",
    px_df$utbildningsnivå == "eftergymnasial utbildning, mindre än 3 år" ~ "Eftergymnasial utbildning, mindre än 3 år",
    px_df$utbildningsnivå == "eftergymnasial utbildning, 3 år eller mer"~ "Eftergymnasial utbildning, 3 år eller mer",
    px_df$utbildningsnivå == "forskarutbildning" ~ "Eftergymnasial utbildning, 3 år eller mer",
    px_df$utbildningsnivå == "uppgift om utbildningsnivå saknas" ~ "Uppgift saknas")
  
  if (spara_data==TRUE){
    flik_lista=lst("Utbildningsnivå_85"= px_df)
    openxlsx::write.xlsx(px_df,paste0(output_mapp,filnamn))
  }
  
}
