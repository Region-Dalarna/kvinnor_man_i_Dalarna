# Skript som laddar hem data. 
# Källa: Kronofogden

if (!require("pacman")) install.packages("pacman")
p_load(pxweb,openxlsx)

# Laddar in de funktioner som används för att skapa diagram
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

#test_list <- diag_kronofogden(skapa_fil = FALSE)

diag_kronofogden <-function(output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                            spara_data = TRUE,
                            filnamn = "skuldsatta.xlsx"){

  # ======================================================================================================
  
  # Skuldsatta mer än 20 år - https://kronofogden.entryscape.net/catalog/2/datasets/196
  # Uppdaterad i slutat av februari 2024
  langsiktiga_skulder_df <-read.csv2("https://kronofogden.entryscape.net/store/2/resource/201",encoding="latin1")
  
  # Antal personer med skulder under indrivning hos Kronofogden https://kronofogden.entryscape.net/catalog/2/datasets/3
  # Uppdaterad sommaren 2023
  skulder_df <-read.csv2("https://kronofogden.entryscape.net/store/2/resource/27",encoding="latin1")
  
  langsiktiga_skulder_df$Kön <- case_when(
                                langsiktiga_skulder_df$Kön == "Kvinna" ~ "kvinnor",
                                langsiktiga_skulder_df$Kön == "Man" ~ "män")
 
  skulder_df$Kön <- case_when(
                    skulder_df$Kön == "Kvinna" ~ "kvinnor",
                    skulder_df$Kön == "Man" ~ "män")

  
  # Gör om antal personer till en integer
  langsiktiga_skulder_df$Antal.personer <- strtoi(langsiktiga_skulder_df$Antal.personer, base=0L)
  
  # Gör om årtal till en character
  langsiktiga_skulder_df$År <- as.character(langsiktiga_skulder_df$År)
  
  skulder_df$År <- as.character(skulder_df$År)
  
  # Sparar data till Excel
  if (spara_data==TRUE){
    flik_lista=lst("Långsiktigt skuldsatta" = langsiktiga_skulder_df,"Skuldsatta" = skulder_df)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  }

}
