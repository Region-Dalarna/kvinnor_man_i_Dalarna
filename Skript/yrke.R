## =================================================================================================================
# Skript som laddar hem data för förvärvsarbetande, bransch från SCB
# https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM0208__AM0208M/YREG60N/
# =================================================================================================================
if (!require("pacman")) install.packages("pacman")
p_load(pxweb,
       openxlsx)

source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")

#test_list <- data_yrke(skapa_fil = TRUE)

data_yrke<-function(region_vekt = "20", 
                                         output_mapp = "G:/skript/jon/Slask/",
                                         filnamn = "yrke.xlsx",
                                         spara_data = TRUE){
  
  # =============================================== API-uttag ===============================================
  # "Adresser" till SCBs databas
  url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0208/AM0208M/YREG60N"
  
  # Variabler som skall tas ut
  varlista <-  list(Region = c(region_vekt),
                    Yrke2012 = c("*") ,
                    Kon=c("1","2"),
                    ContentsCode=c("000005G2"),
                    Tid=max(hamta_giltiga_varden_fran_tabell(url, "tid")))
  
  # Uttag av data
 yrke_df <- pxweb_get(url = url,query = varlista) %>% 
    as.data.frame(., column.name.type = "text", variable.value.type = "text") %>% 
      rename("Yrke" = `Yrke (SSYK 2012)`,
             "Antal" = `Anställda (yrkesregistret) 16-64 år med arbetsplats i regionen (dagbef)`) 

  
  # Sparar data till Excel
  if (spara_data==TRUE){
    flik_lista=lst("yrke"= yrke_df)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  }
  
}
