data_yrke<-function(region_vekt = "20", 
                    output_mapp = "G:/skript/jon/Slask/",
                    filnamn = "yrke.xlsx",
                    spara_data = TRUE){
  
  ## =================================================================================================================
  # Skript som skapar diagram för störst antal anställa med ett visst yrke i en region.
  # Könsuppdelat, finns både som facet och inte facet
  # Uppdaterat 2024-11-11: Jon, har uppdaterat så att data hämtas via ett nytt skript
  # =================================================================================================================
  if (!require("pacman")) install.packages("pacman")
  p_load(pxweb,
         openxlsx)
  
  source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")
  source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/refs/heads/main/hamta_yrke_SSYK4_region_yrke2012_kon_tid_YREG60BAS_YREG60N_YREG60_scb.R")
  # =============================================== API-uttag ===============================================
  # "Adresser" till SCBs databas
  #url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/AM/AM0208/AM0208M/YREG60N"
  
  # Av någon oklar anledning tas även 2018 med när vi hämtar data för sista år. Hamta-data skript behöver nog kollas upp.
  yrke_df <- hamta_yrke_SSYK4_region_yrke2012_kon_tid_scb(region_vekt = region_vekt,
                                                              yrke2012_klartext = "*",
                                                              tid_koder = "9999") %>% 
    filter(år == max(år)) %>% 
      select(-`Anställda (yrkesregistret) 16-64 år med arbetsplats i regionen (dagbef)`) %>% 
        rename(Antal = `Anställda (yrkesregistret) med arbetsplats i regionen (dagbef)`,
               Yrke = `Yrke (SSYK 2012)`)
  
  # # Variabler som skall tas ut
  # varlista <-  list(Region = c(region_vekt),
  #                   Yrke2012 = c("*") ,
  #                   Kon=c("1","2"),
  #                   ContentsCode=c("000005G2"),
  #                   Tid=max(hamta_giltiga_varden_fran_tabell(url, "tid")))
  # 
  # # Uttag av data
  # yrke_df <- pxweb_get(url = url,query = varlista) %>% 
  #   as.data.frame(., column.name.type = "text", variable.value.type = "text") %>% 
  #   rename("Yrke" = `Yrke (SSYK 2012)`,
  #          "Antal" = `Anställda (yrkesregistret) 16-64 år med arbetsplats i regionen (dagbef)`) 
  
  
  # Sparar data till Excel
  if (spara_data==TRUE){
    flik_lista=lst("yrke"= yrke_df)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  }
  
}