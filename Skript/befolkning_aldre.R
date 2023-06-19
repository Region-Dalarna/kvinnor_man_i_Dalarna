if (!require("pacman")) install.packages("pacman")
p_load(pxweb,openxlsx)

source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)


#test_list=data_befolkning_alder()
data_befolkning_alder <- function(spara_data = TRUE,
                                  filnamn = "andel_aldre.xlsx",
                                  output_mapp = "G:/skript/jon/Slask/"){
  
  # ========================================== Inställningar ============================================
  # "Adresser" till SCBs databas
  url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"

  contents_code=c("BE0101N1")
 
  # Variabler som skall tas ut
  varlista <-  list("Region"=hamtaAllaLan(tamedriket = TRUE),
                    "Kon"=c("1","2"),
                    "Alder"= c("*"),
                    "ContentsCode"="BE0101N1",
                    "Tid"=max(hamta_giltiga_varden_fran_tabell(url, "tid")))
  
  # Uttag av data
  px_uttag <- pxweb_get(url = url,query = varlista)
  
  # Konverterar data till en Data Frame
  befolkning_df <- as.data.frame(px_uttag, column.name.type = "text", variable.value.type = "text")
  
  # Städar dataset
  # Tar bort värden efter årtal
  befolkning_df$ålder <- word(befolkning_df$ålder,1)
  
  # Data skall inte vara total. Dessutom görs 100+ om till 100
  befolkning_df <-befolkning_df %>% 
    filter(ålder != "totalt") %>% 
      mutate(ålder=ifelse(ålder=="100+","100",ålder))
  
  # Sparar data till Excel
  
  if (spara_data==TRUE){
    flik_lista=lst("Andel äldre"= befolkning_df)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  }
  
}

