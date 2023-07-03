if (!require("pacman")) install.packages("pacman")
p_load(pxweb,
       openxlsx)

source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")


#test_list=data_befolkning_alder(output_mapp="G:/skript/projekt/data/kvinnor_man/")
data_befolkning_alder <- function(region_vekt = "20",
                                  spara_data = TRUE,
                                  filnamn = "befolkning.xlsx",
                                  output_mapp = "G:/skript/jon/Slask/"){
  
  # ========================================== Inställningar ============================================
  # "Adresser" till SCBs databas
  url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/BE/BE0101/BE0101A/BefolkningNy"
  contents_code=c("BE0101N1")
 
  # Variabler som skall tas ut
  # Alla län men senaste år
  varlista_senaste_ar <-  list("Region"=hamtaAllaLan(tamedriket = TRUE),
                              "Kon"=c("1","2"),
                              "Alder"= c("*"),
                              "ContentsCode"="BE0101N1",
                              "Tid"=max(hamta_giltiga_varden_fran_tabell(url, "tid")))
  
  # Dalarna och riket, men alla år
  varlista_alla_ar <-  list("Region"=c("00",region_vekt),
                               "Kon"=c("1","2"),
                               "Alder"= c("*"),
                               "ContentsCode"="BE0101N1",
                               "Tid"=c("*"))
  
  varlista=lst(varlista_senaste_ar,varlista_alla_ar)          
  lista <- list()
  i=1
  
  while(i<=length(varlista)){
    # Uttag av data
    lista[[i]] <- pxweb_get(url = url,query = varlista[[i]]) %>%
      as.data.frame(., column.name.type = "text", variable.value.type = "text")
    
    # Städar dataset
    lista[[i]] <- lista[[i]] %>%
      filter(ålder != "totalt ålder") %>% 
        mutate(ålder = word(ålder,1)) %>% 
          mutate(ålder=ifelse(ålder=="100+","100",ålder))
    
    #lista[[i]]$ålder <- word(lista[[i]]$ålder,1)
    
    # Data skall inte vara total. Dessutom görs 100+ om till 100
    # lista[[i]] <-lista[[i]] %>% 
    #   filter(ålder != "totalt") %>% 
    #     mutate(ålder=ifelse(ålder=="100+","100",ålder))
    
    
    i=i+1
  }
  names(lista) <- c("befolkning_län","befolkning_förändring")
  # Sparar data till Excel
  
  if (spara_data==TRUE){
    openxlsx::write.xlsx(lista,paste0(output_mapp,filnamn))
  }
  
}

