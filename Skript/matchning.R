# Matchning på arbetsmarknaden. Från reginonala matchningsindikatorer (val 24 under arbetsmarknad)
# https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AM__AM9906__AM9906A/RegionInd19M2N/
# Senast uppdaterad 2023-06-28 (på SCBs hemsida)

# Läser in nödvändiga bibliotek med pacman
if (!require("pacman")) install.packages("pacman")
p_load(pxweb,
       openxlsx)

#test_list=diag_matchning(skapa_fil=FALSE)
diag_matchning <- function(output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                           spara_data = TRUE,
                           filnamn = "matchning.xlsx" ){

  # ========================================== Läser in data ============================================
  # Skapa en lista med information som vi vill ha hem -- skapas lättast via pxweb_interactive()
  
  url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/AM/AM9906/AM9906A/RegionInd19M2N"
  
  pxvardelist(url, "contentscode")  
  
  pxweb_query_list <- 
    list("Region"=hamtaAllaLan(tamedriket = TRUE),
         "Kon"=c("1","2","SAMANST"),
         "AlderFodelselandgr"=c("Sverige","Norden/EU","Afrika","Asien","Övriga_världen","totalt"),
         "ContentsCode"=c("000005SF"),
         "Tid"=max(hamta_giltiga_varden_fran_tabell(url,"tid")))
  
  # Download data 
  matchning_df <- pxweb_get(url = url,
              query = pxweb_query_list) %>%
      as.data.frame(column.name.type = "text", variable.value.type = "text") %>% 
        rename("fodelseland"="ålder/födelselandgrupp","matchningsgrad"="Matchningsgrad, procent ") %>% 
          mutate(region = skapa_kortnamn_lan(region,byt_ut_riket_mot_sverige = TRUE))
  
  # Sparar data till Excel
  if (spara_data==TRUE){
    flik_lista=lst("matchning"= matchning_df)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  }

}
