## =================================================================================================================
# Skript som laddar hem data för ledamöter i riksdag och fullmäktige från SCB
# =================================================================================================================
# Läser in nödvändiga bibliotek med pacman
if (!require("pacman")) install.packages("pacman")
p_load(pxweb,
       openxlsx)

# Funktioner som behövs
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

#test_list <- diag_val(skapa_fil = FALSE)

diag_val<-function(region_vekt = "20", 
                   output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                   spara_data = TRUE,
                   filnamn = "val.xlsx"){
  
   # =============================================== API-uttag ===============================================
  # Riksdag /ME0107C/Riksdagsledamoter, "ME0107A6"
  # Landsting /ME0107B/Ltledamoter, "ME0107A5"
  # Kommunfullmäktige /ME0107A/Kfledamoter ,"ME0107A4"
  
  # Skapar två vektorer som används för att ploca ut data via pxweb (i en loop)
  url_vec <- c("/ME0107C/Riksdagsledamoter","/ME0107B/Ltledamoter","/ME0107A/Kfledamoter")
  contentscode_vec <- c( "ME0107A6","ME0107A5","ME0107A4")
  
  # Funkar i praktiken bara för Dalarnas län eftersom vissa koder skrivs LG och vissa L. Oklart varför
  # Skapar en lista med regioner som skall anropas. För riksdag och region tas alla ut medan vara kommuner för valt län tas ut i sista fallet
  regioner <- lst("*",paste0(region_vekt,"LG"),hamtakommuner(lan=region_vekt,tamedlan=FALSE,tamedriket=FALSE,allakommuner=FALSE))
  
  # "Adresser" till SCBs databas
  url1 <- "https://api.scb.se"
  url2 <- "/OV0104/v1/doris/sv/ssd/ME/ME0107"
  
  for(val in 1:length(url_vec) ){
    
    url3 <-paste0(url1,url2,url_vec[val])
    
    # Variabler som skall tas ut
    varlista <-  list("Region"=c(regioner[[val]]),
                      "Parti"=c("*"),
                      "Kon"=c("1","2"),
                      "ContentsCode"=c(contentscode_vec[val]),
                      "Tid"=c("*"))
    px_uttag <- pxweb_get(url = url3,query = varlista)
    
    if (val==1) 
    {
      riksdagsval_df <-as.data.frame(px_uttag, column.name.type = "text", variable.value.type = "text")
    } else if (val==2){
      regionval_df <-as.data.frame(px_uttag, column.name.type = "text", variable.value.type = "text")
    } else {
      kommunval_df <- as.data.frame(px_uttag, column.name.type = "text", variable.value.type = "text")
    }
    
  }
  
  # Sparar data till Excel
  if (spara_data==TRUE){
    flik_lista=lst("Riksdagsval" = riksdagsval_df,"Regionval" = regionval_df, "Kommunval" = kommunval_df)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  }
 
}

