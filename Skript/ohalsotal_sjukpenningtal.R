## =================================================================================================================
# Skript som laddar hem data för ohälsotalet respektive sjukpenningtalet. 
# Källa: https://www.dataportal.se/sv/datasets?p=1&q=&s=2&t=20&f=&rt=dataset%24esterms_IndependentDataService%24esterms_ServedByDataService&c=false
# Välj organisation: Försäkringskassan
# =================================================================================================================
# Läser in nödvändiga bibliotek med pacman
if (!require("pacman")) install.packages("pacman")
p_load(janitor,
       keyring,
       httr,
       rKolada,
       openxlsx,
       rio,
       tidyverse)

# Funktioner som behövs
source("https://raw.githubusercontent.com/FaluPeppe/func/main/func_API.R")

#test_list <- diag_chefer(skapa_fil = FALSE)

diag_ohalsa<-function(region_vekt = "20", 
                      output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                      filnamn = "ohalsotal_sjukpenningtal.xlsx",
                      spara_data = TRUE){

  # =============================================== Uttag ===============================================
  # Adresser till data
  path = c("https://www.forsakringskassan.se/fk_apps/MEKAREST/public/v1/ohm-ohalsotal/SJPohttal.xlsx","https://www.forsakringskassan.se/fk_apps/MEKAREST/public/v1/ohm-sjptal/SJPsjptal.xlsx")
  
  flik_lista = lst()
  i=1
  
  # Uttag av data. Eftersom det är en väldigt stor datamängd delas den upp i två flikar (av Försäkringskassan), varför lapply används,
  while(i <= length(path)){
    
    td = tempdir()              # skapa temporär mapp
    varsel_fil <- tempfile(tmpdir=td, fileext = ".xlsx")
    download.file(path[[i]], destfile = varsel_fil, mode = "wb")       # ladda hem hela filen, mode = "wb" viktigt, annars blir det fel
    
    lista = lapply(getSheetNames(varsel_fil), function(x) import(file=path[[i]],which = x) %>% 
                                         filter(!row_number() %in% c(0, 1)) %>% 
                                          row_to_names(1) %>% 
                                            filter(substr(Län,1,2) == region_vekt))
    
    # Binder ihop data från de olika flikarna i Excelfilen
    j=1
    df=c()
    while(j<=length(lista)){
      df <- rbind(df,lista[[j]])
      j=j+1
    }
    
    flik_lista[[i]] <- df

   i=i+1
  }
  
  names(flik_lista) <- c("Ohälsotalet","Sjukpenningtalet")
  
  # Sparar data till Excel
  if (spara_data==TRUE){
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  }
}
