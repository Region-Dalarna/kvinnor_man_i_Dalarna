## =================================================================================================================
# Skript som laddar hem data för ohälsotalet respektive sjukpenningtalet.
# För ytterligare information, se text i början av funktionen hamta_data_fk
# =================================================================================================================
# Läser in nödvändiga bibliotek med pacman
if (!require("pacman")) install.packages("pacman")
p_load(openxlsx)

# Funktioner som behövs
#source("https://raw.githubusercontent.com/JonFrank81/funktioner_alternativ/main/hamta_data.R")
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

#test_list <- diag_chefer(skapa_fil = FALSE)

diag_ohalsa<-function(region_vekt = "20", 
                      output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                      filnamn = "ohalsotal_sjukpenningtal.xlsx",
                      spara_data = TRUE){

  # =============================================== Uttag ===============================================
  # Adresser till data
  #path = c("https://www.forsakringskassan.se/fk_apps/MEKAREST/public/v1/ohm-ohalsotal/SJPohttal.xlsx","https://www.forsakringskassan.se/fk_apps/MEKAREST/public/v1/ohm-sjptal/SJPsjptal.xlsx")
  path = c("https://www.forsakringskassan.se/api/sprstatistikrapportera/public/v1/ohm-ohalsotal/SJPohttal.xlsx","https://www.forsakringskassan.se/api/sprstatistikrapportera/public/v1/ohm-sjptal/SJPsjptal.xlsx")
  
  # https://www.forsakringskassan.se/fk_apps/MEKAREST/public/v1/ohm-ohalsotal/SJPohttal.xlsx
  # Tidigare
  # Anropar funktionen hamta_data_FK som hämtar data från öppna data på Försäkringskassan och returnerar en lista.
  #flik_lista <- hamta_data_FK(path, c("Ohälsotalet","Sjukpenningtalet"),region_vekt)
  
  flik_lista = list()
  
  ohalsa_lista = hamta_excel_dataset_med_url(path[1],skippa_rader = 2) 
  
  # Eftersom dataset är så stort returneras det med en lista. Här slås dessa ihop till en dataframe.
  j=1
  ohalsa_df=c()
  while(j<=length(ohalsa_lista)){
    ohalsa_df <- rbind(ohalsa_df,ohalsa_lista[[j]])
    j=j+1
  }
  
  ohalsa_df <- ohalsa_df %>% 
    filter(substr(Län,1,2) %in% region_vekt) %>% 
      select(-kolumnnamn)
  
  flik_lista[[1]] = ohalsa_df
  
  sjp_lista = hamta_excel_dataset_med_url(path[2],skippa_rader = 2) 
  
  j=1
  sjp_df=c()
  while(j<=length(sjp_lista)){
    sjp_df <- rbind(sjp_df,sjp_lista[[j]])
    j=j+1
  }
  
  sjp_df <- sjp_df %>% 
    filter(substr(Län,1,2) %in% region_vekt) %>% 
      select(-kolumnnamn)
  
  flik_lista[[2]] = sjp_df
  
  names(flik_lista) = c("Ohälsotalet","Sjukpenningtalet")
  
  
  # flik_lista = lst()
  # i=1
  # 
  # # Uttag av data. Eftersom det är en väldigt stor datamängd delas den upp i två flikar (av Försäkringskassan), varför lapply används,
  # while(i <= length(path)){
  #   
  #   td = tempdir()              # skapa temporär mapp
  #   varsel_fil <- tempfile(tmpdir=td, fileext = ".xlsx")
  #   download.file(path[[i]], destfile = varsel_fil, mode = "wb")       # ladda hem hela filen, mode = "wb" viktigt, annars blir det fel
  #   
  #   lista = lapply(getSheetNames(varsel_fil), function(x) import(file=path[[i]],which = x) %>% 
  #                                        filter(!row_number() %in% c(0, 1)) %>% 
  #                                         row_to_names(1) %>% 
  #                                           filter(substr(Län,1,2) == region_vekt))
  #   
  #   # Binder ihop data från de olika flikarna i Excelfilen
  #   j=1
  #   df=c()
  #   while(j<=length(lista)){
  #     df <- rbind(df,lista[[j]])
  #     j=j+1
  #   }
  #   
  #   flik_lista[[i]] <- df
  # 
  #  i=i+1
  # }
  # 
  # names(flik_lista) <- c("Ohälsotalet","Sjukpenningtalet")
  
  # Sparar data till Excel
  if (spara_data==TRUE){
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  }
}
