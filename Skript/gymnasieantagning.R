# Skript som anropar Peters funktion för att ladda hem data för gymnasieantagning och sparar till Excel
pacman::p_load(tidyverse,
               pdftools)

source("G:/skript/hamta_data/func_gymnasieantagningen.R", encoding = "utf-8", echo = FALSE)
#source("G:/skript/func/func_filer.R", encoding = "utf-8", echo = FALSE)

#test_lista <-diag_gymnasieantagning(skapa_fil = FALSE)

diag_gymnasieantagning <-function(region_vekt = "20", 
                                        output_mapp = "G:/skript/jon/Slask/",
                                        filnamn = "gymnasieantagning.xlsx",
                                        spara_data = TRUE){

  # =========================== läs in gymnasiedata från Gymnasieantagningen =========================
  gymnant_df <- las_in_data_gymnasieantagningen()
  
  gymnant_sum_Dal <- gymnant_df %>% 
    group_by(ar,program) %>% 
    summarize(Män=sum(Ant_Män),
              Kvinnor=sum(Ant_Kv))
  
  # Pivoterar data för att enkelt kunna dela upp på kvinnor och män
  gymnant_sum_Dal<-pivot_longer(gymnant_sum_Dal,3:4,names_to="Kon",values_to = "Antagna")
  
  # Ändrar ett av utbildningsnamnen
  gymnant_sum_Dal[gymnant_sum_Dal == "Flygteknikutbildningen, Marinteknikutbildningen, Sjöfartsutbildningen, Tågteknikutbildningen, Utbildningen samiska näringar eller Yrkesdansarutbildningen"] <- "Flygteknikutbildningen mfl."
  
  if (spara_data==TRUE){
    flik_lista=lst("gymnasieantagning"= gymnant_sum_Dal)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  }

}
