# Hämtar data för Utbildningsgrupper från 1985 och framåt
if (!require("pacman")) install.packages("pacman")
p_load(pxweb,openxlsx)

# Skript som behövs
source("G:/skript/func/func_API.R", encoding = "utf-8", echo = FALSE)

#test_list=data_utbniva_85(skapa_fil=FALSE)
data_utbniva_85 <- function(region_vekt="20",
                            spara_data = TRUE,
                            output_mapp = "G:/skript/jon/Slask/",
                            filnamn = "utbildningsniva_85.xlsx"){
  
  # ========================================== Läser in data ============================================
  # Skapa en lista med information som vi vill ha hem -- skapas lättast via pxweb_interactive()
  pxweb_query_list <- 
    list("Region" = region_vekt,
         "Kon"  =c("*"),
         "Alder" = c(as.character(25:64)),
         "UtbildningsNiva" = c("*"),
         "ContentsCode" = c("UF0506A1"),
         "Tid" = c("*"))
  
  # Download data 
  px_data <- 
    pxweb_get(url = "https://api.scb.se/OV0104/v1/doris/sv/ssd/UF/UF0506/UF0506B/Utbildning",
              query = pxweb_query_list)
  
  # Convert to data.frame 
  px_df <- as.data.frame(px_data, column.name.type = "text", variable.value.type = "text")
  
  px_df$utb_niva <- case_when(
    px_df$utbildningsnivå == "förgymnasial utbildning kortare än 9 år" ~ "Förgymnasial utbildning",
    px_df$utbildningsnivå == "förgymnasial utbildning, 9 (10) år" ~ "Förgymnasial utbildning",
    px_df$utbildningsnivå == "gymnasial utbildning, högst 2 år" ~ "Gymnasial utbildning",
    px_df$utbildningsnivå == "gymnasial utbildning, 3 år" ~ "Gymnasial utbildning",
    px_df$utbildningsnivå == "eftergymnasial utbildning, mindre än 3 år" ~ "Eftergymnasial utbildning, mindre än 3 år",
    px_df$utbildningsnivå == "eftergymnasial utbildning, 3 år eller mer"~ "Eftergymnasial utbildning, 3 år eller mer",
    px_df$utbildningsnivå == "forskarutbildning" ~ "Eftergymnasial utbildning, 3 år eller mer",
    px_df$utbildningsnivå == "uppgift om utbildningsnivå saknas" ~ "Uppgift saknas")
  
  if (spara_data==TRUE){
    flik_lista=lst("Utbildningsnivå_85"= px_df)
    openxlsx::write.xlsx(px_df,paste0(output_mapp,filnamn))
  }
  
  # # Tar bort uppgift saknas och beräknar hur stor andel som har en viss utbildning - ej uppdelat på kön
  # px_df_utskrift<-px_df %>%
  #   filter(utb_niva!="Uppgift saknas") %>% 
  #     group_by(år,region,utb_niva) %>% 
  #       summarize(antal=sum(Befolkning)) %>% 
  #         mutate(andel=(antal/sum(antal))*100)
  # 
  # # Tar bort uppgift saknas och beräknar hur stor andel som har en viss utbildning - uppdelat på kön
  # px_df_utskrift_kon<-px_df %>%
  #   filter(utb_niva!="Uppgift saknas") %>% 
  #   group_by(år,region,kön,utb_niva) %>% 
  #   summarize(antal=sum(Befolkning)) %>% 
  #   mutate(andel=(antal/sum(antal))*100)
  # 
  # 
  # # Gör om till en faktorvariabel för att styra vilken ordning utbildningsnivåer kommer i.
  # px_df_utskrift$utb_niva <- factor(px_df_utskrift$utb_niva, levels = c("Eftergymnasial utbildning, 3 år eller mer","Eftergymnasial utbildning, mindre än 3 år","Gymnasial utbildning","Förgymnasial utbildning"))
  # 
  # if(diag_utb_85==TRUE){
  #   diagramtitel <- paste0("Utbildningsnivå (25-64 år) i ",ValdGeografi)
  #   diagramfilnamn <- paste0("utbildningsniva_85_21.png")
  #   objektnamn <- c(objektnamn,diagramtitel)
  #   
  #   gg_obj <- SkapaStapelDiagram(skickad_df =px_df_utskrift %>%
  #                                  filter(år%in%c("1985","1990","2000","2010",max(px_df_utskrift_kon$år)),region==ValdGeografi), 
  #                                skickad_x_var = "år", 
  #                                skickad_y_var = "andel", 
  #                                skickad_x_grupp = "utb_niva",
  #                                manual_x_axis_text_vjust=1,
  #                                manual_x_axis_text_hjust=1,
  #                                manual_color = diagramfarger("gron_sex")[3:6],
  #                                diagram_titel = diagramtitel,
  #                                diagram_capt =  diagram_capt,
  #                                diagram_facet = FALSE,
  #                                x_axis_sort_value = FALSE,
  #                                x_axis_lutning = 0, 
  #                                diagram_liggande = FALSE,
  #                                legend_vand_ordning=TRUE,
  #                                geom_position_stack = TRUE,
  #                                manual_y_axis_title="procent",
  #                                berakna_index = FALSE,
  #                                output_mapp = output_mapp,
  #                                filnamn_diagram = diagramfilnamn,
  #                                skriv_till_diagramfil = skapa_fil)
  #   
  #   gg_list[[i]] <-gg_obj
  #   i=i+1
  # }
  # 
  # if(diag_utb_85_kon==TRUE){
  #   diagramtitel <- paste0("Andel av befolkningen (25-64 år) med minst 3 års eftergymnasial utbildning i ",ValdGeografi)
  #   diagramtitel <- str_wrap(diagramtitel)
  #   diagramfilnamn <- paste0("utbildningsniva_85_21_kon.png")
  #   objektnamn <- c(objektnamn,diagramtitel)
  #   
  #   gg_obj <- SkapaStapelDiagram(skickad_df =px_df_utskrift_kon %>%
  #                                  filter(år%in%c("1985","1990","1995","2000","2005","2010","2015",max(px_df_utskrift_kon$år))) %>% 
  #                                  filter(region==ValdGeografi,utb_niva=="Eftergymnasial utbildning, 3 år eller mer"),
  #                                skickad_x_var = "år", 
  #                                skickad_y_var = "andel", 
  #                                skickad_x_grupp = "kön",
  #                                manual_x_axis_text_vjust=1,
  #                                manual_x_axis_text_hjust=1,
  #                                manual_color = diagramfarger("kon"),
  #                                diagram_titel = diagramtitel,
  #                                diagram_capt =  diagram_capt,
  #                                diagram_facet = FALSE,
  #                                x_axis_sort_value = FALSE,
  #                                x_axis_lutning = 0, 
  #                                diagram_liggande = FALSE,
  #                                manual_y_axis_title="procent",
  #                                berakna_index = FALSE,
  #                                output_mapp = output_mapp,
  #                                filnamn_diagram = diagramfilnamn,
  #                                skriv_till_diagramfil = skapa_fil)
  #   
  #   gg_list[[i]] <-gg_obj
  #   i=i+1
  # }
  # 
  # # Ta bort s och län i länsnamn
  # px_df_utskrift$region<-skapa_kortnamn_lan(px_df_utskrift$region)
  # px_df_utskrift_kon$region<-skapa_kortnamn_lan(px_df_utskrift_kon$region)
  # 
  # px_df_utskrift_fokus <- px_df_utskrift
  # # skapa fokusvariabel för att fokusera på valt län och riket
  # px_df_utskrift_fokus$fokus <- NA                      # en metod för att få bort warning messages för "Unknown or uninitialised column: `fokus`."
  # px_df_utskrift_fokus$fokus <- 0
  # px_df_utskrift_fokus$fokus[px_df_utskrift_fokus$region =="Dalarna"] <- 1
  # px_df_utskrift_fokus$fokus[px_df_utskrift_fokus$region =="Riket"] <- 2
  # 
  # if(diag_utb_lan==TRUE){
  #   diagramtitel <- paste0("Andel av befolkningen (25-64 år) med minst 3 års eftergymnasial utbildning ",max(px_df_utskrift$år))
  #   diagramtitel <- str_wrap(diagramtitel)
  #   diagramfilnamn <- paste0("utbildningsniva_jmf.png")
  #   objektnamn <- c(objektnamn,diagramtitel)
  #   
  #   gg_obj <- SkapaStapelDiagram(skickad_df =px_df_utskrift_fokus %>%
  #                                  filter(år==max(px_df_utskrift_fokus$år),utb_niva=="Eftergymnasial utbildning, 3 år eller mer") %>% 
  #                                  filter(utb_niva=="Eftergymnasial utbildning, 3 år eller mer") %>% 
  #                                  mutate(region=ifelse(region=="Riket", "Sverige",region)), 
  #                                skickad_x_var = "region", 
  #                                skickad_y_var = "andel", 
  #                                #skickad_x_grupp = "utb_niva",
  #                                manual_x_axis_text_vjust=1,
  #                                manual_x_axis_text_hjust=1,
  #                                manual_color = diagramfarger("gron_tre_fokus"),
  #                                diagram_titel = diagramtitel,
  #                                diagram_capt =  diagram_capt,
  #                                diagram_facet = FALSE,
  #                                x_axis_sort_value = TRUE,
  #                                x_axis_lutning = 45,
  #                                x_var_fokus = "fokus",
  #                                diagram_liggande = FALSE,
  #                                legend_vand_ordning=FALSE,
  #                                geom_position_stack = FALSE,
  #                                manual_y_axis_title="procent",
  #                                berakna_index = FALSE,
  #                                output_mapp = output_mapp,
  #                                filnamn_diagram = diagramfilnamn,
  #                                skriv_till_diagramfil = skapa_fil)
  #   
  #   gg_list[[i]] <-gg_obj
  #   i=i+1
  # }
  # 
  # if(diag_utb_lan_kon==TRUE){
  #   diagramtitel <- paste0("Andel av befolkningen (25-64 år) med minst 3 års eftergymnasial utbildning ",max(px_df_utskrift$år))
  #   diagramtitel <- str_wrap(diagramtitel)
  #   diagramfilnamn <- paste0("utbildningsniva_jmf_kon.png")
  #   objektnamn <- c(objektnamn,diagramtitel)
  #   
  #   gg_obj <- SkapaStapelDiagram(skickad_df =px_df_utskrift_kon %>%
  #                                  filter(år==max(px_df_utskrift_kon$år),utb_niva=="Eftergymnasial utbildning, 3 år eller mer") %>% 
  #                                  filter(utb_niva=="Eftergymnasial utbildning, 3 år eller mer")%>% 
  #                                  mutate(region=ifelse(region=="Riket", "Sverige",region)),  
  #                                skickad_x_var = "region", 
  #                                skickad_y_var = "andel", 
  #                                skickad_x_grupp = "kön",
  #                                manual_x_axis_text_vjust=1,
  #                                manual_x_axis_text_hjust=1,
  #                                manual_color = diagramfarger("kon"),
  #                                diagram_titel = diagramtitel,
  #                                diagram_capt =  diagram_capt,
  #                                diagram_facet = FALSE,
  #                                x_axis_sort_value = TRUE,
  #                                x_axis_lutning = 45,
  #                                #x_var_fokus = "fokus",
  #                                diagram_liggande = FALSE,
  #                                legend_vand_ordning=FALSE,
  #                                geom_position_stack = FALSE,
  #                                manual_y_axis_title="procent",
  #                                berakna_index = FALSE,
  #                                output_mapp = output_mapp,
  #                                filnamn_diagram = diagramfilnamn,
  #                                skriv_till_diagramfil = skapa_fil)
  #   
  #   gg_list[[i]] <-gg_obj
  #   i=i+1
  # }
  # 
  # 
  # names(gg_list)<-c(objektnamn)
  # return(gg_list)
  
}
