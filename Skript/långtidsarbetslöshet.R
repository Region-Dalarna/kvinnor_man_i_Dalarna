# R-skript som används för att ta fram figurer för långtidsarbetslöshet (gäller alla arbetslösa). Data kommer från Supercross
if (!require("pacman")) install.packages("pacman")
p_load(openxlsx,
       tidyverse,
       here)

# Läser in de funktioner som används i skriptet
#source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

#test_list=diag_langtidsarbetsloshet()
diag_langtidsarbetsloshet <- function(spara_data=TRUE,
                                      output_mapp="G:/skript/jon/Slask/",
                                      filnamn = "långtidsarbetslöshet_bearbetad.xlsx"){
  
  #=======================================================================================================
  # Läser in från en excelfil med data hämtat från Supercross
  #langtidsarbetsloshet_df <- read.xlsx(""),sheet=1)
  # langtidsarbetsloshet_df <-read.xlsx("G:/skript/projekt/data/kvinnor_man/Langtidsarbetsloshet_230703.xlsx",startRow = 6) %>%   
  #   rename("Region" = X1,
  #          "Arb_status" = X2,
  #          "Bakgrund" = X3,
  #          "Kon" = X4)
  
  langtidsarbetsloshet_df <-read.xlsx("G:/skript/projekt/data/kvinnor_man/Langtidsarbetsloshet_230704.xlsx",startRow = 6) %>%   
    rename("Region" = X1,
           "Arb_status" = X2,
           "Bakgrund" = X3,
           "Kon" = X4,
           "Utbildningsniva" = X5)
  
  # Snyggar till data genom att ta bort ett antal space i de olika utbildningskategorierna
  langtidsarbetsloshet_df$Utbildningsniva <- word(langtidsarbetsloshet_df$Utbildningsniva,1,sep=fixed('  '))
  
  # Kvinnor och män skrivs med små bokstäver
  langtidsarbetsloshet_df$Kon <- tolower(langtidsarbetsloshet_df$Kon)
  
  # Andel i olika grupper uppdelat på kön och bakgrund
  langtidsarbetsloshet_andel_df <- langtidsarbetsloshet_df %>%
    filter(Arb_status != "Är ej öppet arbetslös") %>% 
      group_by(Region,Bakgrund,Kon,Arb_status) %>%
        summarize(Antal = sum(as.numeric(Nattbefolkning),na.rm = TRUE)) %>% 
          mutate(Andel = (Antal/sum(Antal))*100)
  
  # Delar upp i kort (-12 månader) och lång (> 12 månader) arbetslöshetstid och fokusera på utbildning
  
  # Slår ihop forskarutbildning och eftergymnasial utbildning längre än 3 år
  langtidsarbetsloshet_df[langtidsarbetsloshet_df == "Eftergymnasial utbildning 3 år eller längre (exkl. forskarutbildning)"] <- "Eftergymnasial utbildning 3 år eller längre"
  langtidsarbetsloshet_df[langtidsarbetsloshet_df == "Forskarutbildning"] <- "Eftergymnasial utbildning 3 år eller längre"

  langtidsarbetsloshet_utbildning_df <- langtidsarbetsloshet_df %>%
    filter(Arb_status != "Är ej öppet arbetslös",Utbildningsniva != "Ingår ej i utbildningsregistret") %>%
      mutate(Arbetsloshet_langd = ifelse(Arb_status %in% c("12-24 månader","24- månader"),"Långtidsarbetslös","Korttidsarbetslös")) %>%
        group_by(Region,Utbildningsniva,Kon,Arbetsloshet_langd) %>%
          summarize(Antal = sum(as.numeric(Nattbefolkning),na.rm = TRUE)) %>%
            mutate(Andel = (Antal/sum(Antal))*100)
  
  # langtidsarbetsloshet_utbildning_df <- langtidsarbetsloshet_df %>%
  #   filter(Arb_status != "Är ej öppet arbetslös",Utbildningsniva != "Ingår ej i utbildningsregistret") %>%
  #       group_by(Region,Utbildningsniva,Kon,Arb_status) %>%
  #         summarize(Antal = sum(as.numeric(Nattbefolkning),na.rm = TRUE)) %>% 
  #           mutate(Andel = (Antal/sum(Antal))*100)
  
  if (spara_data==TRUE){
    flik_lista=lst("Långtidsarbetslöshet bakgrund"= langtidsarbetsloshet_andel_df,"Långtidsarbetslöshet utbildning" = langtidsarbetsloshet_utbildning_df)
    openxlsx::write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  }
  
  # langtidsarbetsloshet_bakgrund_kon <- langtidsarbetsloshet_df %>% 
  #   group_by(Region,Kon,Bakgrund,Arb_status,Utbildningsniva) %>% 
  #     summarize(Antal=sum(as.integer(Dagbefolkning),na.rm = TRUE)) %>% 
  #       mutate(Andel=round((Antal/sum(Antal))*100,1))
  # 
  # # De tre dataset som används för att skriva ut figurer
  # # Lan
  # # Bara uppdelat på kön
  # sum_lan_kon_df <- langtidsarbetsloshet_df %>% 
  #   group_by(Region,Kon,Arb_status) %>% 
  #   summarize(Antal=sum(as.integer(Antal),na.rm = TRUE)) %>% 
  #   mutate(Andel=round((Antal/sum(Antal))*100,1))
  # # Bakgrund
  # sum_lan_bakgrund_df <- langtidsarbetsloshet_df %>% 
  #   group_by(Region,Kon,Bakgrund,Arb_status) %>% 
  #   summarize(Antal=sum(as.integer(Antal),na.rm = TRUE)) %>% 
  #   mutate(Andel=round((Antal/sum(Antal))*100,1))
  # # Utbildningsniva
  # sum_lan_utbildningsniva_df <- langtidsarbetsloshet_df %>% 
  #   group_by(Region,Kon,Utbildningsniva,Arb_status) %>% 
  #   summarize(Antal=sum(as.integer(Antal),na.rm = TRUE)) %>% 
  #   mutate(Andel=round((Antal/sum(Antal))*100,1))
  # 
  # # Kommun - enbart uppdelning på längd då det annars blir för plottrigt
  # sum_kommun_df <- langtidsarbetsloshet_df %>% 
  #   group_by(Kommun,Arb_status) %>% 
  #   summarize(Antal=sum(as.integer(Antal),na.rm = TRUE)) %>% 
  #   mutate(Andel=round((Antal/sum(Antal))*100,1))
  # 
  # Enbart kön
  # if(diag_lan_kon==TRUE){
  #   diagramtitel <- "Arbetslöshet i Dalarnas län (20-64 år)"
  #   diagramfilnamn <- "langtidsarbetsloshet_lan_kon.png"
  # 
  #   gg_obj <- SkapaStapelDiagram(skickad_df =langtidsarbetsloshet_andel_df, 
  #                                skickad_x_var = "Arb_status", 
  #                                skickad_y_var = "Andel", 
  #                                skickad_x_grupp = "Kon",
  #                                x_axis_lutning = 0,
  #                                x_axis_sort_value=FALSE,
  #                                manual_color = diagramfarger("kon"),
  #                                manual_y_axis_title="procent",
  #                                diagram_titel = diagramtitel,
  #                                diagram_capt =  diagram_capt,
  #                                diagram_facet = TRUE,
  #                                facet_grp = "Bakgrund",
  #                                facet_legend_bottom=TRUE,
  #                                facet_scale = "fixed",
  #                                y_axis_100proc = TRUE,
  #                                berakna_index = FALSE,
  #                                output_mapp = "",
  #                                filnamn_diagram = diagramfilnamn,
  #                                skriv_till_diagramfil = FALSE)
  #   
  #   gg_list[[i]] <-gg_obj
  #   i=i+1
  # }
  # 
  # if(diag_lan_bakgrund==TRUE){
  #   # Utländskt/svensk bakgrund
  #   diagramtitel <-paste0("Arbetslöshet i Dalarnas län (20-64 år) ",Ar)
  #   diagramfilnamn <- paste0("langtidsarbetsloshet_lan_bakgrund",".png")
  #   ifelse(j==1,objektnamn <-"langtidsarbetsloshet_lan_bakgrund",objektnamn <- c(objektnamn,"langtidsarbetsloshet_lan_bakgrund"))
  #   j=j+1
  #   
  #   gg_obj <- SkapaStapelDiagram(skickad_df =sum_lan_bakgrund_df[sum_lan_bakgrund_df$Arb_status!="Är ej öppet arbetslös",], 
  #                                skickad_x_var = "Arb_status", 
  #                                skickad_y_var = "Andel", 
  #                                skickad_x_grupp = "Kon",
  #                                #skickad_filter_OR_vect = "Högre utbildningar", 
  #                                #skickad_filter_OR_var = "utbildningsnivå",
  #                                # manual_x_axis_text_vjust=1,
  #                                # manual_x_axis_text_hjust=1,
  #                                x_axis_lutning = 0,
  #                                x_axis_sort_value=FALSE,
  #                                manual_color = diagramfarger("kon"),
  #                                manual_y_axis_title="procent",
  #                                diagram_titel = diagramtitel,
  #                                diagram_capt =  diagram_capt,
  #                                diagram_facet =TRUE,
  #                                facet_grp = "Bakgrund",
  #                                facet_scale = "fixed",
  #                                facet_legend_bottom=TRUE,
  #                                berakna_index = FALSE,
  #                                output_mapp = output_mapp,
  #                                filnamn_diagram = diagramfilnamn,
  #                                skriv_till_diagramfil = skapa_fil)
  #   
  #   gg_list[[i]] <-gg_obj
  #   i=i+1
  # }
  # 
  # # Skapar en faktorvariabel som styr vilken ordning som utbildningsgrupper visas
  # sum_lan_utbildningsniva_df$Utbildningsniva <- factor(sum_lan_utbildningsniva_df$Utbildningsniva, levels = c("Eftergymnasial utbildning 3 år eller längre","Eftergymnasial utbildning kortare än 3 år","Gymnasial utbildning 3 år","Gymnasial utbildning högst 2-årig","Förgymnasial utbildning 9 år","Förgymnasial utbildning kortare än 9 år")[6:1])
  # 
  # 
  # if(diag_lan_utbildningsniva==TRUE){
  #   # Utbildningsbakgrund
  #   diagramtitel <-paste0("Arbetslöshet i Dalarnas län (20-64 år) ",Ar," per utbildningsgrupp")
  #   diagramfilnamn <- paste0("langtidsarbetsloshet_lan_utbildningsniva",".png")
  #   ifelse(j==1,objektnamn <-"langtidsarbetsloshet_lan_utbildningsniva",objektnamn <- c(objektnamn,"langtidsarbetsloshet_lan_utbildningsniva"))
  #   j=j+1
  #   
  #   # Skapar diagram för utbildningnivå på länsnivå 2020
  #   gg_obj <- SkapaStapelDiagram(skickad_df =sum_lan_utbildningsniva_df %>% 
  #                                  filter(Arb_status!="Är ej öppet arbetslös") %>% 
  #                                  filter(Utbildningsniva!="Uppgift saknas",Utbildningsniva!="Ingår ej i utbildningsregistret"), 
  #                                skickad_x_var = "Arb_status", 
  #                                skickad_y_var = "Andel", 
  #                                skickad_x_grupp = "Utbildningsniva",
  #                                #skickad_filter_OR_vect = "Högre utbildningar", 
  #                                #skickad_filter_OR_var = "utbildningsnivå",
  #                                # manual_x_axis_text_vjust=1,
  #                                # manual_x_axis_text_hjust=1,
  #                                x_axis_lutning = 0,
  #                                x_axis_sort_value=FALSE,
  #                                manual_color = diagramfarger("gron_sex"),
  #                                manual_y_axis_title="procent",
  #                                diagram_facet =TRUE,
  #                                facet_grp = "Kon",
  #                                facet_scale = "fixed",
  #                                facet_legend_bottom=TRUE,
  #                                diagram_titel = diagramtitel,
  #                                diagram_capt =  diagram_capt,
  #                                berakna_index = FALSE,
  #                                output_mapp = output_mapp,
  #                                filnamn_diagram = diagramfilnamn,
  #                                skriv_till_diagramfil = skapa_fil)
  #   
  #   gg_list[[i]] <-gg_obj
  #   i=i+1
  # }
  # 
  # if(diag_kommun==TRUE){
  #   # Kommun
  #   diagramtitel <-paste0("Arbetslöshet i Dalarnas län (20-64 år) ",Ar)
  #   diagramfilnamn <- paste0("langtidsarbetsloshet_kommun",".png")
  #   ifelse(j==1,objektnamn <-"langtidsarbetsloshet_kommun",objektnamn <- c(objektnamn,"langtidsarbetsloshet_kommun"))
  #   j=j+1
  #   
  #   gg_obj <- SkapaStapelDiagram(skickad_df=sum_kommun_df[sum_kommun_df$Arb_status!="Är ej öppet arbetslös",], 
  #                                skickad_x_var = "Kommun", 
  #                                skickad_y_var = "Andel", 
  #                                skickad_x_grupp = "Arb_status",
  #                                #skickad_filter_OR_vect = "Högre utbildningar", 
  #                                #skickad_filter_OR_var = "utbildningsnivå",
  #                                manual_x_axis_text_vjust=1,
  #                                manual_x_axis_text_hjust=1,
  #                                x_axis_sort_value=TRUE,
  #                                manual_color = diagramfarger("gron_sex"),
  #                                manual_y_axis_title="procent",
  #                                diagram_titel = diagramtitel,
  #                                diagram_capt =  diagram_capt,
  #                                berakna_index = FALSE,
  #                                output_mapp = output_mapp,
  #                                filnamn_diagram = diagramfilnamn,
  #                                skriv_till_diagramfil = skapa_fil)
  #   
  #   gg_list[[i]] <-gg_obj
  #   i=i+1
  # }
  # 
  # names(gg_list)<-c(objektnamn)
  # return(gg_list)
}

