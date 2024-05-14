# Nödvändiga paket
if (!require("pacman")) install.packages("pacman")
p_load(here,
       tidyverse,
       openxlsx,
       stringi)

# Funktioner som behövs (hämtas från Git-Hub)
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_SkapaDiagram.R")
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

# Här läggs figurerna
outputmapp_figur = "G:/skript/jon/Figurer/"
Output_mapp="G:/skript/projekt/data/kvinnor_man/"
# Om man vill spara figurer sätts denna variabel till TRUE
spara_figur = TRUE

# befolkning_df_for <- read.xlsx("G:/skript/projekt/data/kvinnor_man/befolkning.xlsx",sheet = 2)
# Län
# utbildning_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/utbildningsniva.xlsx")
# utbildning_85_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/utbildningsniva_85.xlsx")
# forvarvsarbetande_bransch <- read.xlsx("G:/skript/projekt/data/kvinnor_man/forvarvsarbetande_bransch.xlsx")
# arbetsmarknadsstatus_lan_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/arbetsmarknadsstatus_senastear.xlsx",sheet = 1)
# arbetsmarknadsstatus_kommun_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/arbetsmarknadsstatus_senastear.xlsx",sheet = 2)
# arbetsloshet_tidsserie <- read.xlsx("G:/skript/projekt/data/kvinnor_man/arbetslöshet_08_senastear_bearbetad.xlsx",sheet = 1)
#langtidsarbetsloshet_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/långtidsarbetslöshet_bearbetad.xlsx",sheet = 1)
#langtidsarbetsloshet_utbildning_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/långtidsarbetslöshet_bearbetad.xlsx",sheet = 2)
# etablering_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/etableringstid.xlsx") %>% 
#   rename(andel = `Andel.förvärvsarbetande.(ny.definition.från.och.med.2019)`)
#disponibel_inkomst_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/disponibel_inkomst.xlsx")
# matchning_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/matchning.xlsx")
#langsiktiga_skulder_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/skuldsatta.xlsx",sheet = 1)
#skulder_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/skuldsatta.xlsx", sheet = 2)
#skulder_andel_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/skuldsatta_andel.xlsx")
#riksdagsval_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/val.xlsx", sheet = 1)
#regionval_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/val.xlsx", sheet = 2)
# kommunval_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/val.xlsx", sheet = 3)
#chefer_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/chefer.xlsx")
# foraldrapenning_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/foraldrapenning.xlsx",sheet = 1) %>% 
#   mutate(Kommun = ifelse(Kommun == "20 Dalarnas län", "Samtliga kommuner",  Kommun))
# vab_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/foraldrapenning.xlsx",sheet = 2) %>% 
#   mutate(Kommun = ifelse(Kommun == "20 Dalarnas län", "Samtliga kommuner",  Kommun))
# ohalsotal_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/ohalsotal_sjukpenningtal.xlsx",sheet = 1) %>% 
#   mutate(Kommun = ifelse(Kommun == "20 Dalarnas län", "Samtliga kommuner",  Kommun)) %>% 
#   filter(Ålder == "Samtliga 16-64 år")
# sjukpenningtal_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/ohalsotal_sjukpenningtal.xlsx",sheet = 2)%>% 
#   mutate(Kommun = ifelse(Kommun == "20 Dalarnas län", "Samtliga kommuner",  Kommun)) %>% 
#   filter(Ålder == "Samtliga 16-64 år")
# stress_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/sjukfall_stress.xlsx")
# OBS! UPPDATERAS MANUELLT
# startade_sjukfall_bransch_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/startade_sjukfall_bransch_bearbetad.xlsx")
#gymnasieantagning_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/gymnasieantagning.xlsx")
#gymnasiet_genomstromning_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/gymnasie_genomströmning.xlsx")



# Utbildningsnivå - två diagram. Eventuellt en kommunjämförelse också
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diag_utbniva_flera_diagram_scb.R")
test = diag_utbniva_tidserie_och_lansjmfr(region_vekt = c("2061","20"), output_mapp = outputmapp_figur)

# # Gymnasiet - spara. Verkar finnas på kommunnivå (i alla fall genomströmning) - Peter har ett skript
# 
# pacman::p_load(tidyverse,
#                pdftools)
# 
# source("G:/skript/hamta_data/func_gymnasieantagningen.R", encoding = "utf-8", echo = FALSE)
# diagram_capt <- "Källa: Källan är Gymnasieantagningen, Dalarnas kommunförbund\nBearbetning:Samhällsanalys, Region Dalarna\nDiagramförklaring: Antagna i början av september"
# 
# gymnant_df <- las_in_data_gymnasieantagningen()
# 
# gymnant_sum_Dal <- gymnant_df %>% 
#   filter(Kommun == "Ludvika-Smedjebacken") %>% 
#     group_by(ar,program) %>% 
#       summarize(Män=sum(Ant_Män),
#                 Kvinnor=sum(Ant_Kv))
# 
# # Pivoterar data för att enkelt kunna dela upp på kvinnor och män
# gymnant_sum_Dal<-pivot_longer(gymnant_sum_Dal,3:4,names_to="Kon",values_to = "Antagna")
# 
# # Ändrar ett av utbildningsnamnen
# gymnant_sum_Dal[gymnant_sum_Dal == "Flygteknikutbildningen, Marinteknikutbildningen, Sjöfartsutbildningen, Tågteknikutbildningen, Utbildningen samiska näringar eller Yrkesdansarutbildningen"] <- "Flygteknikutbildningen mfl."
# 
# gym_sum <- gymnant_sum_Dal %>%
#   filter(Antagna>0) %>% 
#   group_by(ar,program) %>% 
#     mutate(andel = (Antagna/sum(Antagna))*100-0.001)
# 
# diagram_titel <- paste0("Antagna gymnasieelever i Ludvika-Smedjebacken ", max(gymnasieantagning_df$ar)," per program")
# diagramfil <- "gymnasieantagning_alla.png"
# 
# gymnasieantagning_fig <- SkapaStapelDiagram(skickad_df = gym_sum%>%
#                                               filter(ar == max(.$ar)),
#                                             skickad_x_var = "program",
#                                             skickad_y_var = "andel",
#                                             skickad_x_grupp = "Kon",
#                                             manual_color = diagramfarger("kon"),
#                                             diagram_titel = diagram_titel,
#                                             x_axis_sort_value = TRUE,
#                                             x_axis_sort_grp = 2,
#                                             fokusera_varden = list(list(geom = "rect", ymin=40, ymax=60, xmin=0, xmax=Inf, alpha=0.2, fill="grey20")),
#                                             diagram_capt = diagram_capt,
#                                             #procent_0_100_10intervaller = TRUE,
#                                             manual_y_axis_title = "procent",
#                                             x_axis_lutning = 0,
#                                             diagram_liggande = TRUE,
#                                             geom_position_stack = TRUE,
#                                             berakna_index = FALSE,
#                                             output_mapp = outputmapp_figur,
#                                             filnamn_diagram = diagramfil,
#                                             skriv_till_diagramfil = spara_figur)


# Antal förvärvsarbetande - vänta med tillsvidare. Är inget problem dock, men oklart vilken figur jag skall använda
source("C:/Users/frkjon/Projekt/laget_i_Dalarna/Skript/hamta_sysselsatta_region_kon_sni2007_fodelseregion_tid_ArbStDoNMNN_scb.R")
df <- hamta_sysselsatta_region_kon_sni2007_fodelseregion_tid_scb(region_vekt = "2061",
                                                                 kon_klartext = c("män", "kvinnor"),
                                                                 fodelseregion_klartext = "totalt",
                                                                 cont_klartext = "sysselsatta efter arbetsställets belägenhet",
                                                                 tid_koder = "9999") %>%
  rename(tid = månad) %>% 
  mutate(år = str_sub(tid, 1, 4) %>% as.integer(),
         månad_nr = parse_integer(str_sub(tid, 6,7)),
         månad = format(as.Date(paste(år, str_sub(tid, 6,7), "1", sep = "-")), "%B"),
         år_månad = paste0(år, " - ", månad),
         månad_år = paste0(månad, " ", år),
         bransch = ifelse(bransch =="Tillverkning och utvinning","Tillverkning och utv.",bransch)) 

diagram_capt <- "Källa: BAS i SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: 16-74 år"

diagramtitel <- paste0("Antal förvärvsarbetande per bransch i ",unique(df$region), "  i ",unique(df$månad_år))
diagramfil <- "forvarvsarbetande_bransch_Smedjebacken.png"

forvarvsarbetande_kon_fig <- SkapaStapelDiagram(skickad_df <- df %>% 
                                                  filter(bransch != "Okänt"), 
                                                skickad_x_var = "bransch", 
                                                skickad_y_var = "sysselsatta efter arbetsställets belägenhet", 
                                                skickad_x_grupp = "kön",
                                                diagram_liggande=FALSE,
                                                manual_x_axis_text_vjust=1,
                                                manual_x_axis_text_hjust=1,
                                                manual_y_axis_title = "",
                                                x_axis_lutning = 45,
                                                x_axis_sort_value=TRUE,
                                                manual_color = diagramfarger("kon"),
                                                stodlinjer_avrunda_fem = TRUE,
                                                diagram_titel = diagramtitel,
                                                diagram_capt =  diagram_capt,
                                                facet_legend_bottom=TRUE,
                                                berakna_index = FALSE,
                                                output_mapp = outputmapp_figur,
                                                filnamn_diagram = diagramfil,
                                                skriv_till_diagramfil = spara_figur)


# Yrken
source("https://raw.githubusercontent.com/Region-Dalarna/hamta_data/main/hamta_data_yrke_bransch_SCB.R")
yrke_df = hamta_data_yrken_bransch(region_vekt = "2061",
                                bransch_klartext = NA) %>% 
  rename("Yrke" = `Yrke (SSYK 2012)`,
         "Antal" = `Anställda 16-64 år med arbetsplats i regionen (dagbef)`)


diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: 16-64 år"

yrke_kvinnor <- yrke_df %>% 
  filter(kön=='kvinnor',Yrke!="yrke okänt") %>%
  slice_max(Antal,n=10) %>% 
  mutate(Yrke = ifelse(Yrke == "Undersköterskor, hemtjänst, hemsjukvård och äldreboende","Undersköterskor m.fl.",Yrke))

diagram_titel <- paste0("Yrken med högst antal förvärvsarbetande kvinnor i ",unique(yrke_kvinnor$region)," ",unique(yrke_kvinnor$år))
diagram_titel <- str_wrap(diagram_titel,60)
diagramfil="yrken_kvinnor.png"

yrken_kvinnor_fig <- SkapaStapelDiagram(skickad_df = yrke_kvinnor, 
                                        skickad_x_var = "Yrke", 
                                        skickad_y_var = "Antal",
                                        manual_y_axis_title = "Antal förvärvsarbetande",
                                        manual_color = diagramfarger("rus_sex")[1],
                                        x_axis_sort_value = TRUE,
                                        diagram_titel = diagram_titel,
                                        diagram_capt = diagram_capt,
                                        x_axis_lutning = 0,
                                        stodlinjer_avrunda_fem = TRUE,
                                        diagram_liggande = TRUE,
                                        output_mapp = outputmapp_figur,
                                        filnamn_diagram = diagramfil,
                                        skriv_till_diagramfil = spara_figur)

yrke_man <- yrke_df %>% 
  filter(kön=='män',Yrke!="yrke okänt") %>%
  slice_max(Antal,n=10)

diagram_titel <- paste0("Yrken med högst antal förvärvsarbetande män i ",unique(yrke_kvinnor$region)," ",unique(yrke_kvinnor$år))
diagram_titel <- str_wrap(diagram_titel,60)
diagramfil="yrken_man.png"

yrken_man_fig <- SkapaStapelDiagram(skickad_df = yrke_man, 
                                    skickad_x_var = "Yrke", 
                                    skickad_y_var = "Antal",
                                    manual_y_axis_title = "Antal förvärvsarbetande",
                                    manual_color = diagramfarger("rus_sex")[1],
                                    x_axis_sort_value = TRUE,
                                    stodlinjer_avrunda_fem = TRUE,
                                    diagram_titel = diagram_titel,
                                    diagram_capt = diagram_capt,
                                    x_axis_lutning = 0,
                                    diagram_liggande = TRUE,
                                    output_mapp = outputmapp_figur,
                                    filnamn_diagram = diagramfil,
                                    skriv_till_diagramfil = spara_figur)

# Matchning - saknas på kommunnivå

#####################
# Inkomster
#####################

# Medianinkomst

source(here("Skript","medianinkomst.R"), encoding="UTF-8")
diag_medianinkomst(spara_data = TRUE,
                   output_mapp = Output_mapp)

medianinkomst_df <-  read.xlsx("G:/skript/projekt/data/kvinnor_man/medianinkomst.xlsx") %>% 
  rename(medianinkomst = `Medianinkomst,.tkr`) 

diagram_capt <- "Källa: SCB:s öppna statistikdatabas. Bearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Sammaräknad förvärvsinkomst, dvs. alla skattepliktiga inkomster före skatt (dock ej kapitalinkomster)."

medianinkomst_df$region <- skapa_kortnamn_lan(medianinkomst_df$region,byt_ut_riket_mot_sverige = TRUE) 

diagramtitel <- paste0("Medianinkomst (20-64 år) i ",unique(medianinkomst_df %>% filter(regionkod=="2061") %>% .$region))
diagramfilnamn <- paste0("medianinkomst_",unique(medianinkomst_df %>% filter(regionkod=="2061") %>% .$region),"_tid.png")

medianinkomst_tid_fig <- SkapaStapelDiagram(skickad_df =medianinkomst_df %>%
                                              filter(kön != "totalt",
                                                     regionkod=="2061"),
                                            skickad_x_var = "år", 
                                            skickad_y_var = "medianinkomst",
                                            skickad_x_grupp = "kön",
                                            manual_color = diagramfarger("kon"),
                                            diagram_titel = diagramtitel,
                                            diagram_capt =  diagram_capt,
                                            manual_x_axis_text_vjust=1,
                                            manual_x_axis_text_hjust=1,
                                            diagram_facet = FALSE,
                                            facet_scale = "fixed",
                                            stodlinjer_avrunda_fem = TRUE,
                                            vand_sortering = TRUE,
                                            x_axis_lutning = 45,
                                            manual_y_axis_title = "",
                                            output_mapp = outputmapp_figur,
                                            filnamn_diagram = diagramfilnamn,
                                            skriv_till_diagramfil = spara_figur)

diagramtitel <- paste0("Förändring i medianinkomst (20-64 år) i ",unique(medianinkomst_df %>% filter(regionkod=="2061") %>% .$region))
diagramfilnamn <- paste0("medianinkomst_",unique(medianinkomst_df %>% filter(regionkod=="2061") %>% .$region),"_tid_linje.png")

# Skapar diagram för medianinkomst i valt län vs riket (ej uppdelat på kön)
medianinkomst_linje_fig <- SkapaLinjeDiagram(skickad_df = medianinkomst_df %>%
                                               filter(kön != "totalt",
                                                      regionkod=="2061"),
                                             skickad_x_var = "år",
                                             skickad_y_var = "medianinkomst",
                                             skickad_x_grupp = "kön",
                                             #skickad_filter_OR_var = "region",
                                             x_axis_lutning = 45,
                                             manual_color = diagramfarger("kon"),
                                             diagram_titel = diagramtitel,
                                             diagram_capt =  diagram_capt,
                                             stodlinjer_avrunda_fem = TRUE,
                                             diagram_facet = FALSE,
                                             berakna_index = TRUE,
                                             output_mapp = outputmapp_figur,
                                             filnamn_diagram = diagramfilnamn,
                                             skriv_till_diagramfil = spara_figur)

diagramtitel <- paste0("Medianinkomst (20-64 år) år ",max(medianinkomst_df$år))
diagramfilnamn <- paste0("medianinkomst_kommun.png")

medianinkomst_kommun_fig <- SkapaStapelDiagram(skickad_df =medianinkomst_df %>%
                                                 filter(kön != "totalt",
                                                        år == max(år)),
                                               skickad_x_var = "region", 
                                               skickad_y_var = "medianinkomst",
                                               skickad_x_grupp = "kön",
                                               manual_color = diagramfarger("kon"),
                                               diagram_titel = diagramtitel,
                                               diagram_capt =  diagram_capt,
                                               manual_x_axis_text_vjust=1,
                                               manual_x_axis_text_hjust=1,
                                               diagram_facet = FALSE,
                                               facet_scale = "fixed",
                                               x_axis_sort_value = TRUE,
                                               x_axis_sort_grp = 1,
                                               stodlinjer_avrunda_fem = TRUE,
                                               vand_sortering = TRUE,
                                               x_axis_lutning = 45, 
                                               manual_y_axis_title = "",
                                               output_mapp = outputmapp_figur,
                                               filnamn_diagram = diagramfilnamn,
                                               skriv_till_diagramfil = spara_figur)

# Disponibel inkomst  
source(here("Skript","disponibel_inkomst.R"), encoding="UTF-8")
diag_disponibelinkomst(region_vekt = "2061", 
                       output_mapp = Output_mapp,
                       spara_data = TRUE,
                       filnamn = "disponibel_inkomst_smedjebacken.xlsx" )

disponibel_inkomst_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/disponibel_inkomst_smedjebacken.xlsx")

diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."

# Filtrerar dataset på de grupper och år vi vill fokusera på
disponibel_inkomst_utvalda_df <- disponibel_inkomst_df %>%
  filter(hushållstyp %in% c("ensamstående kvinnor utan barn","ensamstående kvinnor med barn 0-19 år","ensamstående män utan barn","ensamstående män med barn 0-19 år","sammanboende utan barn","sammanboende med barn 0-19 år")) %>%
  filter(ålder=="18+ år",
         år %in% c(min(år),max(år)))

diagramtitel <- paste0("Disponibel inkomst (18+ år) i ",unique(disponibel_inkomst_utvalda_df$region))
diagramfilnamn <- paste0("disponibel_inkomst_",unique(disponibel_inkomst_utvalda_df$region),".png")

# Skapar diagram för disponibel inkomst i Dalarna där män jämförs med kvinnor.
disp_ink_fig <- SkapaStapelDiagram(skickad_df = disponibel_inkomst_utvalda_df,
                                   skickad_x_var = "hushållstyp",
                                   skickad_y_var = "Medianvärde,.tkr",
                                   skickad_x_grupp = "år",
                                   # manual_x_axis_text_vjust=0.5,
                                   x_axis_lutning = 0,
                                   manual_color = diagramfarger("rus_sex"),
                                   diagram_titel = diagramtitel,
                                   diagram_capt =  diagram_capt,
                                   stodlinjer_avrunda_fem = TRUE,
                                   diagram_liggande = TRUE,
                                   output_mapp = outputmapp_figur,
                                   filnamn_diagram = diagramfilnamn,
                                   skriv_till_diagramfil = spara_figur)


# Arbetslöshet och sysselsättningsgrad (kommunjämförelse)
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_arbetsmarknadsstatus_senastear.R")
test = diagram_arbetsmarknadsstatus(diag_arbetskraftsdeltagande = FALSE,
                                    kon_klartext = c("kvinnor","män"),
                                    fodelseregion_klartext_vekt = c("inrikes född", "utrikes född"),
                                    valda_farger = diagramfarger("kon"),
                                    output_mapp_figur = outputmapp_figur,
                                    spara_figur = spara_figur)



# Arbetslöshet, bakgrund. 2008 - senaste år. Arbetsförmedlingen, Excel från hemsida
source(here("Skript","arbetsloshet_2008_senastear_kommun.R"), encoding="UTF-8")
diag_arbetsloshet_2008_senastear_kommun(spara_data = TRUE,
                                        filnamn = "arbetslöshet_08_senastear_bearbetad_smed.xlsx",
                                        output_mapp = Output_mapp)

arbetsloshet_tidsserie <- read.xlsx("G:/skript/projekt/data/kvinnor_man/arbetslöshet_08_senastear_bearbetad_smed.xlsx",sheet = 1)

# Text till diagram
diagram_capt <- "Källa: Arbetsförmedlingen.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Månadsdata. Diagrammet visar medelvärdet för året"

diagramtitel <- paste0("Arbetslöshet (16-64 år) i Smedjebacken")
diagramfilnamn <- paste0("arbetsloshet_tidsserie_bakgrund_smed.png")

arbetsloshet_08_fig <- SkapaStapelDiagram(skickad_df = arbetsloshet_tidsserie, 
                                          skickad_x_var = "Ar",
                                          skickad_y_var = "Arbetslöshet",
                                          skickad_x_grupp = "Kon",
                                          manual_x_axis_text_vjust=1,
                                          manual_x_axis_text_hjust=1,
                                          manual_color = diagramfarger("kon"),
                                          diagram_titel = diagramtitel,
                                          diagram_capt =  diagram_capt,
                                          diagram_facet = TRUE,
                                          facet_legend_bottom = TRUE,
                                          stodlinjer_avrunda_fem = TRUE,
                                          facet_grp = "Grupp",
                                          facet_scale = "fixed",
                                          x_axis_lutning = 45,
                                          diagram_liggande = FALSE,
                                          legend_vand_ordning=FALSE,
                                          manual_y_axis_title = "procent",
                                          geom_position_stack = FALSE,
                                          berakna_index = FALSE,
                                          output_mapp = outputmapp_figur,
                                          filnamn_diagram = diagramfilnamn,
                                          skriv_till_diagramfil = spara_figur)


# Etablering - saknas data i många fall. Hoppar över

#Etableringstid - oklart. Kolla upp. Vänta tills det finns ett skapa hämtadataskript som fungerar
#https://www.statistikdatabasen.scb.se/pxweb/sv/ssd/START__AA__AA0003__AA0003B/IntGr1KomKonUtb/

# source(here("Skript","etablering.R"), encoding="UTF-8")
# diag_etablering(spara_data = TRUE,
#                 output_mapp = Output_mapp)
# 
# source("C:/Users/frkjon/Projekt/kvinnor_man_i_Dalarna/Skript/hamta_etableringstid_mm_kommun_region_kon_utbniv_bakgrvar_tid_IntGr1KomKonUtb_scb.R")
# etablering_df = hamta_etableringstid_mm_kommun_region_kon_utbniv_bakgrvar_tid_scb(region_vekt = "2061",
#                                                                          cont_klartext =  "Andel förvärvsarbetande (ny definition från och med 2019)",
#                                                                          bakgrvar_klartext = c("vistelsetid 0-1 år", "vistelsetid 2-3 år", "vistelsetid 4-9 år", "vistelsetid 10- år")) %>% 
#   rename(andel = `Andel förvärvsarbetande (ny definition från och med 2019)`)
# 
# 
# diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."
# 
# # Skapar en faktorvariabel för att få tid sedan etablering i "rätt" ordning i figuren
# etablering_df$bakgrundsvariabel <- factor(etablering_df$bakgrundsvariabel, levels = c("vistelsetid 0-1 år","vistelsetid 2-3 år",
#                                                                                       "vistelsetid 4-9 år","vistelsetid 10- år"))
# 
# diagramtitel <- paste0("Andel förvärvsarbetande 20-64 år bland utrikes födda i ",unique(etablering_df$region)," ",max(etablering_df$år)," efter vistelsetid")
# diagramtitel <- str_wrap(diagramtitel,60)
# diagramfilnamn <- paste0("etablering_Smedjebacken.png")
# 
# # Skapar diagram där etableringstiden jämförs mellan män och kvinnor, oavsett utbildning
# etablering_fig <- SkapaStapelDiagram(skickad_df =etablering_df %>%
#                                        filter(år==max(år),
#                                               utbildningsnivå=="samtliga utbildningsnivåer",
#                                               kön != "män och kvinnor"),
#                                      skickad_x_var = "bakgrundsvariabel",
#                                      skickad_y_var = "andel",
#                                      skickad_x_grupp = "kön",
#                                      # manual_x_axis_text_vjust=0.9,
#                                      manual_color = diagramfarger("kon"),
#                                      diagram_titel = diagramtitel,
#                                      diagram_capt =  diagram_capt,
#                                      manual_x_axis_title = "Vistelsetid i Sverige",
#                                      y_axis_100proc = TRUE,
#                                      diagram_facet = FALSE,
#                                      x_axis_sort_value = FALSE,
#                                      x_axis_lutning = 0,
#                                      manual_y_axis_title = "procent",
#                                      output_mapp = outputmapp_figur,
#                                      filnamn_diagram = diagramfilnamn,
#                                      skriv_till_diagramfil = spara_figur)
# 
# diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna."
# 
# diagramtitel <- paste0("Andel förvärvsarbetande 20-64 år bland utrikes födda i ",unique(etablering_df$region)," ",max(etablering_df$år)," efter vistelsetid")
# diagramtitel <- str_wrap(diagramtitel,60)
# diagramfilnamn <- paste0("etablering_Smedjebacken_utb.png")
# 
# etablering_fig_utb <- SkapaStapelDiagram(skickad_df =etablering_df %>%
#                                            filter(år == max(år),
#                                                   kön != "män och kvinnor",
#                                                   utbildningsnivå %in% c("utbildningsnivå: gymnasial utbildning" ,"utbildningsnivå: eftergymnasial utbildning")),
#                                          skickad_x_var = "bakgrundsvariabel", 
#                                          skickad_y_var = "andel", 
#                                          skickad_x_grupp = "kön",
#                                          manual_x_axis_title = "Vistelsetid i Sverige",
#                                          manual_color = diagramfarger("kon"),
#                                          diagram_titel = diagramtitel,
#                                          diagram_capt =  diagram_capt,
#                                          diagram_facet = TRUE,
#                                          facet_grp = "utbildningsnivå",
#                                          facet_legend_bottom = TRUE,
#                                          facet_scale = "fixed",
#                                          x_axis_sort_value = FALSE,
#                                          y_axis_100proc = TRUE,
#                                          x_axis_lutning = 0, 
#                                          manual_y_axis_title="procent",
#                                          output_mapp = outputmapp_figur,
#                                          filnamn_diagram = diagramfilnamn,
#                                          skriv_till_diagramfil = spara_figur)

# Skuldsättning

# Skuldsatta (Kronofogden)
source(here("Skript","skuldsatta.R"), encoding="UTF-8")
diag_kronofogden(spara_data = TRUE,
                 output_mapp = Output_mapp)

# Skuldsatta andel (Kronofogden)
source(here("Skript","skuldsatta_andel.R"), encoding="UTF-8")
diag_kronofogden_andel(spara_data = TRUE,
                       output_mapp = Output_mapp)

langsiktiga_skulder_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/skuldsatta.xlsx",sheet = 1)
skulder_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/skuldsatta.xlsx", sheet = 2)
#skulder_andel_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/skuldsatta_andel.xlsx")

diagram_capt <- c("Källa: Kronofogden.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Antal personer med skulder under indrivning hos Kronofogden",
                  "Källa: Kronofogden.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Skuldsatta personer som funnits i Kronofogdens register i mer än 20 år")

# Vill enbart fokusera på Dalarna och struntar initialt i kommunnivå
skulder_df_sum <- skulder_df %>%
  filter(Kommun=="SMEDJEBACKEN") %>%
  group_by(År,Kommun,Kön) %>%
  summarize(Antal_skuldsatta=sum(Antal.skuldsatta),
            Total_skuld=sum(Skuldbelopp),
            Skuld_per_person=Total_skuld/Antal_skuldsatta)

# Gör om skuldbelopp till miljarder
# miljard <- 1000000000
# skulder_df_sum$skuld_miljard <-skulder_df_sum$Total_skuld/miljard


diagramtitel <- paste0("Antal skuldsatta i Smedjebacken")
diagramfilnamn <- paste0("Antal_skuldsatta_Smedjebacken.png")

# Skapar diagram för medianinkomsten i Dalarna där män jämförs med kvinnor.
skuldsatta_fig <- SkapaStapelDiagram(skickad_df = skulder_df_sum,
                                     skickad_x_var = "År",
                                     skickad_y_var = "Antal_skuldsatta",
                                     skickad_x_grupp = "Kön",
                                     x_axis_lutning = 0,
                                     manual_color = diagramfarger("kon"),
                                     diagram_titel = diagramtitel,
                                     diagram_capt =  diagram_capt[1],
                                     diagram_facet = FALSE,
                                     stodlinjer_avrunda_fem = TRUE,
                                     manual_y_axis_title = "Antal skuldsatta",
                                     berakna_index = FALSE,
                                     output_mapp = outputmapp_figur,
                                     filnamn_diagram = diagramfilnamn,
                                     skriv_till_diagramfil = spara_figur)

diagram_capt <- "Källa: Kronofogden.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Andel av befolkningen i en viss åldersgrupp med skulder under indrivning hos Kronofogden."

# Fel. Är för hela Dalarna
# diagramtitel <- paste0("Andel skuldsatta i Smedjebacken år ",unique(skulder_andel_df$År))
# diagramfilnamn <- paste0("Andel_skuldsatta_Smedjebacken.png")
# 
# # Skapar diagram för medianinkomsten i Dalarna där män jämförs med kvinnor.
# skuldsatta_andel_fig <- SkapaStapelDiagram(skickad_df = skulder_andel_df,
#                                            skickad_x_var = "Ålder",
#                                            skickad_y_var = "skulder_andel_proc",
#                                            skickad_x_grupp = "kön",
#                                            x_axis_lutning = 0,
#                                            manual_color = diagramfarger("kon"),
#                                            diagram_titel = diagramtitel,
#                                            diagram_capt =  diagram_capt[1],
#                                            diagram_facet = FALSE,
#                                            stodlinjer_avrunda_fem = TRUE,
#                                            manual_y_axis_title = "procent",
#                                            berakna_index = FALSE,
#                                            output_mapp = outputmapp_figur,
#                                            filnamn_diagram = diagramfilnamn,
#                                            skriv_till_diagramfil = spara_figur)

diagram_capt <- c("Källa: Kronofogden.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Antal personer med skulder under indrivning hos Kronofogden",
                  "Källa: Kronofogden.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Skuldsatta personer som funnits i Kronofogdens register i mer än 20 år")

# Vill enbart fokusera på Dalarna och struntar initialt i kommunnivå
langsiktiga_skulder_df_sum <- langsiktiga_skulder_df %>%
  filter(Kommun=="SMEDJEBACKEN") %>% 
  group_by(År,Kommun,Kön)  %>% 
  summarize(Antal_skuldsatta=sum(Antal.personer))

diagramtitel <- paste0("Antal långsiktigt skuldsatta i Smedjebacken")
diagramfilnamn <- paste0("Antal_langsiktigt_skuldsatta_smedjebacken.png")

skuldsatta_lang <- SkapaStapelDiagram(skickad_df = langsiktiga_skulder_df_sum,
                                      skickad_x_var = "År",
                                      skickad_y_var = "Antal_skuldsatta",
                                      skickad_x_grupp = "Kön",
                                      x_axis_lutning = 0,
                                      manual_color = diagramfarger("kon"),
                                      diagram_titel = diagramtitel,
                                      diagram_capt =  diagram_capt[2],
                                      stodlinjer_avrunda_fem = TRUE,
                                      diagram_facet = FALSE,
                                      manual_y_axis_title = "Antal långsiktigt skuldsatta",
                                      berakna_index = FALSE,
                                      output_mapp = outputmapp_figur,
                                      filnamn_diagram = diagramfilnamn,
                                      skriv_till_diagramfil = spara_figur)

# Data från försäkringskassan

# Föräldrapenning och VAB
source(here("Skript","foraldrapenning.R"), encoding="UTF-8")
diag_foraldraforsakring(spara_data = TRUE,
                        filnamn = "foraldrapenning_Smedjebacken.xlsx",
                        output_mapp = Output_mapp)

foraldrapenning_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/foraldrapenning_Smedjebacken.xlsx",sheet = 1) %>% 
  mutate(Kommun = ifelse(Kommun == "20 Dalarnas län", "Samtliga kommuner",  Kommun))
vab_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/foraldrapenning_Smedjebacken.xlsx",sheet = 2) %>% 
  mutate(Kommun = ifelse(Kommun == "20 Dalarnas län", "Samtliga kommuner",  Kommun))

diagram_capt <- "Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna."
diagramtitel <- "Antal mottagare av föräldrapenning i Smedjebacken"
diagramfilnamn <- "Foraldrapenning_antal_Smedjebacken.png"

fp_fig <- SkapaStapelDiagram(skickad_df = foraldrapenning_df %>%
                               filter(Kommun == "2061 Smedjebacken",
                                      Kön != "Kvinnor och män") %>% 
                               mutate(Kön = tolower(Kön),
                                      Antal_mottagare = as.numeric(Antal_mottagare)), 
                             skickad_x_var = "År", 
                             skickad_y_var = "Antal_mottagare", 
                             skickad_x_grupp = "Kön",
                             x_axis_lutning = 45,
                             manual_x_axis_text_vjust=1,
                             manual_x_axis_text_hjust=1,
                             manual_y_axis_title = "Antal mottagare",
                             manual_color = diagramfarger("kon"),
                             stodlinjer_avrunda_fem = TRUE,
                             diagram_titel = diagramtitel,
                             diagram_capt =  diagram_capt,
                             diagram_facet = FALSE,
                             output_mapp = outputmapp_figur,
                             filnamn_diagram = diagramfilnamn,
                             skriv_till_diagramfil = spara_figur)

diagram_capt <- "Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna."
diagramtitel <- "Andel nettodagar (föräldrapenning) per kön i Smedjebacken"
diagramfilnamn <- "Foraldrapenning_andel_Smedjebacken.png"

fp_andel_fig <- SkapaStapelDiagram(skickad_df = foraldrapenning_df%>%
                                     filter(Kommun == "2061 Smedjebacken",
                                            Kön != "Kvinnor och män") %>% 
                                     mutate(Kön = tolower(Kön),
                                            Andel = as.numeric(Andel)), 
                                   skickad_x_var = "År", 
                                   skickad_y_var = "Andel", 
                                   skickad_x_grupp = "Kön",
                                   x_axis_lutning = 45,
                                   manual_x_axis_text_vjust=1,
                                   manual_x_axis_text_hjust=1,
                                   manual_color = diagramfarger("kon"),
                                   stodlinjer_avrunda_fem = TRUE,
                                   manual_y_axis_title = "",
                                   diagram_titel = diagramtitel,
                                   diagram_capt =  diagram_capt,
                                   diagram_facet = FALSE,
                                   output_mapp = outputmapp_figur,
                                   filnamn_diagram = diagramfilnamn,
                                   skriv_till_diagramfil = spara_figur)

diagram_capt <- "Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna."
diagramtitel <- "Vård av barn, antal uttagna nettodagar i Smedjebacken"
diagramfilnamn <- "vab_antal_Smedjebacken.png"

tfp_fig <- SkapaStapelDiagram(skickad_df = vab_df%>%
                                filter(Kommun == "2061 Smedjebacken",
                                       Kön != "Kvinnor och män",
                                       Delförmån == "Vård av barn") %>% 
                                mutate(Kön = tolower(Kön),
                                       Antal_nettodagar = as.numeric(Antal_nettodagar)), 
                              skickad_x_var = "År", 
                              skickad_y_var = "Antal_nettodagar", 
                              skickad_x_grupp = "Kön",
                              x_axis_lutning = 45,
                              manual_x_axis_text_vjust=1,
                              manual_x_axis_text_hjust=1,
                              manual_y_axis_title = "",
                              manual_color = diagramfarger("kon"),
                              diagram_titel = diagramtitel,
                              diagram_capt =  diagram_capt,
                              stodlinjer_avrunda_fem = TRUE,
                              diagram_facet = FALSE,
                              output_mapp = outputmapp_figur,
                              filnamn_diagram = diagramfilnamn,
                              skriv_till_diagramfil = spara_figur)

diagramtitel <- "Vård av barn, förändring i antal uttagna nettodagar i Smedjebacken"
diagramfilnamn <- "Vab_antal_linje_Smedjebacken.png"

tfp_fig_linje <- SkapaLinjeDiagram(skickad_df = vab_df%>%
                                     filter(Kommun == "2061 Smedjebacken",
                                            Kön != "Kvinnor och män",
                                            Delförmån == "Vård av barn") %>% 
                                     mutate(Kön = tolower(Kön),
                                            Antal_nettodagar = as.numeric(Antal_nettodagar),
                                            "år" = År), 
                                   skickad_x_var = "år", 
                                   skickad_y_var = "Antal_nettodagar", 
                                   skickad_x_grupp = "Kön",
                                   manual_color = diagramfarger("kon"),
                                   berakna_index = TRUE,
                                   x_axis_lutning = 45,
                                   diagram_titel = diagramtitel,
                                   diagram_capt =  diagram_capt,
                                   stodlinjer_avrunda_fem = TRUE,
                                   diagram_facet = FALSE,
                                   output_mapp = outputmapp_figur,
                                   filnamn_diagram = diagramfilnamn,
                                   skriv_till_diagramfil = spara_figur)

# Ohälsotal och sjukpenningtal
# source(here("Skript","ohalsotal_sjukpenningtal.R"), encoding="UTF-8")
# diag_ohalsa(spara_data = TRUE,
#             filnamn = "ohalsotal_sjukpenningtal_Smedjebacken.xlsx",
#             output_mapp = Output_mapp)

ohalsotal_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/ohalsotal_sjukpenningtal_Smedjebacken.xlsx",sheet = 1) %>% 
  mutate(Kommun = ifelse(Kommun == "20 Dalarnas län", "Samtliga kommuner",  Kommun)) %>% 
  filter(Ålder == "Samtliga 16-64 år")
sjukpenningtal_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/ohalsotal_sjukpenningtal_Smedjebacken.xlsx",sheet = 2)%>% 
  mutate(Kommun = ifelse(Kommun == "20 Dalarnas län", "Samtliga kommuner",  Kommun)) %>% 
  filter(Ålder == "Samtliga 16-64 år")

diagram_capt_ohälsa <- "Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Ohälsotalet: hur många dagar under en tolvmånadersperiod\nFörsäkringskassan betalar ut ersättning för nedsatt arbetsförmåga\ni förhållande till antalet försäkrade i åldrarna 16-64 år."

ohalsotal_df_utskrift <- ohalsotal_df %>%
  filter(Ålder == "Samtliga 16-64 år") %>%
  mutate(Ohälsotalet = as.numeric(Ohälsotalet)) %>% 
  group_by(År,Län,Kommun,Kön) %>% 
  summarize(Ohälsotalet_medel=mean(Ohälsotalet))

diagramtitel <- "Genomsnittligt ohälsotal (16-64 år) per år i Smedjebacken"
diagramfilnamn <- "ohalsotal_Smedjebacken.png"

ohalsotal_fig <- SkapaStapelDiagram(skickad_df = ohalsotal_df_utskrift %>%
                                      filter(Kommun == "2061 Smedjebacken",
                                             Kön != "Kvinnor och män") %>% 
                                      mutate(Kön = tolower(Kön)), 
                                    skickad_x_var = "År", 
                                    skickad_y_var = "Ohälsotalet_medel", 
                                    skickad_x_grupp = "Kön",
                                    x_axis_lutning = 45,
                                    manual_color = diagramfarger("kon"),
                                    manual_y_axis_title = "",
                                    diagram_titel = diagramtitel,
                                    diagram_capt =  diagram_capt_ohälsa,
                                    stodlinjer_avrunda_fem = TRUE,
                                    diagram_facet = FALSE,
                                    berakna_index = FALSE,
                                    output_mapp = outputmapp_figur,
                                    filnamn_diagram = diagramfilnamn,
                                    skriv_till_diagramfil = spara_figur)

diagram_capt_ohälsa <- "Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring: Ohälsotalet: hur många dagar under en tolvmånadersperiod\nFörsäkringskassan betalar ut ersättning för nedsatt arbetsförmåga\ni förhållande till antalet försäkrade i åldrarna 16-64 år."

diagramtitel <- paste0("Genomsnittligt ohälsotal (16-64 år) år ",max(ohalsotal_df_utskrift$År))
diagramfilnamn <- "ohalsotal_kommun_alla.png"

ohalsotal_kommun_fig <- SkapaStapelDiagram(skickad_df = ohalsotal_df_utskrift %>%
                                             filter(Kommun != "Samtliga kommuner",
                                                    Kön != "Kvinnor och män",
                                                    År == max(.$År)) %>% 
                                             mutate(Kön = tolower(Kön),
                                                    Kommun = stri_extract_last(Kommun, regex='\\S+')), 
                                           skickad_x_var = "Kommun", 
                                           skickad_y_var = "Ohälsotalet_medel", 
                                           skickad_x_grupp = "Kön",
                                           manual_x_axis_text_vjust = 1,
                                           manual_x_axis_text_hjust = 1,
                                           x_axis_lutning = 45,
                                           x_axis_sort_value = TRUE,
                                           x_axis_sort_grp = 1,
                                           vand_sortering = TRUE,
                                           manual_color = diagramfarger("kon"),
                                           manual_y_axis_title = "",
                                           diagram_titel = diagramtitel,
                                           diagram_capt =  diagram_capt_ohälsa,
                                           stodlinjer_avrunda_fem = TRUE,
                                           diagram_facet = FALSE,
                                           berakna_index = FALSE,
                                           output_mapp = outputmapp_figur,
                                           filnamn_diagram = diagramfilnamn,
                                           skriv_till_diagramfil = spara_figur)

diagram_capt_sjukpenning<-"Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna.\nDiagramförklaring:Sjukpenningtalet är antalet dagar med sjukpenning och rehabiliteringspenning\nsom har betalats ut under en 12-månaders period. Den summan delas med antalet försäkrade i\nSverige som är i åldrarna 16–64 år."

sjukpenningtal_df_utskrift <-sjukpenningtal_df %>%
  filter(Ålder == "Samtliga 16-64 år") %>%
  mutate(Sjukpenningtal = as.numeric(Sjukpenningtal.1.0)) %>% 
  group_by(År,Län,Kommun,Kön) %>% 
  summarize(Sjukpenningtal_medel=mean(Sjukpenningtal))

diagramtitel <- "Genomsnittligt sjukpenningtal (16-64 år) per år i Smedjebacken"
diagramfilnamn <- "sjukpenningtal_Smedjebacken.png"

sjukpenningtal_fig <- SkapaStapelDiagram(skickad_df = sjukpenningtal_df_utskrift %>%
                                           filter(Kommun == "2061 Smedjebacken",
                                                  Kön != "Kvinnor och män") %>% 
                                           mutate(Kön = tolower(Kön)), 
                                         skickad_x_var = "År", 
                                         skickad_y_var = "Sjukpenningtal_medel", 
                                         skickad_x_grupp = "Kön",
                                         x_axis_lutning = 45,
                                         manual_color = diagramfarger("kon"),
                                         manual_y_axis_title = "",
                                         diagram_titel = diagramtitel,
                                         diagram_capt =  diagram_capt_sjukpenning,
                                         stodlinjer_avrunda_fem = TRUE,
                                         diagram_facet = FALSE,
                                         berakna_index = FALSE,
                                         output_mapp = outputmapp_figur,
                                         filnamn_diagram = diagramfilnamn,
                                         skriv_till_diagramfil = spara_figur)

diagramtitel <- paste0("Genomsnittligt sjukpenningtal (16-64 år) år ",max(sjukpenningtal_df_utskrift$År))
diagramfilnamn <- "sjukpenningtal_kommun_alla.png"

sjukpenningtal_kommun_fig <- SkapaStapelDiagram(skickad_df = sjukpenningtal_df_utskrift %>%
                                                  filter(Kommun != "Samtliga kommuner",
                                                         Kön != "Kvinnor och män",
                                                         År == max(.$År)) %>% 
                                                  mutate(Kön = tolower(Kön),
                                                         Kommun = stri_extract_last(Kommun, regex='\\S+')), 
                                                skickad_x_var = "Kommun", 
                                                skickad_y_var = "Sjukpenningtal_medel", 
                                                skickad_x_grupp = "Kön",
                                                manual_x_axis_text_vjust = 1,
                                                manual_x_axis_text_hjust = 1,
                                                x_axis_lutning = 45,
                                                x_axis_sort_value = TRUE,
                                                x_axis_sort_grp = 1,
                                                vand_sortering = TRUE,
                                                manual_color = diagramfarger("kon"),
                                                stodlinjer_avrunda_fem = TRUE,
                                                manual_y_axis_title = "",
                                                diagram_titel = diagramtitel,
                                                diagram_capt =  diagram_capt_sjukpenning,
                                                diagram_facet = FALSE,
                                                berakna_index = FALSE,
                                                output_mapp = outputmapp_figur,
                                                filnamn_diagram = diagramfilnamn,
                                                skriv_till_diagramfil = spara_figur)

source(here("Skript","sjukfall_stress.R"), encoding="UTF-8")
diag_stress(output_mapp = Output_mapp,
            filnamn = "sjukfall_stress_Smedjebacken.xlsx",
            spara_data = TRUE)

stress_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/sjukfall_stress_Smedjebacken.xlsx")

diagram_capt<-"Källa: Försäkringskassan.\nBearbetning: Samhällsanalys, Region Dalarna."

stress_df_utskrift <- stress_df %>%
  filter(Kommun == "2061 Smedjebacken",
         År>"2013") %>% 
  mutate(Antal = as.numeric(Antal)) %>% 
  group_by(År,Län,Kommun,Kön) %>% 
  summarize(Antal_medel=mean(Antal))

diagramtitel <- paste0("Genomsnittligt antal pågående sjukfall kopplade till stress i Smedjebacken")
diagramtitel <- str_wrap(diagramtitel,60)
diagramfilnamn <- paste0("Antal_stress_smedjebacken.png")

stress_fig <- SkapaStapelDiagram(skickad_df = stress_df_utskrift %>% 
                                   filter(Kön != "Kvinnor och män") %>% 
                                   mutate(Kön = tolower(Kön)), 
                                 skickad_x_var = "År", 
                                 skickad_y_var ="Antal_medel", 
                                 skickad_x_grupp = "Kön",
                                 x_axis_lutning = 0,
                                 manual_y_axis_title = "",
                                 manual_color = diagramfarger("kon"),
                                 stodlinjer_avrunda_fem = TRUE,
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt,
                                 diagram_facet = FALSE,
                                 berakna_index = FALSE,
                                 output_mapp = outputmapp_figur,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = spara_figur)

source("C:/Users/frkjon/Projekt/kvinnor_man_i_Dalarna/Skript/hamta_etableringstid_mm_kommun_region_kon_utbniv_bakgrvar_tid_IntGr1KomKonUtb_scb.R")
chefer_df = hamta_etableringstid_mm_kommun_region_kon_utbniv_bakgrvar_tid_scb(region_vekt = "2061",
                                                                           cont_klartext =  "Andel i chefsposition, procent",
                                                                           bakgrvar_klartext = "samtliga 20-64 år") %>%
  rename(andel = `Andel i chefsposition, procent`)

diagram_capt <- c("Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Andel chefer av sysselsatta med ett klassificerat yrke (20-64 år) uppdelat på utbildningsnivå",
                  "Källa: SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna\nDiagramförklaring: Förändring i andelen chefer av sysselsatta med ett klassificerat yrke (20-64 år) för samtliga utbildningsnivåer")
diagramtitel <- paste0("Andel chefer år ",max(chefer_df$år)," i ",unique(chefer_df$region))
diagramfilnamn <- paste0("andel_chefer_Smedjebacken.png")

chefer_fig <- SkapaStapelDiagram(skickad_df =chefer_df %>% 
                                   filter(år == max(år),
                                          utbildningsnivå != "utbildningsnivå: uppgift saknas",
                                          kön != "män och kvinnor"),
                                 skickad_x_var = "utbildningsnivå",
                                 skickad_y_var = "andel",
                                 skickad_x_grupp = "kön",
                                 manual_x_axis_text_vjust=1,
                                 manual_x_axis_text_hjust=1,
                                 manual_color = diagramfarger("kon"),
                                 diagram_titel = diagramtitel,
                                 diagram_capt =  diagram_capt[1],
                                 stodlinjer_avrunda_fem = TRUE,
                                 diagram_facet = FALSE,
                                 x_axis_sort_value = TRUE,
                                 manual_y_axis_title="procent",
                                 output_mapp = outputmapp_figur,
                                 filnamn_diagram = diagramfilnamn,
                                 skriv_till_diagramfil = spara_figur)

diagramtitel <- paste0("Förändring i andel chefer i ",unique(chefer_df$region))
diagramfilnamn <- paste0("andel_chefer_linje_Smedjebacken.png")

chefer_linje_fig <- SkapaLinjeDiagram(skickad_df =chefer_df %>% 
                                        filter(år >"2000",
                                               utbildningsnivå == "samtliga utbildningsnivåer",
                                               kön != "män och kvinnor"),
                                      skickad_x_var = "år",
                                      skickad_y_var = "andel",
                                      skickad_x_grupp = "kön",
                                      x_axis_lutning = 45,
                                      manual_color = diagramfarger("kon"),
                                      diagram_titel = diagramtitel,
                                      diagram_capt =  diagram_capt[2],
                                      stodlinjer_avrunda_fem = TRUE,
                                      diagram_facet = FALSE,
                                      berakna_index = TRUE,
                                      output_mapp = outputmapp_figur,
                                      filnamn_diagram = diagramfilnamn,
                                      skriv_till_diagramfil = spara_figur)

diagram_capt <- "Källa: SCB:s öppna statistikdatabas.\n Bearbetning: Samhällsanalys, Region Dalarna."

kommunval_df <- read.xlsx("G:/skript/projekt/data/kvinnor_man/val.xlsx", sheet = 3)

# Summerar riksdagsledamöter på år
kommunval_summering_df <- kommunval_df %>%
  filter(valår==max(valår))%>%
  group_by(region,kön) %>%
  summarise("Antal" = sum(`Kommunfullmäktigledamöter`))

diagramtitel <- paste0("Kommunfullmäktigeledamöter i Dalarna ",max(kommunval_df$valår))
diagramfilnamn <- "kommunfullmäktige_.png"

kommunval_fig <- SkapaStapelDiagram(skickad_df = kommunval_summering_df,
                                    skickad_x_var = "region",
                                    skickad_y_var = "Antal",
                                    skickad_x_grupp = "kön",
                                    manual_x_axis_text_vjust = 1,
                                    manual_x_axis_text_hjust = 1,
                                    #skickad_filter_OR_var = "region",
                                    manual_color = diagramfarger("kon"),
                                    diagram_titel = diagramtitel,
                                    diagram_capt =  diagram_capt,
                                    stodlinjer_avrunda_fem = TRUE,
                                    diagram_facet = FALSE,
                                    berakna_index = FALSE,
                                    output_mapp = outputmapp_figur,
                                    filnamn_diagram = diagramfilnamn,
                                    skriv_till_diagramfil = spara_figur)

