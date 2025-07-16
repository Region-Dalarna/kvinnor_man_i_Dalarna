
# Skall data uppdateras? Annars läses data in från en sparad global environment-fil. Därefter kan man knitta rapporten baserat på de data som fanns vid senaste uppdatering av data.
# Detta är bra om man exempelvis vill ändra slarvfel eller liknande, utan att data skall uppdateras.
# Notera dock att ett flertal diagram skapas via Excelfiler (se skriptet hamta_data). Uppdatera data i detta fall åsyftar enbart de diagram-funktioner som körs nedan
uppdatera_data = FALSE

if(uppdatera_data == TRUE){
  
  cat("Hämtning av data påbörjad\n\n")
  start_time <- Sys.time()


if (!require("pacman")) install.packages("pacman")
p_load(here,
       tidyverse)

output_mapp_figur = here("Diagram","/")

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

# Utbildningsnivå 85 - senaste år och jämförelse mellan länen för senaste år.
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diag_utbniva_flera_diagram_scb.R")
gg_utbniva_85 <- diag_utbniva_tidserie_och_lansjmfr (region_vekt = c("20"),
                                                     output_mapp = output_mapp_figur,
                                                     diagram_capt = "Källa: SCB:s öppna statistikdatabas.\nBearbetning: Samhällsanalys, Region Dalarna",
                                                     skapa_fil = TRUE,
                                                     diag_hogutb_over_tid = TRUE,
                                                     diag_lagutb_over_tid = FALSE,
                                                     diag_andel_alla_utbnivaer = FALSE,
                                                     diag_andel_utbniva_jmfr_lan = TRUE,
                                                     vald_utb_niva = "hogutb")

# Ovan saknar ett funktion som returnerar data till global environment, så jag hämtar detta från ggplot-objektet
utbildning_85_df <- gg_utbniva_85$hogutb_andel_ar20_1985_2024$data

utb_85_senaste_ar <- max(utbildning_85_df$år)
utb_85_senaste_andel_kv <- round(utbildning_85_df %>% filter(kön == "kvinnor",år == utb_85_senaste_ar) %>% .$total,0)
utb_85_senaste_andel_man <- round(utbildning_85_df %>% filter(kön == "män",år == utb_85_senaste_ar) %>% .$total,0)

# Utbildningsnivå, senaste år, flera kategorier
source(here("skript/","diagram_utb_niva_senaste_ar.R"))
gg_utb_niva_senaste <- diag_utbniva_senaste(region_vekt = "20", 
                                            output_mapp = output_mapp_figur,
                                            spara_figur = TRUE,
                                            returnera_data = TRUE)

utbildning_senaste <- max(utbildning_df$år)
vanligast_kvinnor_andel <- round(utbildning_df %>% filter(region == "Dalarna",kön == "kvinnor") %>% filter(andel == max(andel)) %>% .$andel,0)
vanligast_kvinnor_utbildning <- utbildning_df %>% filter(region == "Dalarna",kön == "kvinnor") %>% filter(andel == max(andel)) %>% .$utbildningsnivå
eftergym_andel_man <- round(utbildning_df %>% filter(region == "Dalarna",kön == "män",utbildningsnivå == "eftergymnasial utbildning, 3 år eller mer")  %>% .$andel,0)
vanligast_man_andel <- round(utbildning_df %>% filter(region == "Dalarna",kön == "män") %>% filter(andel == max(andel)) %>% .$andel,0)
vanligast_man_utbildning <- utbildning_df %>% filter(region == "Dalarna",kön == "män") %>% filter(andel == max(andel)) %>% .$utbildningsnivå
eftergym_tot_kvinnor <- round(utbildning_df %>% filter(region == "Dalarna",kön == "kvinnor") %>% filter(andel == max(andel)) %>% .$andel + utbildning_df %>% filter(region == "Dalarna",kön == "kvinnor",utbildningsnivå == "eftergymnasial utbildning, mindre än 3 år")  %>% .$andel,0)
eftergym_tot_man <- round(utbildning_df %>% filter(region == "Dalarna",kön == "män", utbildningsnivå == "eftergymnasial utbildning, 3 år eller mer") %>% .$andel + utbildning_df %>% filter(region == "Dalarna",kön == "män",utbildningsnivå == "eftergymnasial utbildning, mindre än 3 år")  %>% .$andel,0)
gym_max_2_ar_kvinnor <- round(utbildning_df %>% filter(region == "Dalarna",kön == "kvinnor",utbildningsnivå == "gymnasial utbildning, högst 2 år")  %>% .$andel,0)
gym_max_2_ar_man <- round(utbildning_df %>% filter(region == "Dalarna",kön == "män",utbildningsnivå == "gymnasial utbildning, högst 2 år")  %>% .$andel,0)
gym_tot_kvinnor <- round(utbildning_df %>% filter(region == "Dalarna",kön == "kvinnor", utbildningsnivå == "gymnasial utbildning, 3 år") %>% .$andel + utbildning_df %>% filter(region == "Dalarna",kön == "kvinnor",utbildningsnivå == "gymnasial utbildning, högst 2 år")  %>% .$andel,0)
gym_tot_man <- round(utbildning_df %>% filter(region == "Dalarna",kön == "män", utbildningsnivå == "gymnasial utbildning, 3 år") %>% .$andel + utbildning_df %>% filter(region == "Dalarna",kön == "män",utbildningsnivå == "gymnasial utbildning, högst 2 år")  %>% .$andel,0)

# De tio största yrkena för män respektive kvinnor
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diag_yrke_ssyk3_storsta_per_geografi.R")
yrken_konsfordelning <- diag_storsta_yrke_per_geografi(facet_ovanpa_varandra = TRUE, storre_text = TRUE,returnera_dataframe_global_environment = TRUE,antal_yrken = 10)

yrke_storst_kvinnor <- storsta_yrken_per_geografi_df %>% filter(kön == "kvinnor",Antal == max(Antal)) %>% .$yrke
yrke_storst_kvinnor_antal <- storsta_yrken_per_geografi_df %>% filter(kön == "kvinnor",Antal == max(Antal)) %>% .$Antal
yrke_nast_storst_kvinnor <- tolower(storsta_yrken_per_geografi_df %>% filter(kön == "kvinnor", yrke != yrke_storst_kvinnor) %>% filter(Antal == max(Antal))  %>%  .$yrke)

yrke_storst_man <- storsta_yrken_per_geografi_df %>% filter(kön == "män") %>% filter(Antal == max(Antal)) %>% .$yrke
yrke_storst_man_antal <- storsta_yrken_per_geografi_df %>% filter(kön == "män") %>% filter(Antal == max(Antal)) %>% .$Antal
yrke_nast_storst_man <- tolower(storsta_yrken_per_geografi_df %>% filter(kön == "män", yrke != yrke_storst_man) %>% filter(Antal == max(Antal))  %>%  .$yrke)

butikspersonal_kvinnor <- format(plyr::round_any(storsta_yrken_per_geografi_df %>% filter(kön == "kvinnor",yrke == "Butikspersonal") %>% .$Antal,100),big.mark=" ")
butikspersonal_man <- format(plyr::round_any(storsta_yrken_per_geografi_df %>% filter(kön == "män",yrke == "Butikspersonal") %>% .$Antal,100),big.mark=" ")

tio_storsta_sum_kvinnor <- format(plyr::round_any(storsta_yrken_per_geografi_df %>% filter(kön == "kvinnor") %>%  .$Antal %>% sum(),100),big.mark=" ")
tio_storsta_sum_man <- format(plyr::round_any(storsta_yrken_per_geografi_df %>% filter(kön == "män") %>%  .$Antal %>% sum(),100),big.mark=" ")

source("https://raw.githubusercontent.com/Region-Dalarna/diagram/main/diagram_andel_forvarvsarbetande_bransch.R", encoding="UTF-8")
gg_antal_forv <- diag_sysselsatta_andel(output_mapp_figur = output_mapp_figur,
                                        returnera_figur = TRUE,
                                        spara_figur = TRUE, 
                                        diag_lan = FALSE, 
                                        diag_kommun = FALSE,
                                        caption = "Källa: BAS i SCB:s öppna statistikdatabas\nBearbetning: Samhällsanalys, Region Dalarna",
                                        diag_lan_antal = TRUE, # Antal för länet, uppdelat på kvinnor och män
                                        returnera_data = TRUE)

forvarvsarbetande_bygg_andel <- antal_forvarvsarbetande_bransch %>% filter(bransch=="Bygg") %>% pivot_wider(names_from = kön, values_from = Antal) %>%  mutate(andel= (kvinnor/(män+kvinnor))*100) %>% .$andel %>% round(0)
forvarvsarbetande_vard_andel <- antal_forvarvsarbetande_bransch %>% filter(bransch=="Vård och omsorg") %>% pivot_wider(names_from = kön, values_from = Antal) %>%  mutate(andel= (kvinnor/(män+kvinnor))*100) %>% .$andel %>% round(0)


# Inkomst c("20-64 år","65+ år")
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diagram_inkomst_region_aldersgrupper_kv_man.R")
gg_inkomst <- diag_inkomst_scb(regionvekt = "20", # Enbart ett i taget. går även att välja kommuner, men då genereras inget kommundiagram
                               output_mapp = output_mapp_figur,                                  # mapp där diagram ska sparas, NA = sparas ingen fil
                               inkomst_typ = "Medianinkomst, tkr", # Finns "Medianinkomst, tkr", "Medelinkomst, tkr". Max 1 åt gången
                               diag_tid = TRUE,
                               diag_linje = TRUE,
                               diag_kommun = TRUE,
                               skriv_diagrambildfil = FALSE,                           # TRUE om diagram ska skrivas till fil, FALSE om diagram inte ska skrivas till fil
                               alder_klartext = c("20-64 år","65+ år"),			 #  Finns: "20+ år", "20-64 år", "20-65 år", "65+ år", "66+ år". OBS!! Funkar ej med "*"
                               returnera_data_rmarkdown = TRUE
)

inkomst_max_ar <- max(forvarvsinkomst_df$år)

# Län 20-64 år
medianinkomst_man_max_20_64 <- round(forvarvsinkomst_df %>% filter(region == "Dalarna", år == max(år),kön == "män",ålder == "20-64 år") %>% .$`Medianinkomst, tkr`,0)
medianinkomst_kvinna_max_20_64 <- round(forvarvsinkomst_df %>% filter(region == "Dalarna", år == max(år),kön == "kvinnor",ålder == "20-64 år") %>% .$`Medianinkomst, tkr`,0)
medianinkomst_man_forandring_20_64 <- round((forvarvsinkomst_df %>% filter(region == "Dalarna", år == max(år),kön == "män",ålder == "20-64 år") %>% .$`Medianinkomst, tkr`/forvarvsinkomst_df %>% filter(region == "Dalarna", år == min(år),kön == "män",ålder == "20-64 år") %>% .$`Medianinkomst, tkr`-1)*100,0)
medianinkomst_kvinna_forandring_20_64 <- round((forvarvsinkomst_df %>% filter(region == "Dalarna", år == max(år),kön == "kvinnor",ålder == "20-64 år") %>% .$`Medianinkomst, tkr`/forvarvsinkomst_df %>% filter(region == "Dalarna", år == min(år),kön == "kvinnor",ålder == "20-64 år") %>% .$`Medianinkomst, tkr`-1)*100,0)

# Kommun 20-64 år
# Högst
medianinkomst_kommun_max_kvinnor <- forvarvsinkomst_df %>% filter(kön == "kvinnor", år == max(år),ålder == "20-64 år") %>% filter(`Medianinkomst, tkr` == max(`Medianinkomst, tkr`)) %>%  .$region %>%  glue_collapse(sep = ", ", last = " och ")
medianinkomst_kommun_max_kvinnor_varde <- round(forvarvsinkomst_df %>% filter(kön == "kvinnor", år == max(år),ålder == "20-64 år") %>% filter(`Medianinkomst, tkr` == max(`Medianinkomst, tkr`)) %>%  .$`Medianinkomst, tkr` %>% first(),0)
medianinkomst_kommun_max_man <- forvarvsinkomst_df %>% filter(kön == "män", år == max(år),ålder == "20-64 år") %>% filter(`Medianinkomst, tkr` == max(`Medianinkomst, tkr`)) %>%  .$region %>%  glue_collapse(sep = ", ", last = " och ")
medianinkomst_kommun_max_man_varde <- round(forvarvsinkomst_df %>% filter(kön == "män", år == max(år),ålder == "20-64 år") %>% filter(`Medianinkomst, tkr` == max(`Medianinkomst, tkr`)) %>%  .$`Medianinkomst, tkr` %>% first(),0)

# Lägst
medianinkomst_kommun_min_kvinnor <- forvarvsinkomst_df %>% filter(kön == "kvinnor", år == max(år),ålder == "20-64 år") %>% filter(`Medianinkomst, tkr` == min(`Medianinkomst, tkr`)) %>%  .$region %>%  glue_collapse(sep = ", ", last = " och ")
medianinkomst_kommun_min_kvinnor_varde <- round(forvarvsinkomst_df %>% filter(kön == "kvinnor", år == max(år),ålder == "20-64 år") %>% filter(`Medianinkomst, tkr` == min(`Medianinkomst, tkr`)) %>%  .$`Medianinkomst, tkr` %>% first(),0)
medianinkomst_kommun_min_man <- forvarvsinkomst_df %>% filter(kön == "män", år == max(år),ålder == "20-64 år") %>% filter(`Medianinkomst, tkr` == min(`Medianinkomst, tkr`)) %>%  .$region %>%  glue_collapse(sep = ", ", last = " och ")
medianinkomst_kommun_min_man_varde <- round(forvarvsinkomst_df %>% filter(kön == "män", år == max(år),ålder == "20-64 år") %>% filter(`Medianinkomst, tkr` == min(`Medianinkomst, tkr`)) %>%  .$`Medianinkomst, tkr` %>% first(),0)

# Län 65+
medianinkomst_man_max_65 <- round(forvarvsinkomst_df %>% filter(region == "Dalarna", år == max(år),kön == "män",ålder == "65+ år") %>% .$`Medianinkomst, tkr`,0)
medianinkomst_kvinna_max_65 <- round(forvarvsinkomst_df %>% filter(region == "Dalarna", år == max(år),kön == "kvinnor",ålder == "65+ år") %>% .$`Medianinkomst, tkr`,0)
medianinkomst_man_forandring_65 <- round((forvarvsinkomst_df %>% filter(region == "Dalarna", år == max(år),kön == "män",ålder == "65+ år") %>% .$`Medianinkomst, tkr`/forvarvsinkomst_df %>% filter(region == "Dalarna", år == min(år),kön == "män",ålder == "65+ år") %>% .$`Medianinkomst, tkr`-1)*100,0)
medianinkomst_kvinna_forandring_65 <- round((forvarvsinkomst_df %>% filter(region == "Dalarna", år == max(år),kön == "kvinnor",ålder == "65+ år") %>% .$`Medianinkomst, tkr`/forvarvsinkomst_df %>% filter(region == "Dalarna", år == min(år),kön == "kvinnor",ålder == "65+ år") %>% .$`Medianinkomst, tkr`-1)*100,0)

source(here("skript/","diagram_disponibelinkomst.R"))
gg_disponibel_inkomst <- diag_disponibelinkomst(region_vekt = "20", 
                                               output_mapp = output_mapp_figur,
                                               returnera_data = TRUE,
                                               spara_figur = TRUE,
                                               alder = "18+ år") 

disponibel_inkomst_min_ar <- min(disponibel_inkomst_df$år)
disponibel_inkomst_max_ar <- max(disponibel_inkomst_df$år)

# Skuldsatta
source(here("skript","diagram_skuldsatta_kronofogden.R"))
gg_skuldsatta <- diag_kronofogden(output_mapp = output_mapp_figur,
                                  diag_langsiktiga_skulder = TRUE, # Om diagram för långsiktigt skuldsatta ska skapas
                                  diag_skulder = TRUE, # Om diagram för skuldsatta ska skapas,
                                  diag_andel_skuldsatta = TRUE,
                                  spara_figur =TRUE,
                                  returnera_data = TRUE)

# Antal skuldsatta
antal_skuldsatta_totalt <- format(plyr::round_any(skulder_df_sum %>% filter(Kön == "kvinnor",År == max(.$År)) %>% .$Antal_skuldsatta + skulder_df_sum %>% filter(Kön == "män",År == max(.$År)) %>% .$Antal_skuldsatta,100),big.mark=" ") 
antal_skuldsatta_kvinnor <- skulder_df_sum %>% filter(Kön == "kvinnor",År == max(.$År)) %>% .$Antal_skuldsatta
antal_skuldsatta_man <- skulder_df_sum %>% filter(Kön == "män",År == max(.$År)) %>% .$Antal_skuldsatta
andel_skuldsatta_man <- round((antal_skuldsatta_man/(antal_skuldsatta_kvinnor+antal_skuldsatta_man))*100,0)
andel_skuldsatta_kvinnor = 100- andel_skuldsatta_man

# Åldersintervall
andel_skulder_max_ar <- max(skulder_andel_df$År)
hogst_andel_alder_grupp <- skulder_andel_df %>% filter(kön == "kvinnor") %>% filter(skulder_andel_proc==max(skulder_andel_proc)) %>% .$Ålder
hogst_andel_alder_varde_man <- gsub("\\.",",",round(skulder_andel_df %>% filter(kön == "män") %>% filter(skulder_andel_proc==max(skulder_andel_proc)) %>% .$skulder_andel_proc,1))
hogst_andel_alder_varde_kvinna <- gsub("\\.",",",round(skulder_andel_df %>% filter(kön == "kvinnor") %>% filter(Ålder==hogst_andel_alder_grupp) %>% .$skulder_andel_proc,1))

# Långsiktigt skuldsatta
langsiktiga_ar_min <- min(langsiktiga_skulder_df_sum$År)
langsiktiga_ar_max <- max(langsiktiga_skulder_df_sum$År)

lansiktiga_antal_min_man <- langsiktiga_skulder_df_sum %>% filter(År==min(.$År),Kön=="män") %>% .$Antal_skuldsatta
lansiktiga_antal_max_man <- langsiktiga_skulder_df_sum %>% filter(År==max(.$År),Kön=="män") %>% .$Antal_skuldsatta
forandring_man <- round((1-(lansiktiga_antal_max_man/lansiktiga_antal_min_man))*100,0)

lansiktiga_antal_min_kvinna <- langsiktiga_skulder_df_sum %>% filter(År==min(.$År),Kön=="kvinnor") %>% .$Antal_skuldsatta
lansiktiga_antal_max_kvinna <- langsiktiga_skulder_df_sum %>% filter(År==max(.$År),Kön=="kvinnor") %>% .$Antal_skuldsatta
forandring_kvinna <- round(((lansiktiga_antal_max_kvinna/lansiktiga_antal_min_kvinna)-1)*100,0)

# Etablering på arbetsmarknaden
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diag_etableringstid_kon_lan_tidsserie_KvMa_IntRap.R")
#source(here("skript/","etablering_kon_utbildningsniva.R"))
gg_etablering <- diag_etablering_utb_kon_scb(output_mapp = output_mapp_figur,
                                             utbildningsniva_jmf = rev(c("utbildningsnivå: förgymnasial utbildning","utbildningsnivå: gymnasial utbildning" ,"utbildningsnivå: eftergymnasial utbildning")), # Finns även "samtliga utbildningsnivåer", "utbildningsnivå: förgymnasial utbildning", Skriv i den ordning de skall visas i diagram
                                             facet_kolumner = 2,# Välj antalet kolumner som skall visas i Facet-diagramet (diag_utbildning)
                                             skriv_diagrambildfil = TRUE,
                                             returnera_data_rmarkdown = TRUE)

eftergym_0_1_kvinna <- round(etablering_df %>% filter(kön == "kvinnor",bakgrundsvariabel == "0-1 år",år == max(år),utbildningsnivå == "utbildningsnivå: eftergymnasial utbildning") %>% .$andel,0)
eftergym_0_1_man <- round(etablering_df %>% filter(kön == "män",bakgrundsvariabel == "0-1 år",år == max(år),utbildningsnivå == "utbildningsnivå: eftergymnasial utbildning") %>% .$andel,0)
eftergym_10_kvinna <- round(etablering_df %>% filter(kön == "kvinnor",bakgrundsvariabel == "10- år",år == max(år),utbildningsnivå == "utbildningsnivå: eftergymnasial utbildning") %>% .$andel,0)
samtliga_inrikes_kvinna <- round(etablering_df %>% filter(kön == "kvinnor",bakgrundsvariabel == "Inrikes född",år == max(år),utbildningsnivå == "samtliga utbildningsnivåer") %>% .$andel,0)
gym_0_1_kvinna <- round(etablering_df %>% filter(kön == "kvinnor",bakgrundsvariabel == "0-1 år",år == max(år),utbildningsnivå == "utbildningsnivå: gymnasial utbildning") %>% .$andel,0)
gym_0_1_man <-  round(etablering_df %>% filter(kön == "män",bakgrundsvariabel == "0-1 år",år == max(år),utbildningsnivå == "utbildningsnivå: gymnasial utbildning") %>% .$andel,0)
forgym_0_1_kvinna <- round(etablering_df %>% filter(kön == "kvinnor",bakgrundsvariabel == "0-1 år",år == max(år),utbildningsnivå == "utbildningsnivå: förgymnasial utbildning") %>% .$andel,0)
forgym_0_1_man <- round(etablering_df %>% filter(kön == "män",bakgrundsvariabel == "0-1 år",år == max(år),utbildningsnivå == "utbildningsnivå: förgymnasial utbildning") %>% .$andel,0)

# Matchning
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diagram_matchning_lan_bakgrund.R")
gg_matchning <- diag_matchning_lan(region_vekt = "20",
                                   output_mapp_figur = output_mapp_figur,
                                   spara_figur = TRUE,
                                   returnera_data = TRUE,
                                   kon_klartext = "*")

# Föräldrapenning och VAB
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diagram_foraldrapenning_vab_kv_man.R")
gg_fp_vab <- diag_foraldrapenning_vab(region_vekt = "20",
                                      output_mapp = output_mapp_figur,
                                      diag_foraldrapenning = TRUE,
                                      diag_vab = TRUE,
                                      spara_diagrambildfil = TRUE,
                                      spara_dataframe_till_global_environment = TRUE)

# VaB - månadsbasis
source(here("Skript","fp_vab_manad.R"))
gg_forsakringskassan <- diag_foraldrapenning_manad(region_vekt = "20",
                                                   output_mapp = output_mapp_figur,
                                                   diag_foraldrapenning = TRUE,
                                                   diag_vab = FALSE,
                                                   spara_diagrambildfil = TRUE,
                                                   spara_dataframe_till_global_environment = TRUE)

# Sjukpenningtal och ohälsotal
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diagram_ohalsotal_sjukpenningtal_kv_man.R")
gg_ohalsotal_sjp <- diag_ohalsotal_sjukpenningtal (region_vekt = "20",
                                                   output_mapp = output_mapp_figur,
                                                   diag_ohalsotal = TRUE,
                                                   diag_sjukpenningtal = TRUE,
                                                   spara_diagrambildfil = TRUE,
                                                   spara_dataframe_till_global_environment = TRUE)


# Sjukfall kopplade till stress
source(here("Skript","diagram_sjukfall_stress.R"))
gg_sjukfall_stress <- diag_sjukfall_stress (region_vekt = "20",
                                            output_mapp = output_mapp_figur,
                                            spara_diagrambildfil = TRUE,
                                            spara_dataframe_till_global_environment = TRUE)

andel_kvinnor_stress <- round(stress_df %>% filter(Kön=="Kvinnor",År==max(.$År)) %>% 
                                .$Antal_medel/stress_df %>% filter(Kön=="Kvinnor och män",År==max(.$År)) %>% 
                                .$Antal_medel,2)*100

# Sjukfall på branschnivå - kräver att data laddas hem manuellt. För mer info, se diagramskriptet
source(here("Skript","diagram_sjukfall_bransch.R"))
gg_sjukfall_bransch <- diag_sjukfall_bransch(output_mapp = output_mapp_figur,
                                             spara_figur = TRUE,
                                             spara_dataframe_till_global_environment = TRUE)

bransch_max_kvinnor <- tolower(startade_sjukfall_bransch_df %>% filter(Kon == "Kvinnor") %>% filter(Antal_startade_sjukfall == max(Antal_startade_sjukfall)) %>% .$SNI2007 %>%  glue_collapse(sep = ", ", last = " och "))
bransch_max_man <- tolower(startade_sjukfall_bransch_df %>% filter(Kon == "Män") %>% filter(Antal_startade_sjukfall == max(Antal_startade_sjukfall)) %>% .$SNI2007 %>%  glue_collapse(sep = ", ", last = " och "))

# source(here("Skript","diagram_arbesloshet_tidsserie_BAS.R"))
# gg_arbetsloshet_tidsserie_bas <- diag_arbetsloshet_tidsserie(spara_diagrambildfil = FALSE,
#                                                              spara_dataframe_till_global_environment = FALSE,
#                                                              output_mapp = output_mapp_figur)


##########################
## Arbetsmarknadsstatus ##
##########################


##########
## Län ##
#########
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diagram_arbetsmarknadsstatus_senastear.R")
gg_diagram_arbetsmarknadsstatus_lan <- diagram_arbetsmarknadsstatus(region_vekt = hamtaAllaLan(),
                                                                    kon_klartext = c("kvinnor","män"),
                                                                    alder_klartext = "20-64 år",
                                                                    valda_farger = diagramfarger("kon"),
                                                                    fodelseregion_klartext_vekt = c("inrikes född", "utrikes född"),
                                                                    diag_arbetskraftsdeltagande = FALSE,
                                                                    returnera_data = TRUE,
                                                                    spara_figur = FALSE,
                                                                    data_namm = "arbetsmarknadsstatus_lan_df")

# Variabler sysselsättningsgrad län
arbetsloshesstatus_manad <- unique(arbetsmarknadsstatus_lan_df$manad_long)
arbetsloshesstatus_ar <- unique(arbetsmarknadsstatus_lan_df$ar)


# Sysselsättningsgrad inrikes Sverige
syssgrad_inrikes_man_Sverige <- gsub("\\.",",",arbetsmarknadsstatus_lan_df %>% filter(kön == "män",födelseregion == "inrikes född",region == "Sverige",variabel == "sysselsättningsgrad" ) %>%  .$varde)

syssgrad_inrikes_kvinna_Sverige <- gsub("\\.",",",arbetsmarknadsstatus_lan_df %>% filter(kön == "kvinnor",födelseregion == "inrikes född",region == "Sverige",variabel == "sysselsättningsgrad" ) %>%  .$varde)

# Sysselsättningsgrad utrikes Dalarna
syssgrad_utrikes_man_Dalarna <- gsub("\\.",",",arbetsmarknadsstatus_lan_df %>% filter(kön == "män",födelseregion == "utrikes född",region == "Dalarna",variabel == "sysselsättningsgrad" ) %>%  .$varde)

syssgrad_utrikes_kvinna_Dalarna <- gsub("\\.",",",arbetsmarknadsstatus_lan_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född",region == "Dalarna",variabel == "sysselsättningsgrad" ) %>%  .$varde)

syssgrad_utrikes_skillnad_Dalarna <- round(arbetsmarknadsstatus_lan_df %>% filter(kön == "män",födelseregion == "utrikes född",region == "Dalarna",variabel == "sysselsättningsgrad" ) %>%  .$varde-arbetsmarknadsstatus_lan_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född",region == "Dalarna",variabel == "sysselsättningsgrad" ) %>%  .$varde,0)

# Variabler arbetslöshet lan

# Arbetslöshet inrikes/utrikes och skillnad inrikes/utrikes Dalarna
arblosthet_inrikes_man_Dalarna <- gsub("\\.",",",arbetsmarknadsstatus_lan_df %>% filter(kön == "män",födelseregion == "inrikes född",region == "Dalarna",variabel == "arbetslöshet" ) %>%  .$varde)

arblosthet_inrikes_kvinna_Dalarna <- gsub("\\.",",",arbetsmarknadsstatus_lan_df %>% filter(kön == "kvinnor",födelseregion == "inrikes född",region == "Dalarna",variabel == "arbetslöshet" ) %>%  .$varde)

arblosthet_utrikes_man_Dalarna <- gsub("\\.",",",arbetsmarknadsstatus_lan_df %>% filter(kön == "män",födelseregion == "utrikes född",region == "Dalarna",variabel == "arbetslöshet" ) %>%  .$varde)

arblosthet_utrikes_kvinna_Dalarna <- gsub("\\.",",",arbetsmarknadsstatus_lan_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född",region == "Dalarna",variabel == "arbetslöshet" ) %>%  .$varde)

arblosthet_kvinnor_utrikes_inrikes_skillnad_Dalarna <- round(arbetsmarknadsstatus_lan_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född",region == "Dalarna",variabel == "arbetslöshet" ) %>%  .$varde-arbetsmarknadsstatus_lan_df %>% filter(kön == "kvinnor",födelseregion == "inrikes född",region == "Dalarna",variabel == "arbetslöshet") %>%  .$varde,0)

arblosthet_man_utrikes_inrikes_skillnad_Dalarna <- round(arbetsmarknadsstatus_lan_df %>% filter(kön == "män",födelseregion == "utrikes född",region == "Dalarna",variabel == "arbetslöshet" ) %>%  .$varde-arbetsmarknadsstatus_lan_df %>% filter(kön == "män",födelseregion == "inrikes född",region == "Dalarna",variabel == "arbetslöshet") %>%  .$varde,0)

# Högst arbetslöshet län utrikes
arblosthet_utrikes_man_max_lan <- arbetsmarknadsstatus_lan_df %>% filter(kön == "män",födelseregion == "utrikes född") %>%
  filter(variabel == "arbetslöshet" ) %>% filter(varde == max(varde)) %>%  .$region %>% glue_collapse(sep = ", ", last = " och ")

arblosthet_utrikes_man_max_varde <- gsub("\\.",",",arbetsmarknadsstatus_lan_df %>% filter(kön == "män",födelseregion == "utrikes född") %>%
                                           filter(variabel == "arbetslöshet" ) %>% filter(varde == max(varde)) %>%  .$varde) %>% first()

arblosthet_utrikes_kvinna_max_lan <- arbetsmarknadsstatus_lan_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>%
  filter(variabel == "arbetslöshet" ) %>% filter(varde == max(varde)) %>%  .$region %>% glue_collapse(sep = ", ", last = " och ")

arblosthet_utrikes_kvinna_max_varde <- gsub("\\.",",",arbetsmarknadsstatus_lan_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>%
                                              filter(variabel == "arbetslöshet" ) %>% filter(varde == max(varde)) %>%  .$varde) %>% first()

# Lägst arbetslöshet län utrikes

arblosthet_utrikes_man_min_lan <- arbetsmarknadsstatus_lan_df %>% filter(kön == "män",födelseregion == "utrikes född") %>%
  filter(variabel == "arbetslöshet" ) %>% filter(varde == min(varde)) %>%  .$region %>% glue_collapse(sep = ", ", last = " och ")

arblosthet_utrikes_man_min_varde <- gsub("\\.",",",arbetsmarknadsstatus_lan_df %>% filter(kön == "män",födelseregion == "utrikes född") %>%
                                           filter(variabel == "arbetslöshet" ) %>% filter(varde == min(varde)) %>%  .$varde) %>% first()

arblosthet_utrikes_kvinna_min_lan <- arbetsmarknadsstatus_lan_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>%
  filter(variabel == "arbetslöshet" ) %>% filter(varde == min(varde)) %>%  .$region %>% glue_collapse(sep = ", ", last = " och ")

arblosthet_utrikes_kvinna_min_varde <- gsub("\\.",",",arbetsmarknadsstatus_lan_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>%
                                              filter(variabel == "arbetslöshet" ) %>% filter(varde == min(varde)) %>%  .$varde) %>% first()


############
## Kommun ##
############
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diagram_arbetsmarknadsstatus_senastear.R")
gg_diagram_arbetsmarknadsstatus_kommun <- diagram_arbetsmarknadsstatus(region_vekt = hamtakommuner(),
                                                                       kon_klartext = c("kvinnor","män"),
                                                                       alder_klartext = "20-64 år",
                                                                       valda_farger = diagramfarger("kon"),
                                                                       fodelseregion_klartext_vekt = c("inrikes född", "utrikes född"),
                                                                       diag_arbetskraftsdeltagande = FALSE,
                                                                       returnera_data = TRUE,
                                                                       spara_figur = FALSE,
                                                                       data_namm = "arbetsmarknadsstatus_kommun_df")

# Variabler sysselsättningsgrad kommun

# Högst sysselsättningsgrad
syssgrad_utrikes_man_max_kommun <- arbetsmarknadsstatus_kommun_df %>% filter(kön == "män",födelseregion == "utrikes född") %>%
  filter(variabel == "sysselsättningsgrad" ) %>% filter(varde == max(varde)) %>%  .$region %>% glue_collapse(sep = ", ", last = " och ")

syssgrad_utrikes_man_max_varde <- gsub("\\.",",",arbetsmarknadsstatus_kommun_df %>% filter(kön == "män",födelseregion == "utrikes född") %>%
                                         filter(variabel == "sysselsättningsgrad" ) %>% filter(varde == max(varde)) %>%  .$varde) %>% first()

syssgrad_utrikes_kvinna_max_kommun <- arbetsmarknadsstatus_kommun_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>%
  filter(variabel == "sysselsättningsgrad" ) %>% filter(varde == max(varde)) %>%  .$region %>% glue_collapse(sep = ", ", last = " och ")

syssgrad_utrikes_kvinna_max_varde <- gsub("\\.",",",arbetsmarknadsstatus_kommun_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>%
                                            filter(variabel == "sysselsättningsgrad" ) %>% filter(varde == max(varde)) %>%  .$varde) %>% first()

# Lägst sysselsättningsgrad

syssgrad_utrikes_man_min_kommun <- arbetsmarknadsstatus_kommun_df %>% filter(kön == "män",födelseregion == "utrikes född") %>%
  filter(variabel == "sysselsättningsgrad" ) %>% filter(varde == min(varde)) %>%  .$region %>% glue_collapse(sep = ", ", last = " och ")

syssgrad_utrikes_man_min_varde <- gsub("\\.",",",arbetsmarknadsstatus_kommun_df %>% filter(kön == "män",födelseregion == "utrikes född") %>%
                                         filter(variabel == "sysselsättningsgrad" ) %>% filter(varde == min(varde)) %>%  .$varde) %>% first()

syssgrad_utrikes_kvinna_min_kommun <- arbetsmarknadsstatus_kommun_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>%
  filter(variabel == "sysselsättningsgrad" ) %>% filter(varde == min(varde)) %>%  .$region %>% glue_collapse(sep = ", ", last = " och ")

syssgrad_utrikes_kvinna_min_varde <- gsub("\\.",",",arbetsmarknadsstatus_kommun_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>%
                                            filter(variabel == "sysselsättningsgrad" ) %>% filter(varde == min(varde)) %>%  .$varde) %>% first()

# Variabler arbetslöshet kommun

# Utrikes

arblosthet_utrikes_man_max_kommun <- arbetsmarknadsstatus_kommun_df %>% filter(kön == "män",födelseregion == "utrikes född") %>%
  filter(variabel == "arbetslöshet" ) %>% filter(varde == max(varde)) %>%  .$region %>% glue_collapse(sep = ", ", last = " och ")

arblosthet_utrikes_man_kommun_max_varde <- gsub("\\.",",",arbetsmarknadsstatus_kommun_df %>% filter(kön == "män",födelseregion == "utrikes född") %>%
                                                  filter(variabel == "arbetslöshet" ) %>% filter(varde == max(varde)) %>%  .$varde) %>% first()

arblosthet_utrikes_kvinna_max_kommun <- arbetsmarknadsstatus_kommun_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>%
  filter(variabel == "arbetslöshet" ) %>% filter(varde == max(varde)) %>%  .$region %>% glue_collapse(sep = ", ", last = " och ")

arblosthet_utrikes_kvinna_kommun_max_varde <- gsub("\\.",",",arbetsmarknadsstatus_kommun_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>%
                                                     filter(variabel == "arbetslöshet" ) %>% filter(varde == max(varde)) %>%  .$varde) %>% first()

arblosthet_utrikes_man_min_kommun <- arbetsmarknadsstatus_kommun_df %>% filter(kön == "män",födelseregion == "utrikes född") %>%
  filter(variabel == "arbetslöshet" ) %>% filter(varde == min(varde)) %>%  .$region %>% glue_collapse(sep = ", ", last = " och ")

arblosthet_utrikes_man_kommun_min_varde <- gsub("\\.",",",arbetsmarknadsstatus_kommun_df %>% filter(kön == "män",födelseregion == "utrikes född") %>%
                                                  filter(variabel == "arbetslöshet" ) %>% filter(varde == min(varde)) %>%  .$varde) %>% first()

arblosthet_utrikes_kvinna_min_kommun <- arbetsmarknadsstatus_kommun_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>%
  filter(variabel == "arbetslöshet" ) %>% filter(varde == min(varde)) %>%  .$region %>% glue_collapse(sep = ", ", last = " och ")

arblosthet_utrikes_kvinna_kommun_min_varde <- gsub("\\.",",",arbetsmarknadsstatus_kommun_df %>% filter(kön == "kvinnor",födelseregion == "utrikes född") %>%
                                                     filter(variabel == "arbetslöshet" ) %>% filter(varde == min(varde)) %>%  .$varde) %>% first()

# Inrikes

arblosthet_inrikes_man_max_kommun <- arbetsmarknadsstatus_kommun_df %>% filter(kön == "män",födelseregion == "inrikes född") %>%
  filter(variabel == "arbetslöshet" ) %>% filter(varde == max(varde)) %>%  .$region %>% glue_collapse(sep = ", ", last = " och ")


arblosthet_inrikes_man_max_varde <- gsub("\\.",",",arbetsmarknadsstatus_kommun_df %>% filter(kön == "män",födelseregion == "inrikes född") %>%
                                           filter(variabel == "arbetslöshet" ) %>% filter(varde == max(varde)) %>%  .$varde) %>% first()

arblosthet_inrikes_kvinna_max_kommun <- arbetsmarknadsstatus_kommun_df %>% filter(kön == "kvinnor",födelseregion == "inrikes född") %>%
  filter(variabel == "arbetslöshet" ) %>% filter(varde == max(varde)) %>%  .$region %>% glue_collapse(sep = ", ", last = " och ")

arblosthet_inrikes_kvinna_max_varde <- gsub("\\.",",",arbetsmarknadsstatus_kommun_df %>% filter(kön == "kvinnor",födelseregion == "inrikes född") %>%
                                              filter(variabel == "arbetslöshet" ) %>% filter(varde == max(varde)) %>%  .$varde) %>% first()

arblosthet_inrikes_man_min_kommun <- arbetsmarknadsstatus_kommun_df %>% filter(kön == "män",födelseregion == "inrikes född") %>%
  filter(variabel == "arbetslöshet" ) %>% filter(varde == min(varde)) %>%  .$region %>% glue_collapse(sep = ", ", last = " och ")

arblosthet_inrikes_man_min_varde <- gsub("\\.",",",arbetsmarknadsstatus_kommun_df %>% filter(kön == "män",födelseregion == "inrikes född") %>%
                                           filter(variabel == "arbetslöshet" ) %>% filter(varde ==min(varde)) %>%  .$varde) %>% first()

arblosthet_inrikes_kvinna_min_kommun <- arbetsmarknadsstatus_kommun_df %>% filter(kön == "kvinnor",födelseregion == "inrikes född") %>%
  filter(variabel == "arbetslöshet" ) %>% filter(varde == min(varde)) %>%  .$region %>% glue_collapse(sep = ", ", last = " och ")

arblosthet_inrikes_kvinna_min_varde <- gsub("\\.",",",arbetsmarknadsstatus_kommun_df %>% filter(kön == "kvinnor",födelseregion == "inrikes född") %>%
                                           filter(variabel == "arbetslöshet" ) %>% filter(varde ==min(varde)) %>%  .$varde) %>% first()

# Arbetslöshet, tidsserie Arbetsförmedlingen
source(here("Skript","diagram_arbetsloshet_AF_bakgrund.R"))
gg_arbetsloshet_af <- diag_arbetsloshet_2008_senastear(output_mapp = output_mapp_figur,
                                                       spara_figur = TRUE,
                                                       returnera_data = FALSE)

# Långtidsarbetslöshet - Kolada
# långtidsarbetslöshet <- hamta_kolada_df(kpi_id = c("N03926"),
#                                         valda_kommuner = "20",
#                                         valda_ar = 2011:2100,
#                                         konsuppdelat = TRUE) %>% 
#   mutate(kon = tolower(kon))
source(here("Skript","diagram_langtidsarbetsloshet_tidsserie.R"))
gg_langtidsarbetsloshet <- diag_langtidsarbetsloshet(region_vekt = "20", 
                                                     output_mapp = output_mapp_figur,
                                                     returnera_data = TRUE,
                                                     startar = 2010, # Finns från 2010
                                                     spara_figur = TRUE)

langtidsarbetsloshet_ar_min = långtidsarbetslöshet$ar %>% min()
langtidsarbetsloshet_ar_max = långtidsarbetslöshet$ar %>% max()
langtidsarbetsloshet_kvinnor_min = gsub("\\.",",",round(långtidsarbetslöshet %>% filter(kon=="kvinnor",ar==min(ar)) %>%  .$varde,1))
langtidsarbetsloshet_kvinnor_max = gsub("\\.",",",round(långtidsarbetslöshet %>% filter(kon=="kvinnor",ar==max(ar)) %>%  .$varde,1))
langtidsarbetsloshet_man_min = gsub("\\.",",",round(långtidsarbetslöshet %>% filter(kon=="män",ar==min(ar)) %>%  .$varde,1))
langtidsarbetsloshet_man_max = gsub("\\.",",",round(långtidsarbetslöshet %>% filter(kon=="män",ar==max(ar)) %>%  .$varde,1))

# Ledamöter i olika politiska instanser
source(here("Skript","diagram_ledamoter_val_kon_kommun_kv_man.R"))
gg_ledamoter_val <- diag_ledamoter_val(outputmapp = output_mapp_figur,
                                       spara_dataframe_till_global_environment = TRUE,
                                       spara_figur = TRUE,
                                       typ_av_val = c("riksdag","region","kommun"))

# Riksdagsval
riksdagsval_ar_min <- min(riksdagsval_df$valår)
riksdagsval_ar_max <- max(riksdagsval_df$valår)
antal_ar_mellan_min_max <- as.numeric(max(riksdagsval_df$valår))- as.numeric(min(riksdagsval_df$valår))
riksdagsval_max_ar_skillnad <- riksdagsval_df %>% filter(valår == max(valår),kön =="män") %>% .$Antal - riksdagsval_df %>% filter(valår == max(valår),kön =="kvinnor") %>% .$Antal
mest_jamnstalld_ar <- riksdagsval_df %>% filter(kön == "kvinnor") %>% filter(Antal == max(Antal)) %>% .$valår
mest_jamnstalld_antal_kvinnor <- riksdagsval_df %>% filter(kön == "kvinnor") %>% filter(Antal == max(Antal)) %>% .$Antal
riksdagsval_min_ar_antal_kvinnor <- riksdagsval_df %>% filter(kön == "kvinnor", valår == min(valår)) %>% .$Antal

# Regionval
regionval_ar_min <- min(regionval_df$valår)
regionval_ar_max <- max(regionval_df$valår)
regionval_max_ar_skillnad <- regionval_df %>% filter(valår == max(valår),kön =="män") %>% .$Antal - regionval_df %>% filter(valår == max(valår),kön =="kvinnor") %>% .$Antal

# Kommunval
kommunval_ar_max <- max(kommunval_df$valår)
kommunval_kommuner_fler_kvinnor_man <- kommunval_df %>% pivot_wider(names_from = kön, values_from = Antal) %>%  filter(kvinnor > män) %>% .$region %>% glue_collapse(sep = ", ", last = " och ")
kommunval_kommuner_ojamnlik <- kommunval_df %>% pivot_wider(names_from = kön, values_from = Antal) %>%  filter((män/(kvinnor+män))>0.60) %>% .$region %>% glue_collapse(sep = ", ", last = " och ")
antal_ojamlik <- length(unlist(strsplit(gsub(" och ", ", ", kommunval_kommuner_ojamnlik), ",\\s*")))

# Andel chefer
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diagram_andel_chefer_utb_kv_man.R")
gg_andel_chefer <- diag_chefer(region_vekt = "20", # Enbart på län, max 1 åt gången
                               output_mapp = output_mapp_figur,
                               diag_senaste_ar = TRUE,
                               diag_linje = TRUE,
                               returnera_data = TRUE,
                               spara_figur = TRUE)

chefer_ar_max <- max(chefer_df$år)
andel_chefer_man <- round(chefer_df %>% filter(år == max(år),utbildningsnivå == "eftergymnasial utbildning", kön == "män") %>% .$Andel,0)
andel_chefer_kvinnor <- round(chefer_df %>% filter(år == max(år),utbildningsnivå == "eftergymnasial utbildning", kön == "kvinnor") %>% .$Andel,0)


# Överrepresentation av kvinnor
source("https://raw.githubusercontent.com/Region-Dalarna/socioekonomisk_analys_nms/refs/heads/main/skript/socioek_overrep.R")
gg_overrep = skapa_overrep_diagram(spara_diagrambildfil = TRUE,
                                   mapp = here("Diagram/") %>% paste0(., "/"),
                                   returnera_dataframe_global_environment = TRUE,
                                   diagramtitel = "Överrepresentation av manliga chefer i Dalarna",
                                   ta_bort_titel = FALSE,
                                   ta_bort_caption = FALSE)

overrep_max_ar = overrep_df$Ar %>% max()
overrep_min_ar = overrep_df$Ar %>% min()

save.image(file = "G:/skript/projekt/environments/kvinnor_man.RData")

end_time <- Sys.time()
elapsed_time <- as.numeric(difftime(end_time, start_time, units = "secs"))
cat(sprintf("Hämtning av data klar: Det tog %.2f sekunder.", elapsed_time))
cat("\n\n")


}else{
  load("G:/skript/projekt/environments/kvinnor_man.RData")
}

# Diverse tidigare kod som inte används

# test = skapa_overrep_diagram(spara_diagrambildfil = FALSE,
#                              diag_fargvekt = NA, # För diagrammet som inte är könsuppdelat
#                              returnera_dataframe_global_environment = TRUE,
#                              diagramtitel = "Överrepresentation av manliga chefer i Dalarna",
#                              ta_bort_titel = FALSE,
#                              ta_bort_caption = FALSE)

# # ============= Överrepresentation av chefer - motsvarar diagram 32 i den tidigare rapporten
# source(here("skript","socioek_overrep.R"), encoding="UTF-8")
# overrepresentation_bransch <- funktion_upprepa_forsok_om_fel( function() {
#   skapa_overrep_diagram()
# }, hoppa_over = hoppa_over_felhantering)
# 
# overrep_ar_min <- overrep_df$Ar %>% min()
# overrep_ar_max <- overrep_df$Ar %>% max()


# rmarkdown::render(
#   input = 'kvinnor_man_markdown_ny.Rmd',
#   output_file = 'kvinnor_man.html',
#   envir = parent.frame()
# )
