
# Skall data uppdateras? Annars läses data in från en sparad global environment-fil.
# Notera dock att ett flertal diagram skapas via Excelfiler (se skriptet hamta_data). Uppdatera data i detta fall åsyftar enbart de diagram-funktioner som körs nedan
uppdatera_data = FALSE

if(uppdatera_data == TRUE){
  
  cat("Hämtning av data påbörjad\n\n")
  start_time <- Sys.time()


if (!require("pacman")) install.packages("pacman")
p_load(here)

output_mapp_figur = here("Diagram","/")

source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")


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

# Etablering på arbetsmarknaden
source("https://raw.githubusercontent.com/Region-Dalarna/diagram/refs/heads/main/diag_etableringstid_kon_lan_tidsserie_KvMa_IntRap.R")
#source(here("skript/","etablering_kon_utbildningsniva.R"))
gg_etablering <- diag_etablering_utb_kon_scb(output_mapp = output_mapp_figur,
                                             utbildningsniva_jmf = c("utbildningsnivå: förgymnasial utbildning","utbildningsnivå: gymnasial utbildning" ,"utbildningsnivå: eftergymnasial utbildning"), # Finns även "samtliga utbildningsnivåer", "utbildningsnivå: förgymnasial utbildning", Skriv i den ordning de skall visas i diagram
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


# Hämtar data för långtidsarbetslöshet
långtidsarbetslöshet <- hamta_kolada_df(kpi_id = c("N03926"),
                                        valda_kommuner = "20",
                                        valda_ar = 2011:2100,
                                        konsuppdelat = TRUE) %>% 
  mutate(kon = tolower(kon))

langtidsarbetsloshet_ar_min = långtidsarbetslöshet$ar %>% min()
langtidsarbetsloshet_ar_max = långtidsarbetslöshet$ar %>% max()
langtidsarbetsloshet_kvinnor_min = gsub("\\.",",",round(långtidsarbetslöshet %>% filter(kon=="kvinnor",ar==min(ar)) %>%  .$varde,1))
langtidsarbetsloshet_kvinnor_max = gsub("\\.",",",round(långtidsarbetslöshet %>% filter(kon=="kvinnor",ar==max(ar)) %>%  .$varde,1))
langtidsarbetsloshet_man_min = gsub("\\.",",",round(långtidsarbetslöshet %>% filter(kon=="män",ar==min(ar)) %>%  .$varde,1))
langtidsarbetsloshet_man_max = gsub("\\.",",",round(långtidsarbetslöshet %>% filter(kon=="män",ar==max(ar)) %>%  .$varde,1))

# # Överrepresentation av kvinnor
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
