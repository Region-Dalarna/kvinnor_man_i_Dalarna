pacman::p_load(here)
# Läser in en funktion som används i skriptet
source("G:/skript/jon/Funktioner/func_bild_till_ggplot.R", encoding = "utf-8", echo = FALSE)
#testa=hamta_figurer(skapa_ppt=FALSE,skapa_fil=FALSE)

hamta_figurer <- function(skapa_ppt=FALSE,skapa_fil=TRUE,Output_mapp= here("Diagram","/")){
  
  master_lista=list()
  vald_region="20"
  
  # Skapar rubriker. Måste ges namnet rubrik för att skriptet som skriver till PPT skall förstå
  rubriker <-list()
  txt_storlek=NA
  rubriker[[1]] <- c("Ekonomisk jämställdhet","",txt_storlek)
  rubriker[[2]] <- c("Utbildning","",txt_storlek)
  rubriker[[3]] <- c("(O)hälsa","",txt_storlek)
  rubriker[[4]] <- c("Obetalt arbete","",txt_storlek)
  rubriker[[5]] <- c("Makt och politik","",txt_storlek)
  rubriker[[6]] <- c("Kroppslig integritet","",txt_storlek)
  names(rubriker) <- rep("Rubrik",length(rubriker))
  
  # Bilder som används i presentationen
  bilder <- list()
  bilder[[1]] <- c("G:/skript/jon/Presentationer/Byggforetagen juni/Indata/Konsfordelning_gymnasiet.PNG")
  
  # Räknare för rubriker
  j=1
  
  # Räknare för bilder
  i=1
  #======================================================================================================
  #=========================== Befolkning =========================================
  #======================================================================================================
  # Allmän info kopplad till Dalarnas befolkning
  source(here("pxweb_befpyramid_kvinnor_man.R"), encoding = "utf-8", echo = FALSE)
  # 1968, 2021 generellt utan jämförelse
  # master_lista <- c(master_lista,diag_befpyramid(skapa_fil = skapa_fil,
  #                                                utskriftmapp = Output_mapp,
  #                                                delaupp_aldergrp=TRUE,
  #                                                jmf_linje = "",
  #                                                ar=c("1968","2021"),
  #                                                region=c("20"))                                                 )
  
  # Tittar på andelen äldre i befolkningen (län) pxweb, en eller 2 figurer - visar andel över 65 på länsnivå
  # Den tredje figuren i dokumentet
  source(here("pxweb_befolkning_aldre.R"), encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_befolkning_alder(skapa_fil = skapa_fil,
                                                       output_mapp = Output_mapp,
                                                       region_vekt=vald_region,
                                                       diagram_senastear=TRUE,
                                                       diagram_prognos=FALSE))
  
  # Jämför hela befolkningen med utrikes födda 2021
  master_lista <- c(master_lista,diag_befpyramid(skapa_fil = skapa_fil,
                                                 utskriftmapp = Output_mapp,
                                                 delaupp_aldergrp=FALSE,
                                                 jmf_linje = "utr_inr",
                                                 ar=c("2021"),
                                                 region=c("20")))
  
  # Skapar ett linjediagram där befolkningen jämförs över tiden (Dalarna mot riket) - pxweb 1 figur
  source(here("pxweb_befolkning_forandring.R"), encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_befolkning(skapa_fil = skapa_fil,
                                                 output_mapp = Output_mapp,
                                                 region_vekt=vald_region,
                                                 diag_befolkning_linjediagram=TRUE))
  
  #======================================================================================================
  #=========================== Delmål 1, ekonomisk jämställdhet =========================================
  #======================================================================================================
  #Rubrik
  master_lista <- c(master_lista,rubriker[j])
  j=j+1
  
  # # Förvärvsintensitet - 4 figurer - pxweb
  # source("G:/skript/naringsliv/pxweb_API_Forvarvsintensitet_ny.R", encoding = "utf-8", echo = FALSE)
  # master_lista <- c(master_lista, diag_forvarvsintensitet(region_vekt = vald_region,skapa_fil = FALSE,
  #                                                         forvarvsintensitet_inrikes_stapel=TRUE,
  #                                                         forvarsintensitet_inrikes_linje=TRUE,
  #                                                         forvarsintensitet_utrikes_stapel=TRUE,
  #                                                         forvarsintensitet_utrikes_linje=TRUE))
  
  # Medianinkomst - 2 figurer - pxweb
  source(here("pxweb_medianinkomst.R"), encoding = "utf-8", echo = FALSE)
  # De två figurerna under ekonomisk jämställdhet/inkomst
  master_lista <- c(master_lista, diag_medianinkomst(region_vekt = vald_region,
                                                     skapa_fil = skapa_fil,
                                                     output_mapp = Output_mapp,
                                                     medianinkomst_stapel=FALSE,
                                                     medianinkomst_linje=FALSE,
                                                     medianinkomst_stapel_kon=TRUE,
                                                     medianinkomst_linje_kon=TRUE))
  
  # Disponibel inkomst - 1 figur - pxweb
  # Den tredje figuren under ekonomisk jämställdhet/inkomst
  source(here("pxweb_disponibelinkomst_kvinnor_man.R"), encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_disponibelinkomst(region_vekt = vald_region,
                                                        skapa_fil = skapa_fil,
                                                        output_mapp = Output_mapp))
  
  # Ekonomiskt bistånd - 1 figur - Socialstyrelsen (Excel) - UNDERLAG BEHÖVER UPPDATERS FÖR HAND!!!
  # Används för tillfället inte i kvinnor och män
  # source(here("ekonomisktbistand_kvinnor_man.R"), encoding = "utf-8", echo = FALSE)
  # master_lista <- c(master_lista,diag_ek_bistand(region_vekt = vald_region,
  #                                                skapa_fil = skapa_fil,
  #                                                output_mapp = Output_mapp))
  
  # Skuldsatta - 2 figurer - Kronofogden - Går via API- Enbart Dalarnas län
  # De två figurerna under antal skuldsatta
  source(here("kronofogden_skuldsatta.R"), encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_kronofogden(region_vekt = vald_region,
                                                  skapa_fil = skapa_fil,
                                                  output_mapp = Output_mapp,
                                                  Diagram_skuldsatta=TRUE,
                                                  Diagram_skuldsatta_lange=TRUE))
  
  # Andelen företagare - 1 figur - NMS (Excel) - Data till 2019 (20220504 - uppdateras via Mona)
  # Använd för tillfället inte i kvinnor och män
  # source("G:/skript/naringsliv/NMS_egenforetagare_ny.R", encoding = "utf-8", echo = FALSE)
  # master_lista <- c(master_lista, diag_egenforetagare(skapa_fil = FALSE))
  
  
  #======================================================================================================
  #======================== Delmål 2, utbildning + yrke + arbetslöshet  =================================
  #======================================================================================================
  #Rubrik
  master_lista <- c(master_lista,rubriker[j])
  j=j+1
  
  # Könsfördelning gymnsasiet - 1 figur. 
  source(here("Gymnasieantagningen.R"), encoding = "utf-8", echo = FALSE)
  master_lista <-c(master_lista,diag_gymnasieantagning(skapa_fil = skapa_fil,
                                                       output_mapp = Output_mapp,
                                                       region_vekt = vald_region))
  
  # Utbildningsnivå län - pxweb 1 figur. Visar utbildningsnivå 25-64 år uppdelat på utbildningsgrupper
  # Första diagrammet efter rubriken utbildning
  source(here("pxweb_utbildningsniva_region.R"), encoding = "utf-8", echo = FALSE)
  master_lista <-c(master_lista,diag_utbildningsniva(skapa_fil = skapa_fil,
                                                     output_mapp = Output_mapp,
                                                     region_vekt = vald_region))
  
  # # Högutbildade län
  # source("G:/skript/naringsliv/pxweb_API_Utbildningsniva_ny.R", encoding = "utf-8", echo = FALSE)
  # master_lista <-c(master_lista,diag_hogutbildade(region_vekt = vald_region,
  #                                                 skapa_fil = skapa_fil,
  #                                                 output_mapp = Output_mapp,
  #                                                 utbildning_stapel_sverige=FALSE,
  #                                                 utbildning_stapel_lan=FALSE,
  #                                                 utbildning_stapel_kon_sverige=TRUE,
  #                                                 utbildning_stapel_kon_lan=TRUE))
  
  # Utbildningsgrupper 1985-2021 (högutbildade) - pxweb, 2 figurer (1 visar andel på länsnivå senaste år och den andra utveckling 85-senaste år)
  # Figurer 3 och 4 under utbildning
  source("G:/skript/projekt/kompetens_genomgang/pxweb_utbniva_85_21.R", encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_utbniva_85(skapa_fil = skapa_fil,
                                                 output_mapp = Output_mapp,
                                                 region_vekt=vald_region,
                                                 diag_utb_85=FALSE,
                                                 diag_utb_85_kon=TRUE,
                                                 diag_utb_lan=FALSE,
                                                 diag_utb_lan_kon=TRUE))
  
  # Förvärvsarbetande, könsuppdaterad - vilka branscher jobbar man i. Notera att förändring fokuserar på byggverksamhet
  # Den första figuren under yrke.
  source("G:/skript/jon/Presentationer/Byggforetagen/NMS_utrikes_svensk.R", encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista, diag_utl_sv_bygg(region_vekt = vald_region,
                                                   skapa_fil = skapa_fil,
                                                   output_mapp = Output_mapp,
                                                   utrikes_bransch = FALSE,
                                                   utrikes_forandring = FALSE,
                                                   kvinnor_bransch = TRUE,
                                                   kvinnor_bransch_stapel=FALSE,
                                                   kvinnor_forandring = FALSE))
  
  # Vad jobbar man med? De tio vanligaste yrkena för kvinnor/män - 2 figurer - supercross - LÄN
  # Figurer 3 och 4 under yrken
  source(here("supercross_yrken_kvinnor_man.R"), encoding = "utf-8", echo = FALSE)
  master_lista <-c(master_lista,diag_syss_bygg_kon(skapa_fil = skapa_fil,
                                                   output_mapp = Output_mapp,
                                                   yrken_man = TRUE,
                                                   yrken_kvinna = TRUE))
  
  # Arbetsmarknadsstatus (arbetslöshet mm) uppdelat på län,kommun, kön och bakgrund - Pxweb, SCB 6 figurer (visar arbetslöshet mm uppdelat på kön och bakgrund (facet))
  # De fyra första figurerna under arbetsmarknadsstatus
  source(here("pxweb_arbetsmarknadsstatus.R"), encoding = "utf-8", echo = FALSE)
  master_lista <-c(master_lista,diag_arbetsloshet(region_vekt = "20",
                                                  skapa_fil = skapa_fil,
                                                  output_mapp = Output_mapp,
                                                  diag_lan = TRUE,
                                                  diag_kommun = TRUE,
                                                  diag_arbetslosthet = TRUE,
                                                  diag_arbetskraftsdeltagande = TRUE,
                                                  diag_sysselsattningsgrad = TRUE))
  
  # Arbetslöshet 2008 till senaste år - AF (Excel), 1 figur (visar arbetslöshet över tid uppdelat på kön och bakgrund (facet))
  # Från projektet kompetens_genomgang
  source("G:/skript/projekt/kompetens_genomgang/AF_arbetsloshet.R", encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_arbetsloshet_2008_senaste(skapa_fil = skapa_fil,
                                                                output_mapp = Output_mapp,
                                                                region_vekt=vald_region,
                                                                diag_arbetsloshet_totalt=FALSE,
                                                                diag_arbetsloshet_kon=TRUE))
  
  # Långtidsarbetslöshet uppdelat på på län,kommun, kön och bakgrund - Supercross (SCB), 4 figurer
  # 3 figurer under arbetsmarknadsstatus
  source(here("supercross_langtidsarbetsloshet.R"), encoding = "utf-8", echo = FALSE)
  master_lista <-c(master_lista,diag_langtidsarbetsloshet(skapa_fil = skapa_fil,
                                                          output_mapp = Output_mapp,
                                                          diag_lan_kon=TRUE,
                                                          diag_lan_bakgrund=TRUE,
                                                          diag_lan_utbildningsniva=TRUE,
                                                          diag_kommun=TRUE))
  
  # Hur lång tid tar det att etablera sig på arbetsmarknaden (senaste år) - pxweb 2 figurer
  # De två första figurerna under rubriken etablering
  source(here("pxweb_etablering_nyanlanda.R"), encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_etablering(region_vekt = "20", 
                                                skapa_fil = skapa_fil,
                                                output_mapp = Output_mapp,
                                                diag_etablering_gen=TRUE,
                                                diag_etablering_utb=TRUE))
  
 
  # Hur lång tid tar det att etablera sig på arbetsmarknaden (förändring över tid) - API 1 figur
  # Den tredje figuren under etablering. Ej uppdelat på kön
  source(here("Etablering.R"), encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_etablering_tid(region_vekt = "20",
                                                     Output_mapp = Output_mapp))
  
  # Allmän matchning på arbetsmarknaden - pxweb, 2 diagram - matchning per län och bakgrund (i Dalarna)
  # De två figurerna under rubriken matchning på arbetsmarknaden
  source("G:/skript/projekt/kompetens_genomgang/pxweb_matchning.R", encoding = "utf-8", echo = FALSE)
  master_lista <- c(master_lista,diag_matchning(region_vekt = "20", 
                                                skapa_fil = skapa_fil,
                                                output_mapp = Output_mapp,
                                                matchning_lan=TRUE,
                                                matchning_bakgrund=TRUE))
  
  #======================================================================================================
  #=======================================  Delmål 3, ohälsa  ===========================================
  #======================================================================================================
  #Rubrik
  master_lista <- c(master_lista,rubriker[j])
  j=j+1
  
  # Diverse mått på ohälsa - 4 figurer - Försäkringskassan (Excel) - LÄN
  # 4 figurer kopplat till Försäkringskassan på länsnivå
  source(here("forsakringskassan_kvinnor_man.R"), encoding = "utf-8", echo = FALSE)
  master_lista <-c(master_lista,diag_forsakringskassan(region_vekt = vald_region,
                                                       skapa_fil = skapa_fil,
                                                       output_mapp = Output_mapp,
                                                       ohalsotal_stapel=TRUE,
                                                       sjukpenningtal_stapel=TRUE,
                                                       antal_per_forsakrad_stapel=TRUE,
                                                       stress_stapel=TRUE))
  
  # Diverse mått på ohälsa - 4 figurer - Försäkringskassan (Excel) - KOMMUN
  # Som ovan på kommunnivå
  source(here("forsakringskassan_kommun_kvinnor_man.R"), encoding = "utf-8", echo = FALSE)
  master_lista <-c(master_lista,diag_forsakringskassan_kommun(region_vekt = vald_region,
                                                              skapa_fil = skapa_fil,
                                                              output_mapp = Output_mapp,
                                                              ohalsotal_stapel=TRUE,
                                                              sjukpenningtal_stapel=TRUE,
                                                              antal_per_forsakrad_stapel=TRUE,
                                                              stress_stapel=TRUE))
  
  # Startade sjukfall och längd per bransch - Sverige - Försäkringskassan
  # Den sista figuren under rubriken ohälsa. Finns för både startade sjukfall och sjukskrivningsdagar
  source(here("forsakringskassan_sjp_bransch.R"), encoding = "utf-8", echo = FALSE)
  master_lista <-c(master_lista,diag_sjukpenning_bransch(region_vekt = vald_region,
                                                         skapa_fil = skapa_fil,
                                                         output_mapp = Output_mapp,
                                                         startade_sjukfall=TRUE,
                                                         sjukskrivningsdagar=FALSE))
  
  #======================================================================================================
  #=======================================  Delmål 4, obetalt arbete  ===================================
  #======================================================================================================
  #Rubrik
  master_lista <- c(master_lista,rubriker[j])
  j=j+1
  
  # Föräldrapenning - 4 figurer - Försäkringskassan (Excel) - LÄN
  source(here("fk_foraldrapenning_kvinnor_man.R"), encoding = "utf-8", echo = FALSE)
  master_lista <-c(master_lista,diag_foraldrapenning(region_vekt = vald_region,
                                                     skapa_fil = skapa_fil,
                                                     output_mapp = Output_mapp,
                                                     antal_mottagare_stapel=TRUE,
                                                     antal_mottagare_linje=TRUE,
                                                     andel_mottagare_stapel=TRUE,
                                                     andel_mottagare_linje=TRUE))
  
  # Föräldrapenning - 2 figurer - Försäkringskassan (Excel) - Kommun
  source(here("fk_foraldrapenning_kommun_kvinnor_man.R"), encoding = "utf-8", echo = FALSE)
  master_lista <-c(master_lista,diag_foraldrapenning_kommun(region_vekt = vald_region,
                                                            skapa_fil = skapa_fil,
                                                            output_mapp = Output_mapp,
                                                            antal_mottagare_stapel=TRUE,
                                                            andel_mottagare_stapel=TRUE))
  
  
  # Tillfällig föräldrapenning - 3 figurer - Försäkringskassan (Excel) - Kommun och län
  # VAB tre figurer
  source(here("fk_tf_foraldrapenning_kvinnor_man.R"), encoding = "utf-8", echo = FALSE)
  master_lista <-c(master_lista,diag_tf_foraldrapenning(region_vekt = vald_region,
                                                        skapa_fil = skapa_fil,
                                                        output_mapp = Output_mapp,
                                                        antal_nettodagar_stapel=TRUE,
                                                        antal_nettodagar_linje=TRUE))
  
  #======================================================================================================
  #===========================      Delmål 5, makt och politik  =========================================
  #======================================================================================================
  #Rubrik
  master_lista <- c(master_lista,rubriker[j])
  j=j+1
  
  # Valda ledamöter till olika nivåer av det politiska styret -pxweb - funkar för tillfället enbart för Dalarna
  source(here("pxweb_val_kvinnor_man.R"), encoding = "utf-8", echo = FALSE)
  master_lista <-c(master_lista,diag_val(region_vekt = vald_region,
                                         skapa_fil = skapa_fil,
                                         output_mapp = Output_mapp,
                                         riksdagsval_stapel=TRUE,
                                         regionval_stapel=TRUE,
                                         kommunval_stapel=TRUE))
  
  # Andel chefer -pxweb - 2 igurer
  source(here("pxweb_andel_chefer.R"), encoding = "utf-8", echo = FALSE)
  master_lista <-c(master_lista,diag_chefer(region_vekt = vald_region,
                                            skapa_fil = skapa_fil,
                                            output_mapp = Output_mapp,
                                            andel_chefer=TRUE,
                                            andel_chefer_linje=TRUE)) 
  
  
  ##########################################################################################
  #######################           Export till Powerpoint           ####################### 
  ##########################################################################################
  if (skapa_ppt==TRUE){

    dag <- paste0(gsub("^0", "", format(Sys.Date(), "%d")), "_")
    man_ar <- format(Sys.Date(), "%b_%Y")
    datum <- paste0(dag, man_ar)
    filnamn_datum=paste0("Kvinnor_man_",datum,".pptx")
    output <- here("output","/")

    source("G:/skript/jon/PPT/PPT_jon_slutgiltig.R", encoding = "utf-8", echo = FALSE)
    skapa_ppt_fran_lista(pptlista=master_lista,"Kvinnor och män i Dalarna",filnamn=filnamn_datum,output_mapp=output)
  }
  
  return(master_lista)

}
                                                   
