# Uppdaterar data som används i rapporten "Läget i Dalarna"
if (!require("pacman")) install.packages("pacman")
p_load(here)

Output_mapp="G:/skript/projekt/data/kvinnor_man/"

# Befolkning:
# Alla län för senaste år (används för att beräkna andelar över 65)
# Dalarna och riket för alla år (används för att skapa figur med förändring i befolkning över tid)
source(here("Skript","befolkning.R"), encoding="UTF-8")
data_befolkning_alder(spara_data = TRUE,
                      output_mapp = Output_mapp)

# Utbildningsnivå för senaste år (används för att skapa två figurer: utbildningsnivåer i Dalarna och länsjämförelse)
source(here("Skript","utbildningsniva_senastear.R"), encoding="UTF-8")
data_utbildningsniva(spara_data = TRUE,
                     output_mapp = Output_mapp)

# Utbildningsnivå från 85 och framåt uppdelat på kön
source(here("Skript","utbildningsniva_85.R"), encoding="UTF-8")
data_utbniva_85(spara_data = TRUE,
                output_mapp = Output_mapp)

# Förvärvsarbetande, uppdelat på kön
source(here("Skript","forvarvsarbetande_bransch.R"), encoding="UTF-8")
data_forvarvsarbetande_bransch(spara_data = TRUE,
                               output_mapp = Output_mapp)

# Yrke, uppdelat på kön
source(here("Skript","yrke.R"), encoding="UTF-8")
data_yrke(spara_data = TRUE,
          output_mapp = Output_mapp)

# Arbetsmarknadsstatus (senaste år)
source(here("Skript","arbetsmarknadsstatus_senastear.R"), encoding="UTF-8")
diag_arbetsmarknadsstatus(skapa_fil = TRUE,
                          output_mapp = Output_mapp)

# Etableringstid på arbetsmarknaden
source(here("Skript","etablering.R"), encoding="UTF-8")
diag_etablering(spara_data = TRUE,
                output_mapp = Output_mapp)

# Medianinkomst
source(here("Skript","medianinkomst.R"), encoding="UTF-8")
diag_medianinkomst(spara_data = TRUE,
                   output_mapp = Output_mapp)

# Disponibel inkomst
source(here("Skript","disponibel_inkomst.R"), encoding="UTF-8")
diag_disponibelinkomst(spara_data = TRUE,
                       output_mapp = Output_mapp)

# Matchning
source(here("Skript","matchning.R"), encoding="UTF-8")
diag_matchning(spara_data = TRUE,
               output_mapp = Output_mapp)

# Skuldsatta
source(here("Skript","skuldsatta.R"), encoding="UTF-8")
diag_kronofogden(spara_data = TRUE,
                 output_mapp = Output_mapp)

# Val (riksdag, region och kommun)
source(here("Skript","val.R"), encoding="UTF-8")
diag_val(spara_data = TRUE,
         output_mapp = Output_mapp)

# Chefer 
source(here("Skript","Chefer.R"), encoding="UTF-8")
diag_chefer(spara_data = TRUE,
            output_mapp = Output_mapp)

# Föräldrapenning och VAB
source(here("Skript","foraldrapenning.R"), encoding="UTF-8")
diag_foraldraforsakring(spara_data = TRUE,
                        output_mapp = Output_mapp)

# Ohälsotal och sjukpenningtal
source(here("Skript","ohalsotal_sjukpenningtal.R"), encoding="UTF-8")
diag_ohalsa(spara_data = TRUE,
            output_mapp = Output_mapp)

# Startade sjukfall stress
source(here("Skript","sjukfall_stress.R"), encoding="UTF-8")
diag_stress(spara_data = TRUE,
            output_mapp = Output_mapp)

# Startade sjukfall bransch
source(here("Skript","startade_sjukfall_bransch.R"), encoding="UTF-8")
diag_sjukfall_bransch(spara_data = TRUE,
                      output_mapp = Output_mapp)

