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
