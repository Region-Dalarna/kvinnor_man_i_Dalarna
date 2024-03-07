# Hämtar data för skuldsatta i olika åldersgrupper från Kronofogden och sätter dessa i relation till befolkningen i samma åldersgrupp

# Läser in nödvändiga bibliotek med pacman
if (!require("pacman")) install.packages("pacman")
p_load(pxweb,
       openxlsx)

# Funktioner som behövs
source("https://raw.githubusercontent.com/Region-Dalarna/funktioner/main/func_API.R")

diag_kronofogden_andel <- function(output_mapp = "G:/Samhällsanalys/Statistik/Näringsliv/basfakta/",
                                   spara_data = TRUE,
                                   filnamn = "skuldsatta_andel.xlsx"){

  skuldsatta_df <- read.csv2("https://kronofogden.entryscape.net/store/2/resource/27",encoding="latin1" ) %>% 
    filter(Län == "DALARNA") %>% 
      group_by(År,Ålder,Kön) %>% 
        summarize(Antal_skuldsatta = sum(Antal.skuldsatta),
                  Skuldbelopp = sum(Skuldbelopp),
                  Skuldbelopp_person = Skuldbelopp/Antal_skuldsatta,
                  Antal_utmätning = sum(Antal.med.löneutmätning)) %>% 
          rename("kön" = Kön) %>% 
            mutate(År=as.character(År),
                   kön = ifelse(kön == "Kvinna","kvinnor","män")) %>%
              filter(År == max(.$År))
  
  url <- "https://api.scb.se/OV0104/v1/doris/sv/ssd/START/BE/BE0101/BE0101A/BefolkningNy"
  
  # Variabler som skall tas ut. Tar bara ut för det senast år för vilket det finns data för skuldsatta.
  varlista <-  list("Region" = c("20"),
                    "Civilstand" = c("*"),
                    "Alder" = c("*"),
                    "Kon"=c("1","2"),
                    "ContentsCode"="BE0101N1",
                    "Tid"=as.character(max(skuldsatta_df$År)))
  
  px_uttag <- pxweb_get(url = url,query = varlista)
  
  # Tar ut data,gör vissa justeringar och delar upp ålder i samma grupper som skuldsatta.
  befolkning_df <- as.data.frame(px_uttag, column.name.type = "text", variable.value.type = "text") %>% 
    filter(ålder!="totalt ålder") %>% 
      mutate(ålder = ifelse(ålder== "100+ år","100",ålder)) %>% 
        mutate(ålder = as.integer(word(ålder))) %>% 
          mutate(Ålder = case_when(
            between(ålder,0,17) ~ "0-17 år",
            between(ålder,18,25) ~ "18-25 år",
            between(ålder,26,34) ~ "26-34 år",
            between(ålder,35,44) ~ "35-44 år",
            between(ålder,45,54) ~ "45-54 år",
            between(ålder,55,64) ~ "55-64 år",
            ålder>64 ~ "65+ år" )) %>% 
              group_by(kön,år,Ålder) %>% 
                summarize(antal_personer=sum(Folkmängd)) %>% 
                  rename("År" = år)
  
  skulder_befolkning_df <- skuldsatta_df %>%
      left_join(befolkning_df,by = c("År","Ålder","kön")) %>% 
        mutate(skulder_andel_proc = (Antal_skuldsatta/antal_personer)*100)
  
  # Sparar data till Excel
  if (spara_data==TRUE){
    flik_lista=lst("Skulder" = skulder_befolkning_df)
    write.xlsx(flik_lista,paste0(output_mapp,filnamn))
  }

}


