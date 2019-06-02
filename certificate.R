library(tidyverse)
library(readxl)
library(writexl)
library(ggpubr)

copyBarcode <- function(df) {
  df %>%
    select(Barcode) %>%
    pull %>%
    writeClipboard()
}


#---------------------------------------------------------------
#inserisci

Sequenza_giri <- read_excel("Sequenza_giri.xlsx")

Certificate <- read_excel(file.choose(new = TRUE))

Certificate %>% count()
Certificate <- Certificate %>% distinct(Barcode, .keep_all = TRUE) # togli duplicati
Certificate %>% count()

copyBarcode(Certificate)

BusteBuste <- read_excel(file.choose(new = TRUE))
BusteBuste %>% count()

BusteBuste <- BusteBuste %>%
  mutate(E = ifelse(!is.na(`Data Altre Azioni`),
                        `Data Altre Azioni`,
                        `Data Rec.`)) %>%
  mutate(Esito = ifelse(is.na(E), "NO", "SI")) %>%
  select(Barcode, Esito)

Certificate <- Certificate %>%
  semi_join(BusteBuste, by="Barcode") %>% # elimina barcode non al sistema
  left_join(BusteBuste, by = "Barcode") %>% # esito
  mutate(Giro = giro) %>%
  select(Barcode, Giro, Esito) %>%
  left_join(Sequenza_giri) # aggiungi colona sequenza e postino

head(Certificate)
Certificate %>%
  write_xlsx(str_c("Certificate.xlsx"))

#----------------------------------------------------------------------------
#aggiorna

Certificate <- read_excel("Certificate.xlsx")
copyBarcode(Certificate)

BusteBuste <- read_excel(file.choose(new = TRUE))

Certificate <- Certificate %>%
  select(Barcode, Giro, Seq, Postino)
BusteBuste$Esito <- ifelse(!is.na(BusteBuste$`Data Altre Azioni`),
                           BusteBuste$`Data Altre Azioni`,
                           BusteBuste$`Data Rec.`)
BusteBuste <- BusteBuste %>%
  select(Barcode, Esito)
Certificate <- Certificate %>%
  left_join(BusteBuste, by = "Barcode")
Certificate$Esito <- ifelse(is.na(Certificate$Esito),
                            "NO",
                            "SI")
Certificate <- Certificate %>% select(Barcode, Giro, Esito, Seq, Postino)
head(Certificate)
Certificate %>%
  write_xlsx(str_c("Certificate.xlsx"))
#-----------------------------------------------------------------------------
#grafico

grCert <- Certificate %>%
  group_by(Postino, Esito) %>%
  count()
grCert


ggdotchart(grCert, x = "Postino", y = "n",
           color = "Esito",
           #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
           #sorting = "descending",
           add = "segments",
           rotate = TRUE,
           group = "Postino",
           #facet.by = "Seq",
           dot.size = 10,
           label = round(grCert$n),
           font.label = list(color = "black", size = 8, 
                             vjust = 0.5),
           ggtheme = theme_pubr())

Certificate %>%
  write_xlsx("Certificate.xlsx")
#----------------------------------------------------------------


daFinire <- Certificate %>%
  filter(Esito == "NO")
daFinire %>%
  write_xlsx("daFinire.xlsx")
daFinire <- read_xlsx("daFinire.xlsx")
count(daFinire)

daFinire <- Certificate
grDaFinire <- daFinire %>%
  group_by(Postino, Esito) %>%
  count()

ggdotchart(grDaFinire, x = "Postino", y = "n",
           color = "Esito",
           #palette = c("#00AFBB", "#E7B800", "#FC4E07"),
           #sorting = "descending",
           add = "segments",
           rotate = TRUE,
           group = "Postino",
           #facet.by = "Seq",
           dot.size = 10,
           label = round(grDaFinire$n),
           font.label = list(color = "black", size = 8, 
                             vjust = 0.5),
           ggtheme = theme_pubr())

#aggiorna
#------------------------------------------------------
Certificate <- read_excel("daFinire.xlsx")
copyBarcode(Certificate)
#-----------------------------------------------------
aggiorna <- function() {
  fileToLoadCertBuste <- file.choose(new = TRUE)
  BusteBuste <- read_excel(fileToLoadCertBuste)
  Certificate <- Certificate %>%
    select(Barcode, Giro, Seq, Postino)
  BusteBuste$Esito <- ifelse(!is.na(BusteBuste$`Data Altre Azioni`),
                             BusteBuste$`Data Altre Azioni`,
                             BusteBuste$`Data Rec.`)
  
}
#---------------------------------------------------
fileToLoadCertBuste <- file.choose(new = TRUE)
BusteBuste <- read_excel(fileToLoadCertBuste)
BusteBuste %>% count()
head(Certificate)
Certificate <- Certificate %>%
  select(Barcode, Giro, Seq, Postino)
BusteBuste$Esito <- ifelse(!is.na(BusteBuste$`Data Altre Azioni`),
                           BusteBuste$`Data Altre Azioni`,
                           BusteBuste$`Data Rec.`)
BusteBuste <- BusteBuste %>%
  select(Barcode, Esito)
Certificate <- Certificate %>%
  left_join(BusteBuste, by = "Barcode")
Certificate$Esito <- ifelse(is.na(Certificate$Esito),
                            "NO",
                            "SI")
Certificate <- Certificate %>% select(Barcode, Giro, Esito, Seq, Postino)
head(Certificate)
#Certificate %>%
#  write_xlsx(str_c("Certificate.xlsx"))