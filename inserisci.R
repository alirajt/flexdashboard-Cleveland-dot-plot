library(tidyverse)
library(readxl)
library(writexl)
library(lubridate)
library(stringr)

copyBarcode <- function(df) {
  df %>%
    select(Barcode) %>%
    pull %>%
    writeClipboard()
}

Sequenza_giri <- read_excel("Sequenza_giri.xlsx")

#---------------------------------------------------------------------------
# leggi e prepara i barcode arrivati

#fileToLoad1 <- file.choose(new = TRUE)
Arrivi <- read_excel(file.choose(new = TRUE))

Arrivi <- Arrivi %>%
  filter(!is.na(BARCODE)) %>%
  filter(nchar(BARCODE) > 1) %>%
  mutate(Barcode = ifelse(startsWith(BARCODE, "T"), 
                         substr(BARCODE, 1, 16), 
                         BARCODE),
         Giro = GIRI) %>%
  select(Barcode, Giro)

Arrivi %>% count()

#-----------------------------------------------------------------------------
# leggi e prepara il file scaricato da buste buste

# copia barcode
copyBarcode(Arrivi)

# buste buste
fileToLoad2 <- file.choose(new = TRUE)
Buste <- read_excel(fileToLoad2)

Buste <- Buste %>%
  mutate(Arrivo = today()) %>% # colona data arrivo
  select("Barcode", "RS.Cliente", "Nome Dest.", "Via", "N°", "CAP", "Arrivo") %>%
  left_join(Arrivi) %>%
  left_join(Sequenza_giri) # aggiungi colona sequenza e postino

#--------------------------------------------------------------------
# aggiungi colona scadenza

# copia barcode
copyBarcode(Buste)

fileToLoad3 <- file.choose(new = TRUE)
Sla <- read_excel(fileToLoad3)
Sla <- Sla %>%
  mutate(Scadenza = dmy(`Data Scadenza Partner`)) %>%
  select(Barcode, Scadenza)

Buste <- Buste %>%
  left_join(Sla)

#-------------------------------------------------------------------
# aggiungi nuovi e salva

Raccomandate <- read_excel("Raccomandate.xlsx")
Raccomandate$Arrivo <- ymd(Raccomandate$Arrivo)
Raccomandate$Scadenza <- ymd(Raccomandate$Scadenza)

Raccomandate_new <-  Raccomandate %>% 
  bind_rows(Buste)

Raccomandate_new %>% count()
Raccomandate_new <- Raccomandate_new %>% distinct(Barcode, .keep_all = TRUE)
Raccomandate_new %>% count()

Raccomandate_new %>%
  write_xlsx("Raccomandate.xlsx")

#----------------------------------------------------------------------
#eventuali barcode non al sistema

barcodeSbagliati <-  Arrivi %>%
  anti_join(Buste)
barcodeSbagliati %>% count()

barcodeSbagliati

copyBarcode(barcodeSbagliati)

barcodeSbagliati %>%
  write_xlsx(str_c("barcodeSbagliati", today()))
#----------------------------------------------------------------------