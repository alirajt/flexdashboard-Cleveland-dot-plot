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

Raccomandate <- read_excel("Raccomandate.xlsx")
Raccomandate$Arrivo <- ymd(Raccomandate$Arrivo)
Raccomandate$Scadenza <- ymd(Raccomandate$Scadenza)

copyBarcode(Raccomandate)

fileToLoad <- file.choose(new = TRUE)
RaccBuste <- read_excel(fileToLoad)

RaccBuste$Esito <- ifelse(!is.na(RaccBuste$`Data Altre Azioni`),
                          RaccBuste$`Data Altre Azioni`,
                          RaccBuste$`Data Rec.`)

inFiliale <-  RaccBuste %>%
  filter(is.na(Esito))
esitate <- RaccBuste %>%
  filter(!is.na(Esito))

esitate %>% count()
inFiliale %>% count()

Rimangono <- Raccomandate %>%
  anti_join(esitate, by="Barcode")
Rimangono %>% count()

Rimangono %>%
  write_xlsx("Raccomandate.xlsx")

