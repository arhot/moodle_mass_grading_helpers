

# clean_up_moodle_gift_for_human_readers.R
#
# Muuntaa Moodlen kysymyspankin (XML → Excel) ihmisluettavaksi taulukoksi.
#
# Työnkulku:
#   1. Vie kysymyspankki Moodlesta XML-muodossa.
#   2. Avaa XML Excel:ssä ja tallenna .xlsx-muodossa (kysym.xlsx).
#   3. Muokkaa alla KONFIGURAATIO-osion polut ja aja skripti.

library(tidyverse)
library(readxl)
library(rvest)

# =============================================================================
# KONFIGURAATIO – muokkaa täältä
# =============================================================================

# Polku Moodlesta vietyyn kysymyspankki-Excel:iin
kysymykset_tiedosto <- "matskut2023/kysym.xlsx"

# Polku tulostiedostolle
tuloste_tiedosto <- "matskut2023/kysymykset_kaannokseen.txt"

# =============================================================================
# AJO
# =============================================================================

kysym <- read_excel(kysymykset_tiedosto)

kysym <- kysym %>% fill(category.text)

kysym <- kysym %>% filter(!is.na(questiontext.text))

kysym <- kysym %>%
  mutate(kysymys = gsub("<.*?>", "", questiontext.text),
         vastaus = gsub("<.*?>", "", answer.text))

kysym <- kysym %>% select(category.text, kysymys, vastaus) 

kysym_wide <- kysym %>% 
  group_by(category.text, kysymys) %>% 
  mutate(vastaus_n = row_number()) %>% 
  pivot_wider(c(category.text, kysymys), names_from = vastaus_n, values_from = vastaus, names_glue = "vastaus.{vastaus_n}")

write.table(kysym_wide, tuloste_tiedosto, sep = "\n", row.names = FALSE, quote = FALSE)
