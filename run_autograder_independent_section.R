# run_autograder_independent_section.R
#
# Automaattinen arviointi itsenäisen osion tehtäville:
# kielitunnistuksen ja sanamäärän perusteella.
# Muokkaa alla olevaa KONFIGURAATIO-osiota kurssikohtaisesti.
#
# Riippuvuudet:
source("helpers_autograder.R")
source("helpers_read_files.R")

library(tidyverse)
library(readr)
library(writexl)
library(cld2)


# =============================================================================
# KONFIGURAATIO – muokkaa täältä
# =============================================================================

# --- Tiedostopolut ---

# Progress-raportti (Moodlesta: Aktiviteetin suoritus → Vie CSV:nä)
progress_file <- "teemaviikkojen_arviot/progress.csv"

# Arvioinnit-Excel Moodlesta (sisältää Tunnistenumero, Sähköpostiosoite, Etunimi, Sukunimi)
arvioinnit_file <- "teemaviikkojen_arviot/arvioinnit.xlsx"

# Esseepalautusten hakemistot (yksi hakemisto per tehtävä)
essay_dir_J2 <- "teemaviikkojen_arviot/tehtava_j2/"
essay_dir_1c <- "teemaviikkojen_arviot/tehtava_1c/"

# Avotentti-CSV (kysymystentti, jossa "Vastaus 1", "Vastaus 2" jne.)
open_exam_file <- "teemaviikkojen_arviot/1B_avotentti-vastaukset.csv"

# --- Tulostiedostot ---
# (tallennetaan samaan kansioon kuin syötetiedostot)

output_dir <- "teemaviikkojen_arviot/"  # muokkaa tähän


# =============================================================================
# AJO
# =============================================================================

# 1) Lue ja suodata progress-raportti
progress <- read_moodle_progress(progress_file)

alkutentti_sarake <- "J1) Tentti kurssin suoritustavoista"  # muokkaa tähän jos tarvitsee

ei_aloittanut <- progress %>%
  filter(.data[[alkutentti_sarake]] != "Suoritettu (hyväksytty arvosana)") %>%
  select(Nimi, Sähköpostiosoite, all_of(alkutentti_sarake))

write.csv2(ei_aloittanut, file.path(output_dir, "alkutentti_tekematta.csv"), row.names = FALSE, na = "")
message("Alkutentti tekemättä: ", nrow(ei_aloittanut), " opiskelijaa")

progress <- progress %>%
  filter(.data[[alkutentti_sarake]] == "Suoritettu (hyväksytty arvosana)")

# 2) Liitä käyttäjätunnukset
kayttaja_idt <- read_moodle_user_ids(arvioinnit_file) %>%
  left_join(progress %>% select(-Nimi), by = c("email" = "Sähköpostiosoite"))

# 3) J2-tehtävä (essee)
tehtava_J2 <- read_directory_files_essay_files_in_separate_directories(essay_dir_J2)
J2_autograded <- autograder_independent_task_done(tehtava_J2, "J2", min_words = 39)  # muokkaa tähän
kayttaja_idt <- kayttaja_idt %>% left_join(J2_autograded, by = c("Nimi" = "name"))

# 4) 1C-tehtävä (essee)
tehtava_1c <- read_directory_files_essay_files_in_separate_directories(essay_dir_1c)
tehtava_1c_autograded <- autograder_independent_task_done(tehtava_1c, "1c", min_words = 25)  # muokkaa tähän
kayttaja_idt <- kayttaja_idt %>% left_join(tehtava_1c_autograded, by = c("Nimi" = "name"))

# 5) 1B-avotentti
tehtavat_1b <- autograder_open_exam(
  open_exam_file,
  required_language = "fi",
  min_words = c(7, 7),  # muokkaa tähän – yksi arvo per Vastaus-sarake tai yksi arvo kaikille
  points    = c(2, 3)   # muokkaa tähän – yksi arvo per Vastaus-sarake tai yksi arvo kaikille
)
kayttaja_idt <- kayttaja_idt %>% left_join(tehtavat_1b, by = c("email" = "Sähköpostiosoite"))

# 6) Jaa Moodle-vienti, käsin tarkastettavat ja puuttuvat
hyvaksytty_cols <- c("arvosana_J2", "palaute_1b", "arvosana_1c")

vienti <- kayttaja_idt %>%
  filter(if_all(all_of(hyvaksytty_cols), ~ . == "Hyväksytty"))

tehtava_puuttuu <- kayttaja_idt %>%
  filter(if_any(all_of(hyvaksytty_cols), is.na))

kasi_tarkastus <- kayttaja_idt %>%
  filter(!if_all(all_of(hyvaksytty_cols), ~ . == "Hyväksytty"),
         if_all(all_of(hyvaksytty_cols), ~ !is.na(.)))

write.csv2(vienti, file.path(output_dir, "alkuosan_arvosanat_vienti_moodleen.csv"), row.names = FALSE, na = "")
message("Moodle-vienti kirjoitettu: ", nrow(vienti), " opiskelijaa")

write.csv2(kasi_tarkastus, file.path(output_dir, "alkuosan_arvosanat_käsin_tarkastus.csv"), row.names = FALSE, na = "")
message("Käsin tarkastus: ", nrow(kasi_tarkastus), " opiskelijaa")

write.csv2(tehtava_puuttuu, file.path(output_dir, "alkuosan_arvosanat_tehtava_puuttuu.csv"), row.names = FALSE, na = "")
message("Tehtävä puuttuu: ", nrow(tehtava_puuttuu), " opiskelijaa")
