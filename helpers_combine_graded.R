# helpers_combine_graded.R
#
# Funktiot arvioitujen esseeiden ja avotenttien kokoamiseksi,
# sanallisen palautteen liittämiseksi ja Moodleen vietävän
# tiedoston tuottamiseksi.
#
# Riippuvuudet:
source("helpers_rubric_feedback.R")

library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(readxl)
library(writexl)


# Kokoaa arvioitujen esseeiden Excel-tiedostot, liittää sanalliset palautteet
# arviointikirjan avulla ja kirjoittaa tulostiedostot.
#
# graded_dir       : hakemisto, jossa arvioijien palauttamat Excel-tiedostot ovat
# criteria_file    : polku arviointikriteerit-Excel:iin (sarakkeet: kriteeri, pisteet, sanallinen_arvio)
# user_id_file     : polku Moodlen Arvioinnit-Excel:iin (sarake Tunnistenumero käytetään)
# id_cols          : tunnistesarakkeet (oletus: c("name","id","Sähköpostiosoite"))
# comment_col      : opiskelijalle näytettävä kommenttisarake (oletus: "Kommentti_opiskelijalle")
# filter_col       : sarake, jonka perusteella arvioimattomat rivit suodatetaan pois (tai NULL)
# excluded_emails  : poistettavien opiskelijoiden sähköpostiosoitteet (tietosuoja: lista)
# user_id_overrides: nimetty lista muotoa list(email = user_id), ylikirjoittaa user_id-arvon
# score_map        : valinnainen funktio, joka muuntaa pisteet_yhteensä kurssipisteiksi.
#                    Esim. function(x) case_match(x, c(14,15)~40, c(11,12,13)~36, .default=0)
# output_xlsx      : Excel-tulostiedoston polku (tai NULL)
# output_csv       : CSV-tulostiedoston polku (tai NULL, kirjoitetaan WINDOWS-1252)
# output_cols      : sarakkeet CSV:hen (oletus: c("user_id","palaute","pisteet_yhteensä"))
#
# Palauttaa koostetun data framen näkymättömästi
combine_graded_essays <- function(graded_dir,
                                  criteria_file,
                                  user_id_file,
                                  id_cols          = c("name", "id", "Sähköpostiosoite"),
                                  comment_col      = "Kommentti_opiskelijalle",
                                  filter_col       = NULL,
                                  excluded_emails  = character(0),
                                  user_id_overrides = list(),
                                  score_map        = NULL,
                                  output_xlsx      = NULL,
                                  output_csv       = NULL,
                                  output_cols      = c("user_id", "palaute", "pisteet_yhteensä")) {

  # 1) Lue kaikki tiedostot
  file_list <- list.files(graded_dir, full.names = TRUE)
  esseet    <- lapply(file_list, read_excel) %>% bind_rows()

  # 2) Suodata arvioimattomat rivit
  if (!is.null(filter_col) && filter_col %in% names(esseet)) {
    esseet <- esseet %>% filter(!is.na(.data[[filter_col]]))
  }

  # 3) Liitä sanalliset palautteet
  kriteerit <- read_excel(criteria_file)

  tulos <- combine_numerical_grades_with_verbal_feedback(
    tehtava_arvioitu = esseet,
    arviointikirja   = kriteerit,
    id_cols          = id_cols,
    comment_col      = comment_col,
    drop_all_na_rows = TRUE
  )

  # 4) Liitä käyttäjätunnukset Arvioinnit-Excel:n Tunnistenumero-sarakkeesta
  kayttaja_idt <- read_moodle_user_ids(user_id_file)
  email_id_col <- intersect(c("Sähköpostiosoite", "email"), id_cols)[1]

  tulos <- tulos %>%
    left_join(kayttaja_idt, by = setNames("email", email_id_col))

  # 5) Poista opiskelijat, joilta puuttuu palautus
  if (length(excluded_emails) > 0) {
    tulos <- tulos %>% filter(!.data[[email_id_col]] %in% excluded_emails)
  }

  # 6) Ylikirjoita käyttäjätunnukset tarvittaessa
  for (em in names(user_id_overrides)) {
    tulos <- tulos %>%
      mutate(user_id = ifelse(.data[[email_id_col]] == em, user_id_overrides[[em]], user_id))
  }

  # 7) Pisteiden muunnos (valinnainen)
  if (!is.null(score_map)) {
    tulos <- tulos %>% mutate(pisteet_kurssille = score_map(pisteet_yhteensä))
    # päivitä output_cols automaattisesti jos pisteet_kurssille halutaan
    if ("pisteet_yhteensä" %in% output_cols) {
      output_cols <- c(setdiff(output_cols, "pisteet_yhteensä"), "pisteet_kurssille")
    }
  }

  # 8) Kirjoita tulostiedostot
  if (!is.null(output_xlsx)) {
    write_xlsx(tulos, output_xlsx)
    message("Kirjoitettu: ", output_xlsx)
  }

  if (!is.null(output_csv)) {
    valid_cols <- intersect(output_cols, names(tulos))
    write.csv2(tulos %>% select(all_of(valid_cols)),
               output_csv, fileEncoding = "WINDOWS-1252", row.names = FALSE, na = "")
    message("Kirjoitettu: ", output_csv)
  }

  invisible(tulos)
}


# Kokoaa arvioitujen avotenttien Excel-tiedostot koreittain,
# liittää sanalliset palautteet ja yhdistää kaikki kysymykset per opiskelija.
#
# graded_dir       : hakemisto, jossa arvioijien palauttamat Excel-tiedostot ovat
# criteria_file    : polku arviointikriteerit-Excel:iin
#                    (sarakkeet: id (kori_kysymys), kriteeri, pisteet, sanallinen_arvio, kysymys, kori)
# user_id_file     : polku Moodlen Arvioinnit-Excel:iin (sarake Tunnistenumero käytetään)
# sheet_map        : nimetty lista muotoa list(sheet_name = list(id="kori1_k1", label="k1", kysymys_label="K1"))
#                    Kukin alkio kuvaa yhden arviointivälilehden.
# id_col           : sähköpostisarake vastauksissa (oletus: "Sähköpostiosoite")
# comment_col      : kommenttisarake (oletus: "kommentti_opiskelijalle")
# output_xlsx      : Excel-tulostiedoston polku (tai NULL)
# output_csv       : CSV-tulostiedoston polku (tai NULL)
#
# Palauttaa yhdistetyn wide-data framen näkymättömästi
combine_graded_open_exams <- function(graded_dir,
                                      criteria_file,
                                      user_id_file,
                                      sheet_map,
                                      id_col      = "Sähköpostiosoite",
                                      comment_col = "kommentti_opiskelijalle",
                                      output_xlsx = NULL,
                                      output_csv  = NULL) {

  file_list          <- list.files(graded_dir, full.names = TRUE)
  arviointikriteerit <- read_excel(criteria_file) %>%
    mutate(id = paste(kori, kysymys, sep = "_"))

  # Lue ja käsittele jokainen välilehti
  question_results <- lapply(names(sheet_map), function(sheet_name) {
    meta   <- sheet_map[[sheet_name]]
    raw    <- lapply(file_list, read_excel, sheet = sheet_name) %>% bind_rows()

    result <- process_question(
      df                 = raw,
      arviointikriteerit = arviointikriteerit,
      kysymys_id         = meta$id,
      kysymys_label      = meta$label,
      id_cols            = id_col,
      comment_col        = comment_col
    )

    result %>%
      select(all_of(id_col), pisteet, sanallinen_palaute) %>%
      mutate(
        kysymys          = meta$kysymys_label,
        sanallinen_palaute = paste0(meta$kysymys_label, ": ", sanallinen_palaute)
      )
  })

  kaikki <- bind_rows(question_results)

  # Pivotoi leveäksi
  kysymykset <- unique(kaikki$kysymys)

  kaikki_wide <- kaikki %>%
    pivot_wider(
      names_from  = kysymys,
      values_from = c(pisteet, sanallinen_palaute),
      names_glue  = "{.value}_{kysymys}"
    ) %>%
    mutate(
      palaute = do.call(function(...) paste(..., sep = "\n\n"),
                        select(., starts_with("sanallinen_palaute_"))),
      pisteet = rowSums(select(., starts_with("pisteet_")), na.rm = TRUE)
    )

  # Liitä käyttäjätunnukset Arvioinnit-Excel:n Tunnistenumero-sarakkeesta
  kayttaja_idt <- read_moodle_user_ids(user_id_file)
  kaikki_wide  <- kaikki_wide %>%
    left_join(kayttaja_idt, by = setNames("email", id_col))

  if (!is.null(output_xlsx)) {
    write_xlsx(kaikki_wide, output_xlsx)
    message("Kirjoitettu: ", output_xlsx)
  }

  if (!is.null(output_csv)) {
    write.csv2(kaikki_wide, output_csv,
               fileEncoding = "WINDOWS-1252", row.names = FALSE, na = "")
    message("Kirjoitettu: ", output_csv)
  }

  invisible(kaikki_wide)
}
