# helpers_distribute_assignments.R
#
# Funktiot palautusten jakamiseksi arvioijille Excel-tiedostoihin.
#
# Kaksi pääfunktiota:
#   distribute_essays()     – esseet hakemistosta arvioijille
#   distribute_open_exams() – avotenttivastaukset CSV:stä arvioijille (tukee korijakoa)
#
# Riippuvuudet:
source("helpers_read_files.R")

library(dplyr)
library(stringr)
library(readr)
library(readxl)
library(writexl)
library(tidyr)


# Jakaa esseepalaututukset arvioijille.
#
# essay_dir        : hakemisto, jossa palautetut esseet (yksi opiskelija per alihakemisto)
# arvioinnit_file  : polku Moodlen Arvioinnit-Excel:iin.
#                    Sieltä luetaan Tunnistenumero (user_id), Sähköpostiosoite ja Nimi.
# rubric_cols      : merkkijonovektori, arviointisarakkeiden nimet (asetetaan NA:ksi)
# graders          : arvioijien nimivektori, esim. c("raakel", "aino").
#                    Opiskelijat jaetaan tasan arvioijien kesken.
# output_dir       : hakemisto, johon Excel-tiedostot kirjoitetaan
# excluded_emails  : poistettavat sähköpostiosoitteet (tietosuoja: lista)
#
# Sivuvaikutus: kirjoittaa Excel-tiedostot output_dir-hakemistoon
distribute_essays <- function(essay_dir,
                              arvioinnit_file,
                              rubric_cols,
                              graders         = c("arvioija1", "arvioija2"),
                              output_dir      = ".",
                              excluded_emails = character(0)) {

  # 1) Lue esseet
  esseet <- read_directory_files_essay_files_in_separate_directories(essay_dir) %>%
    select(-id)

  # 2) Lisää tyhjät arviointisarakkeet
  esseet[, rubric_cols] <- NA

  # 3) Lue tunnistetiedot Arvioinnit-Excel:stä (email, user_id, Nimi)
  kayttaja_idt <- read_moodle_user_ids(arvioinnit_file)

  # 4) Suodata poistetut sähköpostit
  if (length(excluded_emails) > 0) {
    kayttaja_idt <- kayttaja_idt %>% filter(!email %in% excluded_emails)
  }

  # 5) Liitä opiskelijan tiedot esseisiin
  esseet <- esseet %>%
    left_join(kayttaja_idt, by = c("name" = "Nimi"))

  # 6) Jaa arvioijille tasan ja kirjoita tiedostot
  n <- nrow(esseet)
  k <- length(graders)
  groups <- ceiling(seq_len(n) * k / n)
  for (i in seq_along(graders)) {
    write_xlsx(esseet[groups == i, ],
               file.path(output_dir, paste0("esseet_arviointiin_", graders[i], ".xlsx")))
    message("Kirjoitettu: ", graders[i])
  }

  invisible(esseet)
}


# Jakaa avotenttivastaukset arvioijille.
#
# Tukee Moodlen korijakoa (eri opiskelijoilla eri kysymysvariantit kysymyksessä 1).
#
# answers_file       : polku Moodlen avotentti-CSV:hen
# criteria_file      : polku arviointikriteerit-Excel:iin (sarakkeet: kori, kysymys, kriteeri)
# kori1_keyword_col  : sarakkeen nimi, josta korijakoa etsitään (esim. "Kysymys 1")
# kori1_keywords     : nimetty lista muotoa list(avain1 = "tutkimusasetelma", ...)
#                      käytetään str_detect-hakuun. NULL = ei korijakoa.
# drop_cols          : poistettavat sarakkeet vastaustiedostosta
# graders            : arvioijien nimivektori, esim. c("raakel", "aino").
#                      Opiskelijat jaetaan tasan arvioijien kesken.
# output_dir         : hakemisto, johon Excel-tiedostot kirjoitetaan
#
# Sivuvaikutus: kirjoittaa Excel-tiedostot (yksi per arvioija, monta välilehteä)
distribute_open_exams <- function(answers_file,
                                  criteria_file,
                                  kori1_keyword_col = "Kysymys 1",
                                  kori1_keywords    = NULL,
                                  drop_cols         = c("Tila", "Aloitettiin", "Suoritettu",
                                                        "Suorituskerran kesto", "Arvosana/18"),
                                  graders           = c("arvioija1", "arvioija2"),
                                  output_dir        = ".") {

  kaikki_vastaukset  <- read_csv(answers_file, show_col_types = FALSE)
  arviointikriteerit <- read_excel(criteria_file)

  # Poista turhat sarakkeet
  drop_existing <- intersect(drop_cols, names(kaikki_vastaukset))
  if (length(drop_existing) > 0) {
    kaikki_vastaukset <- kaikki_vastaukset %>% select(-all_of(drop_existing))
  }

  # Tunnista kori (jos korijakoa käytetään)
  if (!is.null(kori1_keywords)) {
    kw_names <- names(kori1_keywords)
    kaikki_vastaukset <- kaikki_vastaukset %>%
      mutate(lyhytnimi_kysymys1 = case_when(
        !!!setNames(
          lapply(kori1_keywords, function(kw) expr(str_detect(!!sym(kori1_keyword_col), !!kw))),
          kw_names
        ),
        TRUE ~ "Ongelma"
      ))
  }

  # Yhdistä vastaukset arviointikriteereihin (combine_responses_and_criteria)
  .combine <- function(df, keyword, crit_keyword) {
    kriteerit <- arviointikriteerit %>% filter(kysymys == crit_keyword)
    nimet <- c(unique(kriteerit$kriteeri), "kommentti_opiskelijalle", "kommentti_Arholle")
    if (!is.null(kori1_keywords)) {
      df <- df %>% filter(str_detect(lyhytnimi_kysymys1, keyword))
    }
    df[, nimet] <- NA
    df
  }

  # Rakenna kori-dataframet
  n_questions <- sum(str_detect(names(kaikki_vastaukset), "^Kysymys \\d+$"))
  sheets <- list()

  if (!is.null(kori1_keywords)) {
    for (kw in names(kori1_keywords)) {
      crit_kw <- kori1_keywords[[kw]]
      df_k    <- kaikki_vastaukset %>%
        select(Sähköpostiosoite, matches("^Kysymys 1$|^Vastaus 1$"), lyhytnimi_kysymys1)
      sheets[[paste0("kori1_", kw, "_arvioon")]] <- .combine(df_k, kw, crit_kw)
    }
    # Kori 2 eteenpäin
    for (q in seq(2, n_questions)) {
      df_q <- kaikki_vastaukset %>%
        select(Sähköpostiosoite, matches(paste0("^Kysymys ", q, "$|^Vastaus ", q, "$")))
      crit_key <- paste0("kaikki", q)
      sheets[[paste0("kori", q, "_arvioon")]] <- .combine(df_q, "ei", crit_key)
    }
  } else {
    for (q in seq_len(n_questions)) {
      df_q <- kaikki_vastaukset %>%
        select(Sähköpostiosoite, matches(paste0("^Kysymys ", q, "$|^Vastaus ", q, "$")))
      crit_key <- paste0("kaikki", q)
      sheets[[paste0("kori", q, "_arvioon")]] <- .combine(df_q, "ei", crit_key)
    }
  }

  # Jaa arvioijille tasan ja kirjoita tiedostot
  all_emails <- unique(unlist(lapply(sheets, `[[`, "Sähköpostiosoite")))
  n <- length(all_emails)
  k <- length(graders)
  groups <- ceiling(seq_len(n) * k / n)
  for (i in seq_along(graders)) {
    grp_emails <- all_emails[groups == i]
    subset     <- lapply(sheets, function(df) df %>% filter(Sähköpostiosoite %in% grp_emails))
    write_xlsx(subset, path = file.path(output_dir, paste0("avotentit_arvioon_", graders[i], ".xlsx")))
    message("Kirjoitettu: ", graders[i])
  }

  invisible(sheets)
}
