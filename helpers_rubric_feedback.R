# helpers_rubric_feedback.R
#
# Funktiot numeeristen arvostelukriteerien yhdistämiseksi
# sanallisiin palauteteksteihin ja yhteispisteiden laskemiseksi.
#
# Sisältää myös process_question()-funktion, jota käytetään
# avotenttien ja esseetehtävien yhdistämisskripteissä.
#
# Riippuvuudet:
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)


# Apufunktiot -----------------------------------------------------------

# Tarkistaa, mitkä arviointikirjan kriteerit puuttuvat
# tehtävädatasta sarakkeina.
missing_criteria_cols <- function(tehtava_arvioitu, arviointikirja) {
  required <- unique(arviointikirja$kriteeri)
  required[!required %in% names(tehtava_arvioitu)]
}


# Kokoaa kaikki sanallinen_arvio_*-sarakkeet yhdeksi palaute-merkkijonoksi
# per opiskelija. Lisää haluttaessa erillisen kommenttisarakkeen loppuun.
build_feedback <- function(df, comment_col = NULL) {
  arvio_cols <- grep("^sanallinen_arvio_", names(df), value = TRUE)
  df %>%
    mutate(
      palaute = pmap_chr(
        .l = c(
          across(all_of(arvio_cols)),
          if (!is.null(comment_col) && comment_col %in% names(df)) across(all_of(comment_col)) else list()
        ),
        .f = function(...) {
          parts <- c(...) %>% unlist(use.names = FALSE) %>% na.omit()
          paste(parts, collapse = " ")
        }
      )
    )
}


# Pääfunktiot -----------------------------------------------------------

# Yhdistää nimelliset pisteet sanalliseen palautteeseen arviointikirjan avulla.
#
# tehtava_arvioitu : data frame, jossa yksi rivi per opiskelija ja yksi sarake
#                   per kriteeri (sarakkeen nimi = kriteerin nimi, arvo = pisteet)
# arviointikirja   : data frame, sarakkeet: kriteeri, pisteet, sanallinen_arvio
# id_cols          : sarakkeet, jotka säilytetään tunnistetietoina
# comment_col      : valinnainen sarake, jonka sisältö lisätään palautteen loppuun
# drop_all_na_rows : poistetaanko rivit, joilla kaikki pisteet puuttuvat
#
# Palauttaa data framen: id_cols + pisteet_* + pisteet_yhteensä + palaute
combine_numerical_grades_with_verbal_feedback <- function(
    tehtava_arvioitu,
    arviointikirja,
    id_cols          = c("Etunimi", "Sukunimi", "Opiskelijanumero", "Sähköpostiosoite"),
    comment_col      = "kommentti_opiskelijalle",
    drop_all_na_rows = TRUE
) {
  miss <- missing_criteria_cols(tehtava_arvioitu, arviointikirja)
  if (length(miss) > 0) {
    stop(sprintf(
      "Seuraavat arviointikriteerit puuttuvat tehtävädatasta sarakkeina:\n- %s",
      paste(miss, collapse = "\n- ")
    ), call. = FALSE)
  }

  criteria     <- unique(arviointikirja$kriteeri)
  id_cols_full <- unique(c(id_cols, if (comment_col %in% names(tehtava_arvioitu)) comment_col))

  dat <- tehtava_arvioitu
  if (drop_all_na_rows) {
    dat <- dat %>% filter(if_any(all_of(criteria), ~ !is.na(.)))
  }

  out <- dat %>%
    pivot_longer(cols = all_of(criteria), names_to = "kriteeri", values_to = "pisteet") %>%
    left_join(arviointikirja, by = c("kriteeri", "pisteet")) %>%
    pivot_wider(
      id_cols    = all_of(id_cols_full),
      names_from = "kriteeri",
      values_from = c("pisteet", "sanallinen_arvio"),
      names_sep  = "_"
    )

  out <- build_feedback(
    out,
    comment_col = if (comment_col %in% names(tehtava_arvioitu)) comment_col else NULL
  )

  pisteet_cols <- grep("^pisteet_", names(out), value = TRUE)
  out <- out %>%
    mutate(pisteet_yhteensä = rowSums(across(all_of(pisteet_cols)), na.rm = TRUE))

  # Varoita puuttuvista arvioista
  email_col <- intersect(c("Sähköpostiosoite", "email"), id_cols)[1]
  if (!is.na(email_col)) {
    incomplete <- out %>%
      filter(if_any(all_of(pisteet_cols), is.na)) %>%
      pull(all_of(email_col))
    if (length(incomplete) > 0) {
      warning(sprintf(
        "Seuraavilla opiskelijoilla on puuttuvia arvosanoja: %s",
        paste(incomplete, collapse = ", ")
      ))
    }
  }

  out %>% select(all_of(id_cols), all_of(pisteet_cols), pisteet_yhteensä, palaute)
}


# Yhdistää yhden kysymyksen tai tehtäväkorin pisteet ja sanalliset arviot.
# Käytetään avotenttien ja esseetehtävien yhdistämisskripteissä.
#
# df               : data frame graatuista suorituksista
# arviointikriteerit : data frame, sarakkeet: id (kori_kysymys), kriteeri, pisteet, sanallinen_arvio, kysymys
# kysymys_id       : merkkijono, jonka perusteella id-sarake suodatetaan (str_detect)
# kysymys_label    : arviointikirjan kysymys-sarakkeen arvo tähän kysymykseen
# id_cols          : tunnistesarakkeet (voi olla useita, esim. c("Sähköpostiosoite") tai c("email","user_id"))
# comment_col      : valinnainen kommenttisarake, joka lisätään palautteen loppuun
#
# Palauttaa data framen: id_cols + value_* + sanallinen_arvio_* + pisteet + sanallinen_palaute
process_question <- function(df, arviointikriteerit, kysymys_id, kysymys_label,
                             id_cols     = "Sähköpostiosoite",
                             comment_col = "kommentti_opiskelijalle") {
  has_comment <- comment_col %in% names(df)

  criteria      <- arviointikriteerit %>%
    filter(str_detect(id, kysymys_id)) %>%
    pull(kriteeri) %>%
    unique()

  value_cols    <- paste0("value_", criteria)
  feedback_cols <- paste0("sanallinen_arvio_", criteria)
  select_cols   <- unique(c(id_cols, criteria, if (has_comment) comment_col))
  pivot_id_cols <- unique(c(id_cols, if (has_comment) comment_col))

  result <- df %>%
    select(all_of(select_cols)) %>%
    pivot_longer(cols = all_of(criteria)) %>%
    left_join(
      arviointikriteerit %>% filter(kysymys == kysymys_label),
      by = c("name" = "kriteeri", "value" = "pisteet")
    ) %>%
    pivot_wider(
      id_cols     = all_of(pivot_id_cols),
      names_from  = name,
      values_from = c("value", "sanallinen_arvio")
    ) %>%
    mutate(
      pisteet       = rowSums(select(., all_of(value_cols)), na.rm = TRUE),
      feedback_base = str_squish(do.call(paste, c(select(., all_of(feedback_cols)), sep = " ")))
    )

  if (has_comment) {
    result <- result %>%
      mutate(sanallinen_palaute = if_else(
        is.na(.data[[comment_col]]),
        feedback_base,
        str_squish(paste(feedback_base, .data[[comment_col]]))
      )) %>%
      select(-feedback_base)
  } else {
    result <- result %>% rename(sanallinen_palaute = feedback_base)
  }

  result
}
