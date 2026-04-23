# helpers_autograder.R
#
# Automaattinen arviointi lyhyille tekstitehtäville:
# tarkistetaan kielen tunnistuksella ja sanamäärällä,
# onko palautus hyväksyttävissä automaattisesti vai
# tarvitaanko käsin tarkistus.
#
# Riippuvuudet:
library(dplyr)
library(stringr)
library(cld2)    # kielentunnistus


# Arvioi lyhyet kirjoitustehtävät automaattisesti.
#
# df                  : data frame, jossa sarake `teksti` (opiskelijan palautus)
# assignment_identifier: lyhyt tunniste tehtävälle (esim. "J2", "1c"), käytetään sarakkeiden nimissä
# min_words           : vähimmäissanamäärä hyväksynnälle (oletus: 60)
# required_language   : vaadittu kieli kielitunnistukselle (oletus: "fi")
#
# Palauttaa data framen, johon on lisätty sarakkeet:
#   arvosana_<id>  : "Hyväksytty" / "Hylätty" / "Käsin_tarkistus"
#   kieli_<id>     : tunnistettu kieli
#   pituus_<id>    : sanamäärä
#   teksti_<id>    : alkuperäinen teksti
# (alkuperäiset `teksti`, `kieli`, `pituus` sarakkeet poistetaan)
autograder_independent_task_done <- function(df, assignment_identifier,
                                             min_words = 60,
                                             required_language = "fi") {
  mutate(
    df,
    kieli  = cld2::detect_language(teksti),
    pituus = str_count(teksti, "\\w+"),
    "arvosana_{assignment_identifier}" := case_when(
      kieli == required_language & pituus > min_words ~ "Hyväksytty",
      pituus == 0                                      ~ "Hylätty",
      TRUE                                             ~ "Käsin_tarkistus"
    ),
    "kieli_{assignment_identifier}"  := kieli,
    "pituus_{assignment_identifier}" := pituus,
    "teksti_{assignment_identifier}" := teksti
  ) %>%
    select(-kieli, -pituus, -teksti)
}


# Arvioi avotentti-vastaukset automaattisesti kielentunnistuksen ja sanamäärän perusteella.
# Toimii kaikille vastausmäärille: sarakkeet "Vastaus 1", "Vastaus 2", jne. tunnistetaan automaattisesti.
#
# path              : polku avotentti-CSV-tiedostoon (Moodle-vienti)
# required_language : vaadittu kieli kielentunnistukselle (esim. "fi")
# min_words         : vähimmäissanamäärät hyväksynnälle – joko yksi luku (käytetään kaikille vastauksille)
#                     tai lista, jonka pituus vastaa Vastaus-sarakkeiden määrää
# points            : pisteet per vastaus – sama logiikka kuin min_words
#
# Palauttaa data framen, johon on lisätty sarakkeet:
#   kieli_<i>, pituus_<i>, arvosana_pisteet_<i>   (per vastaus)
#   arvosana_pisteet_1b_yhteensa, arvosana_final_1b, palaute_1b
autograder_open_exam <- function(path, required_language = "fi", min_words, points) {
  df <- readr::read_csv(path, show_col_types = FALSE) %>%
    group_by(Sähköpostiosoite) %>% slice_tail(n = 1) %>% ungroup()

  vastaus_cols <- names(df)[stringr::str_detect(names(df), "^Vastaus \\d+$")]
  n <- length(vastaus_cols)

  if (n == 0) {
    stop("CSV-tiedostosta ei löydy 'Vastaus X' -sarakkeita: ", path, call. = FALSE)
  }

  # Validoi min_words
  if (length(min_words) == 1) {
    min_words <- rep(min_words, n)
  } else if (length(min_words) != n) {
    stop(
      "min_words-listan pituuden on oltava 1 tai ", n,
      " (vastaa Vastaus-sarakkeiden määrää), mutta se on ", length(min_words),
      call. = FALSE
    )
  }

  # Validoi points
  if (length(points) == 1) {
    points <- rep(points, n)
  } else if (length(points) != n) {
    stop(
      "points-listan pituuden on oltava 1 tai ", n,
      " (vastaa Vastaus-sarakkeiden määrää), mutta se on ", length(points),
      call. = FALSE
    )
  }

  # Laske kieli, pituus ja pisteet per vastaus
  for (i in seq_along(vastaus_cols)) {
    col <- vastaus_cols[i]
    df[[paste0("kieli_", i)]]            <- cld2::detect_language(df[[col]])
    df[[paste0("pituus_", i)]]           <- stringr::str_count(df[[col]], "\\w+")
    df[[paste0("arvosana_pisteet_", i)]] <- ifelse(df[[paste0("pituus_", i)]] > min_words[i],
                                                    points[i], 0)
  }

  kieli_cols   <- paste0("kieli_", seq_len(n))
  pituus_cols  <- paste0("pituus_", seq_len(n))
  pisteet_cols <- paste0("arvosana_pisteet_", seq_len(n))

  df %>% mutate(
    .any_lang_ok    = rowSums(across(all_of(kieli_cols), ~ . == required_language),
                              na.rm = TRUE) > 0,
    .total_pituus   = rowSums(across(all_of(pituus_cols)), na.rm = TRUE),
    arvosana_pisteet_1b_yhteensa = rowSums(across(all_of(pisteet_cols)), na.rm = TRUE),
    arvosana_final_1b = if_else(
      .any_lang_ok & .total_pituus > sum(min_words),
      "Hyväksytty", "Käsin_tarkistus"
    ),
    palaute_1b = case_when(
      arvosana_final_1b == "Käsin_tarkistus"                      ~ "Ei suoritusta",
      arvosana_pisteet_1b_yhteensa < sum(points)                  ~
        "Ainakin yksi vastaus puuttui tai oli puutteellinen. Suoritus on kuitenkin kokonaisuutena hyväksytty.",
      TRUE                                                         ~ "Hyväksytty"
    )
  ) %>%
    select(-.any_lang_ok, -.total_pituus)
}
