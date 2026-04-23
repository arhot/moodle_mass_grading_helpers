# run_combine_essays.R
#
# Kokoaa arvioitujen esseeiden tiedostot, liittää sanalliset palautteet
# ja tuottaa Digicampukseen vietävän tiedoston.
# Muokkaa alla olevaa KONFIGURAATIO-osiota kurssikohtaisesti.
#
# Riippuvuudet:
source("helpers_combine_graded.R")


# =============================================================================
# KONFIGURAATIO – muokkaa täältä
# =============================================================================

# Hakemisto, jossa arvioijien palauttamat Excel-tiedostot ovat
graded_dir <- "teemaviikkojen_arviot/esseet_valmiit/"

# Polku arviointikriteerit-Excel:iin
# Sarakkeet: kriteeri, pisteet, sanallinen_arvio
criteria_file <- "teemaviikkojen_arviot/esseen_arviointikriteerit.xlsx"

# Arvioinnit-Excel Moodlesta (sisältää Tunnistenumero, Sähköpostiosoite, Etunimi, Sukunimi)
user_id_file <- "teemaviikkojen_arviot/arvioinnit.xlsx"

# Tunnistesarakkeet (säilytetään tuloksessa)
id_cols <- c("name", "id", "Sähköpostiosoite")

# Sarake, jonka sisältö lisätään palautteen loppuun (opiskelijalle näkyvä)
comment_col <- "Kommentti_opiskelijalle"

# Ensimmäinen arviointisarake – käytetään suodattamaan arvioimattomat rivit pois.
# NULL = ei suodateta.
filter_col <- "kriteeri_1"

# Poistettavat sähköpostiosoitteet (opiskelijat ilman palautusta)
excluded_emails <- c(
  # "opiskelija@tuni.fi"
)

# Käyttäjätunnusten manuaaliset korjaukset (key = sähköposti, value = user_id)
user_id_overrides <- list(
  # "opiskelija@tuni.fi" = 123456
)

# Pisteiden muunnostaulukko: muuntaa arviointikirjan pisteet kurssipisteiksi.
# NULL = ei muunnosta (käytetään suoraan pisteet_yhteensä).
# Esimerkki: esseen 15-pisteasteikko → kurssin 40-pisteasteikko
score_map <- NULL
# score_map <- function(x) dplyr::case_match(
#   x,
#   c(14, 15)    ~ 40,
#   c(11, 12, 13) ~ 36,
#   c(8, 9, 10)  ~ 32,
#   c(5, 6, 7)   ~ 28,
#   c(2, 3, 4)   ~ 24,
#   .default     = 0
# )

# Tulostiedostot (NULL = ei kirjoiteta)
output_xlsx <- "teemaviikkojen_arviot/kaikki_esseet_valmiit.xlsx"
output_csv  <- "teemaviikkojen_arviot/kaikki_esseet_valmiit.csv"

# Sarakkeet CSV-tiedostoon
output_cols <- c("user_id", "palaute", "pisteet_yhteensä")

# =============================================================================
# AJO
# =============================================================================

combine_graded_essays(
  graded_dir        = graded_dir,
  criteria_file     = criteria_file,
  user_id_file      = user_id_file,
  id_cols           = id_cols,
  comment_col       = comment_col,
  filter_col        = filter_col,
  excluded_emails   = excluded_emails,
  user_id_overrides = user_id_overrides,
  score_map         = score_map,
  output_xlsx       = output_xlsx,
  output_csv        = output_csv,
  output_cols       = output_cols
)
