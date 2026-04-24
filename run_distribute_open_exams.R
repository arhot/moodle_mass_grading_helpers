# run_distribute_open_exams.R
#
# Jakaa avotenttivastaukset arvioijille koreittain.
# Muokkaa alla olevaa KONFIGURAATIO-osiota kurssikohtaisesti.
#
# Riippuvuudet:
source("helpers_distribute_assignments.R")


# =============================================================================
# KONFIGURAATIO – muokkaa täältä
# =============================================================================

# Polku Moodlen avotentti-CSV:hen (ladataan Moodlesta: Tentti → Vastaukset → Vie)
answers_file <- "teemaviikkojen_arviot/avotentti-vastaukset.csv"

# Polku arviointikriteerit-Excel:iin.
# Sarakkeet: kori, kysymys, kriteeri, pisteet, sanallinen_arvio
criteria_file <- "teemaviikkojen_arviot/avotentti_arviointikriteerit.xlsx"

# Sarake, josta kysymyksen 1 korijakoavainsanat etsitään.
# NULL jos korijakoa ei ole (kaikki saavat saman kysymyksen 1).
kori1_keyword_col <- "Kysymys 1"

# Korijakoavainsanat: nimetty lista muotoa list(tunniste = "hakusana").
# Tunniste vastaa arviointikriteerit-tiedoston kysymys-sarakkeen arvoa.
# NULL = ei korijakoa kysymyksessä 1.
kori1_keywords <- list(
  # tutkimusasetelma    = "tutkimusasetelma",
  # tutkimuskys_ja_vast = "tutkimuskysymykset",
  # tulkinta            = "rajoitteita"
)
# Jos ei korijakoa, jätä lista tyhjäksi tai aseta NULL:
# kori1_keywords <- NULL

# Sarakkeet, jotka poistetaan vastaustiedostosta ennen jakoa
drop_cols <- c("Tila", "Aloitettiin", "Suoritettu", "Suorituskerran kesto")
# Lisää tarvittaessa: c(..., "Arvosana/18")

# Arvioijien nimet – opiskelijat jaetaan tasan
graders <- c("arvioija1", "arvioija2")

# Hakemisto, johon Excel-tiedostot kirjoitetaan
output_dir <- "teemaviikkojen_arviot/"

# =============================================================================
# AJO
# =============================================================================

distribute_open_exams(
  answers_file      = answers_file,
  criteria_file     = criteria_file,
  kori1_keyword_col = kori1_keyword_col,
  kori1_keywords    = if (length(kori1_keywords) == 0) NULL else kori1_keywords,
  drop_cols         = drop_cols,
  graders           = graders,
  output_dir        = output_dir
)
