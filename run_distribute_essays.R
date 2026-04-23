# run_distribute_essays.R
#
# Jakaa esseepalaututukset arvioijille.
# Muokkaa alla olevaa KONFIGURAATIO-osiota kurssikohtaisesti.
#
# Riippuvuudet:
source("helpers_distribute_assignments.R")


# =============================================================================
# KONFIGURAATIO – muokkaa täältä
# =============================================================================

# Hakemisto, jossa opiskelijoiden palautetut esseet ovat
# (yksi opiskelija per alihakemisto, Moodlen ZIP-vienti purettuna)
essay_dir <- "teemaviikkojen_arviot/esseet_arvioitavaksi/"

# Arvioinnit-Excel Moodlesta (sisältää Tunnistenumero, Sähköpostiosoite, Etunimi, Sukunimi)
arvioinnit_file <- "teemaviikkojen_arviot/arvioinnit.xlsx"

# Arviointisarakkeet (asetetaan tyhjiksi arvioijia varten)
rubric_cols <- c(
  "kriteeri_1",
  "kriteeri_2",
  "kriteeri_3",
  "huomioita opiskelijalle",
  "huomioita kurssinvetäjälle"
)

# Arvioijaryhmät: merkitse domeeni tai NULL
# Domeeni (esim. "tuni.fi") → kaikki siltä domainilta menevät omaan tiedostoon
# NULL → saa loput; useita NULL-arvoja → loput jaetaan tasan
graders <- list(
  tuni      = "tuni.fi",
  arvioija1 = NULL,
  arvioija2 = NULL
)

# Hakemisto, johon Excel-tiedostot kirjoitetaan
output_dir <- "teemaviikkojen_arviot/"

# Poistettavat sähköpostiosoitteet (ei jaeta arvioijille, esim. testikäyttajät)
excluded_emails <- c(
  # "testi.kayttaja@tuni.fi"
)

# =============================================================================
# AJO
# =============================================================================

distribute_essays(
  essay_dir       = essay_dir,
  arvioinnit_file = arvioinnit_file,
  rubric_cols     = rubric_cols,
  graders         = graders,
  output_dir      = output_dir,
  excluded_emails = excluded_emails
)
