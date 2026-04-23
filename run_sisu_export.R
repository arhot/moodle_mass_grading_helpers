# run_sisu_export.R
#
# Laskee loppuarvosanat ja tuottaa SISU-vientitiedostot
# Helsingin yliopiston ja Tampereen yliopiston opiskelijoille.
# Muokkaa alla olevaa KONFIGURAATIO-osiota kurssikohtaisesti.
#
# Riippuvuudet:
source("helpers_sisu_export.R")


# =============================================================================
# KONFIGURAATIO – muokkaa täältä
# =============================================================================

# Polku Moodlen arvioinnit-Excel:iin (ladataan Moodlesta: Arvioinnit → Vie Exceliin)
grading_file <- "teemaviikkojen_arviot/ykt_arvioinnit_valmiit.xlsx"

# Sarakkeiden uudelleennimeäminen: c(lyhytnimi = "Pitkä Moodlen sarakenimi")
# Helpottaa käsittelyä ja porttiehdon kirjoittamista.
column_renames <- c(
  t2a   = "Työpaja:2A) Miniessee väestösuhteista ja polarisaatiosta, vertaisarviointi (6 p) / Miniessee väestösuhteista ja polarisaatiosta, vertaisarviointi (palautus) (Todellinen)",
  t2a_p = "Työpaja:2A) Miniessee väestösuhteista ja polarisaatiosta, vertaisarviointi (6 p) / Miniessee väestösuhteista ja polarisaatiosta, vertaisarviointi (arviointi) (Todellinen)",
  t2b   = "Tentti:2B) Monivalintatentti väestösuhteista ja polarisaatiosta (Todellinen)",
  t3a   = "Tentti:3A) Monivalintatentti: sosiaalitieteellinen näkökulma asiantuntijuuteen (8 p) (Todellinen)",
  t3b   = "Tentti:3B) Monivalintatentti: terveys yhteiskunnassa (12 p) (Todellinen)",
  t4    = "Tehtävä:4) Tehtäväohje ja tehtävän palautus: Asuinalue-essee (40p) (Todellinen)",
  t5a   = "Tentti:5A) Monivalintatentti: intersektionaalisuus (8p) (Todellinen)",
  t5b   = "Tentti:5B) Monivalintatentti: Sukupuoli yhteiskunnassa (12p) (Todellinen)"
)

# Sarake, jossa kokonaispisteet (Moodlen "Kurssiyhteenveto")
total_score_col <- "Kurssiyhteenveto (Todellinen)"

# Porttiehto: R-lauseke (merkkijono), joka on TRUE kun opiskelija täyttää
# alkuosan ja/tai videon suoritusvaatimuksen.
# Käytä column_renames-nimistä lyhytnimeä tai Moodlen sarakkeen nimeä.
# NULL = ei porttiehtoa, pisteet riittävät yksin arvosanaan.
gate_expr <- paste0(
  "`Tehtävä:J2) Yhteiskuntatutkimus / Sosiaalitieteet suomalaisissa yliopistoissa (Todellinen)` == 'Hyväksytty' & ",
  "as.numeric(`Tentti:1A) Monivalintatentti: videoluennot (10p) (Todellinen)`) > 0 & ",
  "as.numeric(`Tentti:1B) Avotentti: Suomen luokkarakenne (5 p) (Todellinen)`) > 0 & ",
  "`Tehtävä:1C) Tulkitsemistehtävä: tilasto (5p) (Todellinen)` == 'Hyväksytty' & ",
  "`Tehtävä:6A) Video: Sosiaalitieteelliset jäsennykset (Hyv-Hyl) / Socialvetenskapliga indelningar (Godk. – Underk.) (Todellinen)` == 'Hyväksytty'"
)

# Arvosanarajat: kokonaispisteiden alarajat kullekin arvosanalle (1–5)
grade_thresholds <- c("1" = 50, "2" = 60, "3" = 70, "4" = 80, "5" = 90)

# HY-ilmoittautumistiedosto (TSV, UTF-16LE – ladataan SISUsta tai WebOodista)
hy_enrollment_file <- "teemaviikkojen_arviot/Opiskelijat_Verkko-opetus_Johdatus-sosiaalitieteisiin.csv"
hy_email_col       <- "ENSISIJAINEN SÄHKÖPOSTI"
hy_student_num_col <- "OPISKELIJANUMERO"

# TUNI-ilmoittautumistiedosto (CSV2)
tuni_enrollment_file <- "teemaviikkojen_arviot/Tampereen_ilmoittautuneet.csv"
tuni_email_col       <- "ENSISIJAINEN SÄHKÖPOSTI"
tuni_student_num_col <- "studentNumber"

# Sähköpostidomainit
hy_domain   <- "helsinki.fi"
tuni_domain <- "tuni.fi"

# Opintopisteet ja arviointipäivät
credits              <- 5
hy_assessment_date   <- "20.10.2025"   # dd.mm.yyyy
tuni_assessment_date <- "10.10.2025"
completion_language  <- "Suomi"

# Ongelmatapaukset-tiedosto (opiskelijat joilla pisteet OK mutta porttiehto ei täyty)
# NULL = ei kirjoiteta
problem_xlsx <- "teemaviikkojen_arviot/ongelmatapaukset_alkuosasta_tai_videosta_puuttuu.xlsx"

# Tulostiedostot
hy_output_csv   <- "teemaviikkojen_arviot/UH_sisu.csv"
tuni_output_csv <- "teemaviikkojen_arviot/tuni_sisu.csv"

# =============================================================================
# AJO
# =============================================================================

sisu_export(
  grading_file         = grading_file,
  column_renames       = column_renames,
  total_score_col      = total_score_col,
  gate_expr            = gate_expr,
  grade_thresholds     = grade_thresholds,
  hy_enrollment_file   = hy_enrollment_file,
  tuni_enrollment_file = tuni_enrollment_file,
  hy_email_col         = hy_email_col,
  tuni_email_col       = tuni_email_col,
  hy_student_num_col   = hy_student_num_col,
  tuni_student_num_col = tuni_student_num_col,
  hy_domain            = hy_domain,
  tuni_domain          = tuni_domain,
  credits              = credits,
  hy_assessment_date   = hy_assessment_date,
  tuni_assessment_date = tuni_assessment_date,
  completion_language  = completion_language,
  problem_xlsx         = problem_xlsx,
  hy_output_csv        = hy_output_csv,
  tuni_output_csv      = tuni_output_csv
)
