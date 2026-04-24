# run_combine_open_exams.R
#
# Kokoaa arvioitujen avotenttien tiedostot koreittain, liittää sanalliset
# palautteet ja tuottaa Digicampukseen vietävän tiedoston.
# Muokkaa alla olevaa KONFIGURAATIO-osiota kurssikohtaisesti.
#
# Riippuvuudet:
source("helpers_combine_graded.R")


# =============================================================================
# KONFIGURAATIO – muokkaa täältä
# =============================================================================

# Hakemisto, jossa arvioijien palauttamat Excel-tiedostot ovat
graded_dir <- "teemaviikkojen_arviot/avotentit_valmiit/"

# Polku arviointikriteerit-Excel:iin.
# Sarakkeet: kori, kysymys, kriteeri, pisteet, sanallinen_arvio
# Funktion käyttämä id-sarake luodaan automaattisesti: paste(kori, kysymys, sep="_")
criteria_file <- "teemaviikkojen_arviot/avotentti_arviointikriteerit.xlsx"

# Arvioinnit-Excel Moodlesta (sisältää Tunnistenumero, Sähköpostiosoite, Etunimi, Sukunimi)
user_id_file <- "teemaviikkojen_arviot/arvioinnit.xlsx"

# Välilehtien kuvaus: yksi alkio per arvioitava kori/kysymys-yhdistelmä.
#
# sheet_name  = välilehden nimi Excel-tiedostossa (sama kuin jakoskriptissä)
# id          = tunniste arviointikirjan id-sarakkeessa (kori_kysymys)
# kysymys_label = lyhyt tunniste palautemerkkijonoon (esim. "K1", "K2", "K3")
#
# Muokkaa tämä kurssin kysymysrakenteen mukaan:
sheet_map <- list(
  kori1_k1_arvioon = list(id = "kori1_tutkimusasetelma",     kysymys_label = "K1"),
  kori1_k2_arvioon = list(id = "kori1_tutkimuskys_ja_vast",  kysymys_label = "K1"),
  kori1_k3_arvioon = list(id = "kori1_tulkinta",             kysymys_label = "K1"),
  kori2_arvioon    = list(id = "kori2_kaikki2",              kysymys_label = "K2"),
  kori3_arvioon    = list(id = "kori3_kaikki3",              kysymys_label = "K3")
)

# Sähköpostisarakkeen nimi vastaustiedostoissa
id_col <- "Sähköpostiosoite"

# Kommenttisarakkeen nimi (lisätään palautteen loppuun)
comment_col <- "kommentti_opiskelijalle"

# Tulostiedostot (NULL = ei kirjoiteta)
output_xlsx <- "teemaviikkojen_arviot/kaikki_avotentit_valmiit.xlsx"
output_csv  <- "teemaviikkojen_arviot/kaikki_avotentit_valmiit.csv"

# =============================================================================
# AJO
# =============================================================================

combine_graded_open_exams(
  graded_dir    = graded_dir,
  criteria_file = criteria_file,
  user_id_file  = user_id_file,
  sheet_map     = sheet_map,
  id_col        = id_col,
  comment_col   = comment_col,
  output_xlsx   = output_xlsx,
  output_csv    = output_csv
)
