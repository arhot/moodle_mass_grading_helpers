# helpers_sisu_export.R
#
# Funktio loppuarvosanojen laskemiseen ja SISU-vientitiedostojen
# tuottamiseen Helsingin yliopiston ja Tampereen yliopiston opiskelijoille.
#
# Riippuvuudet:
library(dplyr)
library(stringr)
library(readr)
library(readxl)
library(writexl)
library(tidyr)


# Laskee loppuarvosanat ja tuottaa SISU-vientitiedostot.
#
# grading_file      : polku Moodlen arvioinnit-Excel:iin
# column_renames    : nimetty vektori muotoa c(lyhytnimi = "Pitkä Moodle-sarakenimi"),
#                     käytetään rename(all_of(column_renames)):lla
# total_score_col   : sarake, jossa kokonaispisteet (Moodlen "Kurssiyhteenveto")
# gate_conditions   : lista named logical exprs (quosures) tai merkkijono evaluoitavaksi.
#                     Helpoiten: anna gate_expr merkkijonona: esim.
#                     "col_a == 'Hyväksytty' & as.numeric(col_b) > 0"
# gate_expr         : merkkijonona R-lauseke, joka evaluoidaan rivillä; tulos tallennetaan
#                     alkuosa_ja_video_ok-sarakkeeseen ("OK" / "Ei OK").
#                     NULL = ei porttiehtoä, kaikki pisteet kelpaavat.
# grade_thresholds  : nimetty numeerinen vektori pinasteista: c("1"=50,"2"=60,...,"5"=90)
# hy_enrollment_file: polku HY-ilmoittautumistiedostoon (TSV, UTF-16LE)
# tuni_enrollment_file: polku TUNI-ilmoittautumistiedostoon (CSV2)
# hy_email_col      : sähköpostisarakkeen nimi HY-tiedostossa
# tuni_email_col    : sähköpostisarakkeen nimi TUNI-tiedostossa
# hy_student_num_col: opiskelijanumerosarake HY-tiedostossa
# tuni_student_num_col: opiskelijanumerosarake TUNI-tiedostossa
# hy_domain         : HY-sähköpostidomaini (str_detect)
# tuni_domain       : Tampere-sähköpostidomaini (str_detect)
# credits           : opintopisteet (oletus: 5)
# hy_assessment_date: arviointipäivä HY:lle (dd.mm.yyyy)
# tuni_assessment_date: arviointipäivä TUNI:lle (dd.mm.yyyy)
# completion_language: suorituskieli (oletus: "Suomi")
# problem_xlsx      : polku ongelmatapaukset-Excel:iin (valinnainen)
# hy_output_csv     : polku HY SISU -CSV:hen
# tuni_output_csv   : polku TUNI SISU -CSV:hen
#
# Palauttaa listan list(hy = ..., tuni = ...) näkymättömästi
sisu_export <- function(grading_file,
                        column_renames       = character(0),
                        total_score_col      = "Kurssiyhteenveto (Todellinen)",
                        gate_expr            = NULL,
                        grade_thresholds     = c("1" = 50, "2" = 60, "3" = 70, "4" = 80, "5" = 90),
                        hy_enrollment_file   = NULL,
                        tuni_enrollment_file = NULL,
                        hy_email_col         = "ENSISIJAINEN SÄHKÖPOSTI",
                        tuni_email_col       = "ENSISIJAINEN SÄHKÖPOSTI",
                        hy_student_num_col   = "OPISKELIJANUMERO",
                        tuni_student_num_col = "studentNumber",
                        hy_domain            = "helsinki.fi",
                        tuni_domain          = "tuni.fi",
                        credits              = 5,
                        hy_assessment_date   = format(Sys.Date(), "%d.%m.%Y"),
                        tuni_assessment_date = format(Sys.Date(), "%d.%m.%Y"),
                        completion_language  = "Suomi",
                        problem_xlsx         = NULL,
                        hy_output_csv        = NULL,
                        tuni_output_csv      = NULL) {

  arvioinnit <- read_excel(grading_file) %>%
    select(-matches("Palaute"))

  # Nimeä sarakkeet lyhyiksi
  if (length(column_renames) > 0) {
    arvioinnit <- arvioinnit %>% rename(all_of(column_renames))
  }

  # Laske kokonaispisteet
  arvioinnit <- arvioinnit %>%
    mutate(pisteet = suppressWarnings(as.numeric(.data[[total_score_col]])))

  # Porttiehto (alkuosa + video tms.)
  if (!is.null(gate_expr)) {
    arvioinnit <- arvioinnit %>%
      mutate(alkuosa_ja_video_ok = ifelse(eval(parse(text = gate_expr)), "OK", "Ei OK"))
  } else {
    arvioinnit <- arvioinnit %>% mutate(alkuosa_ja_video_ok = "OK")
  }

  # Laske arvosana
  thresholds  <- sort(grade_thresholds)
  grade_cases <- lapply(rev(seq_along(thresholds)), function(i) {
    g <- as.numeric(names(thresholds)[i])
    t <- thresholds[i]
    list(grade = g, threshold = t)
  })

  arvioinnit <- arvioinnit %>%
    mutate(grade = {
      g <- NA_real_
      for (gc in grade_cases) {
        g <- ifelse(pisteet > gc$threshold & alkuosa_ja_video_ok == "OK", gc$grade, g)
      }
      g
    })

  # Ongelmatapaukset (pisteet OK mutta porttiehto ei)
  if (!is.null(problem_xlsx)) {
    ongelma <- arvioinnit %>%
      filter(pisteet > min(thresholds) & alkuosa_ja_video_ok == "Ei OK")
    write_xlsx(ongelma, problem_xlsx)
    message("Ongelmatapaukset kirjoitettu: ", problem_xlsx)
  }

  # HY-vienti
  hy_sisu <- NULL
  if (!is.null(hy_enrollment_file) && !is.null(hy_output_csv)) {
    ilmo_hy <- readr::read_tsv(hy_enrollment_file,
                                locale = readr::locale(encoding = "UTF-16LE"),
                                show_col_types = FALSE)
    hy_sisu <- arvioinnit %>%
      filter(!is.na(grade) & (str_detect(Sähköpostiosoite, hy_domain))) %>%
      left_join(
        ilmo_hy %>% select(
          Sähköpostiosoite = all_of(hy_email_col),
          studentNumber    = all_of(hy_student_num_col)
        ),
        by = "Sähköpostiosoite"
      ) %>%
      filter(!is.na(studentNumber)) %>%
      mutate(credits            = credits,
             assessmentDate     = hy_assessment_date,
             completionLanguage = completion_language,
             comment            = "") %>%
      select(studentNumber, grade, credits, assessmentDate, completionLanguage, comment)

    write.csv2(hy_sisu, hy_output_csv, fileEncoding = "WINDOWS-1252", row.names = FALSE)
    message("HY SISU kirjoitettu: ", hy_output_csv)
  }

  # TUNI-vienti
  tuni_sisu <- NULL
  if (!is.null(tuni_enrollment_file) && !is.null(tuni_output_csv)) {
    ilmo_tuni <- read_csv2(tuni_enrollment_file, show_col_types = FALSE)
    tuni_sisu <- arvioinnit %>%
      filter(!is.na(grade) & str_detect(Sähköpostiosoite, tuni_domain)) %>%
      left_join(
        ilmo_tuni %>% select(
          Sähköpostiosoite = all_of(tuni_email_col),
          studentNumber    = all_of(tuni_student_num_col)
        ),
        by = "Sähköpostiosoite"
      ) %>%
      filter(!is.na(studentNumber)) %>%
      mutate(credits        = credits,
             assessmentDate = tuni_assessment_date) %>%
      select(studentNumber, grade, credits, assessmentDate)

    write.csv2(tuni_sisu, tuni_output_csv, fileEncoding = "WINDOWS-1252", row.names = FALSE)
    message("TUNI SISU kirjoitettu: ", tuni_output_csv)
  }

  invisible(list(hy = hy_sisu, tuni = tuni_sisu))
}
