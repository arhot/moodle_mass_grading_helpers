# helpers_read_files.R
#
# Funktiot Moodle-tiedostojen lukemiseen hakemistoista.
# Näitä funktioita käytetään sekä esseetehtävien käsittelyssä
# että plagioinnintutkinnassa.
#
# Riippuvuudet:
library(rvest)
library(xml2)
library(stringr)
library(dplyr)
library(tibble)
library(purrr)
library(readr)


# Lukee Moodlen aktiviteettisuoritusraportin (progress-CSV) ja siivoaa
# ylimääräiset sarakkeet pois.
#
# Moodlen progress-CSV:ssä on kolmenlaisia turhia sarakkeita:
#   1. Ilmoitussarakkeet (otsikko alkaa "dd.mm.yyyy ...")
#   2. Määräpäiväsarakkeet (otsikko on pelkkä ISO-päivämäärä "yyyy-mm-dd ...")
#   3. Tyhjät otsikkosarakkeet (readr: "...N")
#
# Säilytetään sarakkeet, joiden otsikossa on ")" (= tehtävät) sekä
# Nimi- ja Sähköpostiosoite-sarakkeet.
#
# path     : polku progress-CSV-tiedostoon
# encoding : tiedoston merkistö (oletus: "UTF-8"; kokeile "windows-1252" jos ongelmia)
#
# Palauttaa siistin data framen: Nimi, Sähköpostiosoite, ja tehtäväsarakkeet
read_moodle_progress <- function(path, encoding = "UTF-8") {
  raw <- readr::read_csv(
    path,
    locale        = readr::locale(encoding = encoding),
    show_col_types = FALSE,
    name_repair   = "unique"
  )

  # Ensimmäinen sarake on opiskelijan nimi (ei otsikkoa Moodlessa)
  names(raw)[1] <- "Nimi"

  # Tunnista poistettavat sarakkeet
  is_junk_col <- function(nm) {
    # Ilmoitussarakkeet: "16.5.2025 Jokin otsikko..."
    str_detect(nm, "^\\d{1,2}\\.\\d{1,2}\\.\\d{4}") |
    # Määräpäiväsarakkeet: "2023-02-03 05:00:00"
    str_detect(nm, "^\\d{4}-\\d{2}-\\d{2}") |
    # Readrin automaattiset nimet tyhjille otsikoille: "...3", "...17" jne.
    str_detect(nm, "^\\.\\.\\.\\d+$")
  }

  keep <- names(raw)[
    names(raw) %in% c("Nimi", "Sähköpostiosoite") |
    (str_detect(names(raw), "\\)") & !is_junk_col(names(raw)))
  ]

  raw %>% select(all_of(keep))
}


# Lukee opiskelijoiden tunnistetiedot Moodlen Arvioinnit-Excel:stä.
#
# Moodlen Arvioinnit-Excel:ssä on sarake `Tunnistenumero`, joka on opiskelijan
# Moodle-käyttäjätunnus (user_id). Tätä tunnusta tarvitaan Moodlen massatuonnissa.
#
# arvioinnit_file : polku Moodlen arvioinnit-Excel:iin
#
# Palauttaa data framen: email, user_id, Nimi (jos Etunimi ja Sukunimi löytyvät)
read_moodle_user_ids <- function(arvioinnit_file) {
  df <- readxl::read_excel(arvioinnit_file)

  if (!"Tunnistenumero" %in% names(df)) {
    stop("Sarake 'Tunnistenumero' puuttuu tiedostosta: ", arvioinnit_file,
         "\nTarkista Moodlen Arvioinnit-viennin versio.", call. = FALSE)
  }

  result <- df %>%
    select(email = Sähköpostiosoite, user_id = Tunnistenumero) %>%
    mutate(user_id = as.character(user_id)) %>%
    filter(!is.na(email), !is.na(user_id)) %>%
    distinct(email, .keep_all = TRUE)

  # Lisää Nimi-sarake esseiden jakelussa käytettäväksi
  if (all(c("Etunimi", "Sukunimi") %in% names(df))) {
    nimet <- df %>%
      mutate(Nimi = paste(Etunimi, Sukunimi)) %>%
      select(email = Sähköpostiosoite, Nimi) %>%
      distinct(email, .keep_all = TRUE)
    result <- left_join(result, nimet, by = "email")
  }

  result
}


# Lukee HTML-esseepalaututukset, joissa jokainen opiskelija on omassa
# alihakemistossaan (Moodlen oletusvientimuoto).
#
# path_used  : hakemiston polku, jossa opiskelijan palautukset ovat
# file_name  : luettavan tiedoston nimi hakemiston sisällä (oletus: "verkkoteksti.html")
#
# Palauttaa data framen: name, teksti, id
read_directory_files_essay_files_in_separate_directories <- function(path_used, file_name = "verkkoteksti.html") {
  filenames <- list.files(path = path_used)
  datalist <- list()
  for (directory in filenames) {
    name   <- word(directory, 1, sep = "_")
    tiedosto <- paste(path_used, directory, file_name, sep = "/")
    teksti <- rvest::read_html(tiedosto) %>% html_text2()
    datalist[[directory]] <- data.frame(name = name, teksti = teksti)
  }
  assignments <- bind_rows(datalist)
  kaima_nimet <- assignments$name[duplicated(assignments$name)]
  if (length(kaima_nimet) > 0) {
    message("Lisätty _kaima: ", paste(unique(kaima_nimet), collapse = ", "))
    assignments <- assignments %>%
      mutate(name = if_else(duplicated(name), paste0(name, "_kaima"), name))
  }
  assignments <- assignments %>% mutate(id = seq_len(nrow(assignments)))
  assignments
}


# Lukee HTML-esseepalaututukset, joissa kaikki tiedostot ovat samassa
# pakkauksessa (zipissä purettu tasohakemisto).
#
# path_used  : hakemiston polku
#
# Palauttaa data framen: name, teksti, id
read_directory_essay_files_in_one_directory <- function(path_used) {
  filenames <- list.files(path = path_used)
  datalist <- list()
  for (file in filenames) {
    name   <- word(file, 1, sep = "_")
    tiedosto <- paste(path_used, file, sep = "/")
    teksti <- rvest::read_html(tiedosto) %>% html_text2()
    datalist[[file]] <- data.frame(name = name, teksti = teksti)
  }
  assignments <- bind_rows(datalist)
  kaima_nimet <- assignments$name[duplicated(assignments$name)]
  if (length(kaima_nimet) > 0) {
    message("Lisätty _kaima: ", paste(unique(kaima_nimet), collapse = ", "))
    assignments <- assignments %>%
      mutate(name = if_else(duplicated(name), paste0(name, "_kaima"), name))
  }
  assignments <- assignments %>% mutate(id = seq_len(nrow(assignments)))
  assignments
}


# Lukee Moodlen workshop-palautukset yhdestä HTML-tiedostosta.
#
# path_used  : hakemiston polku
#
# Palauttaa data framen: opiskelija, teksti, id
read_directory_files_workshop <- function(path_used) {
  filenames <- list.files(path = path_used)
  datalist <- list()
  for (file in filenames) {
    tiedosto     <- paste(path_used, file, sep = "/")
    kaikki_tiedot <- read_html(tiedosto)
    teksti <- kaikki_tiedot %>%
      html_nodes("div.submission-full") %>%
      html_nodes("div.content") %>%
      html_text()
    opiskelija <- kaikki_tiedot %>%
      html_nodes("div.author") %>%
      html_text() %>%
      word(2, 3)
    datalist[[file]] <- data.frame(opiskelija, teksti)
  }
  assignments <- bind_rows(datalist)
  assignments <- assignments %>% mutate(id = seq_len(nrow(assignments)))
  assignments
}


# Lukee Moodlen opiskelija-ID-sivut hakemistosta ja poimii
# sähköpostiosoitteen, käyttäjätunnuksen ja paikkakunnan.
#
# path_used  : hakemiston polku, jossa opiskelijan profiilisivut ovat HTML-tiedostoina
#
# Palauttaa data framen: email, user_id, paikkakunta, id
read_directory_files_student_id_pages <- function(path_used) {
  filenames <- list.files(path = path_used)
  datalist  <- list()
  for (file in filenames) {
    tiedosto         <- paste(path_used, file, sep = "/")
    kaikki_id_sivulta <- read_html(tiedosto)
    kaikki_linkit    <- kaikki_id_sivulta %>% html_nodes("a") %>% html_attr("href")

    user_id <- kaikki_linkit %>%
      str_subset("user/view.php") %>%
      tail(n = 1) %>%
      str_extract("(?<=id=)(.*?)(?=&)")

    user_email <- kaikki_linkit %>%
      str_subset("mailto:") %>%
      str_extract("(?<=mailto:)(.*)") %>%
      URLdecode()

    paikkakunta <- tibble(
      dd = kaikki_id_sivulta %>% html_nodes("dt") %>% html_text(),
      dt = kaikki_id_sivulta %>% html_nodes("dd") %>% html_text()
    ) %>% filter(dd == "Paikkakunta")

    if (nrow(paikkakunta) == 0) {
      paikkakunta <- tibble(dd = "Paikkakunta", dt = "Ei_paikkakuntaa")
    }

    datalist[[file]] <- data.frame(
      email      = user_email,
      user_id    = user_id,
      paikkakunta = paikkakunta$dt
    )
  }
  users <- bind_rows(datalist)
  users <- users %>% mutate(id = seq_len(nrow(users)))
  users
}


# Lukee Moodlen opiskelija-ID-sivut hakemistosta (uudempi versio,
# käyttää data-userid-attribuuttia ja XPath-haun sähköpostille).
# Suodattaa tiedostot, joiden nimessä on pattern-merkkijono.
#
# path_used  : hakemiston polku
# pattern    : tiedostonimen suodatin (oletus: "", eli kaikki tiedostot)
#
# Palauttaa data framen: filename, email, user_id, paikkakunta, id
read_users <- function(path_used, pattern = "") {
  filenames <- list.files(path = path_used, pattern = pattern, full.names = TRUE)

  datalist <- vector("list", length(filenames))
  names(datalist) <- basename(filenames)

  for (i in seq_along(filenames)) {
    file <- filenames[i]
    doc  <- read_html(file)

    # Sähköposti
    email_node <- html_element(
      doc,
      xpath = "//dt[normalize-space()='Sähköpostiosoite']/following-sibling::dd[1]//a"
    )
    if (is.na(email_node)) {
      email_node <- html_element(doc, "a[href^='mailto']")
    }
    email <- NA_character_
    if (!is.na(email_node)) {
      href <- html_attr(email_node, "href")
      if (!is.na(href)) {
        email <- href %>% str_replace("^mailto:", "") %>% URLdecode()
      } else {
        email <- xml_text(email_node)
      }
      email <- str_trim(email)
    }

    # Käyttäjätunnus
    data_userids <- html_elements(doc, "[data-userid]") %>%
      html_attr("data-userid") %>%
      suppressWarnings(as.integer()) %>%
      as.character() %>%
      unique() %>%
      discard(is.na)

    user_id <- NA_character_
    if (length(data_userids) > 0) {
      user_id <- data_userids[1]
    } else {
      hrefs     <- html_elements(doc, "a") %>% html_attr("href") %>% discard(is.na)
      ids_found <- str_extract_all(hrefs, "(?<=\\b(?:id|user1|user2)=)\\d+") %>% unlist()
      if (length(ids_found) > 0) {
        user_id <- names(sort(table(ids_found), decreasing = TRUE))[1]
      }
    }

    # Paikkakunta
    paikkakunta_tbl <- tibble(
      dd = html_nodes(doc, "dt") %>% html_text(trim = TRUE),
      dt = html_nodes(doc, "dd") %>% html_text(trim = TRUE)
    ) %>% filter(dd == "Paikkakunta")

    if (nrow(paikkakunta_tbl) == 0) {
      paikkakunta_tbl <- tibble(dd = "Paikkakunta", dt = "Ei_paikkakuntaa")
    }

    datalist[[i]] <- tibble(
      filename    = basename(file),
      email       = email %||% NA_character_,
      user_id     = user_id %||% NA_character_,
      paikkakunta = paikkakunta_tbl$dt[1]
    )
  }

  users <- bind_rows(datalist) %>% mutate(id = row_number())
  users
}
