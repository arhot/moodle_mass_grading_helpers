# helpers_plagiarism.R
#
# Funktiot tekstisamankaltaisuuden tunnistamiseen (plagiaatintarkistus)
# Jaccard-samankaltaisuuden avulla.
#
# Riippuvuudet:
source("helpers_read_files.R")

library(dplyr)
library(stringr)
library(text2vec)   # install.packages("text2vec")


# Esikäsittelee tekstit: muuttaa pieniksi, poistaa erikoismerkit,
# tiivistää välilyönnit.
prep_fun <- function(x) {
  x <- str_to_lower(x)
  x <- str_replace_all(x, "[^[:alnum:]]", " ")
  str_replace_all(x, "\\s+", " ")
}


# Rakentaa Jaccard-samankaltaisuusmatriisin tekstijoukosta.
#
# assignments : data frame, jossa sarake `teksti` ja `id`
#
# Palauttaa sparseMatrix-olion (samankaltaisuusmatriisi)
prep_files <- function(assignments) {
  assignments$teksti_clean <- prep_fun(assignments$teksti)

  it <- itoken(assignments$teksti_clean, progressbar = FALSE)
  v  <- create_vocabulary(it)
  v  <- prune_vocabulary(v, doc_proportion_max = 0.1, term_count_min = 5)
  vectorizer <- vocab_vectorizer(v)

  dtm <- create_dtm(it, vectorizer)
  sim2(dtm, dtm, method = "jaccard", norm = "none")
}


# Poimii samankaltaiset tekstiparit matriisista.
#
# d1_d2_jac_sim : samankaltaisuusmatriisi (prep_files-funktion tulos)
# threshold     : samankaltaisuusraja (0–1), jonka ylittävät parit palautetaan
# data          : alkuperäinen assignments-data frame (id- ja name-sarakkeet)
#
# Palauttaa data framen: row, col, name (opiskelijan nimi), teksti
get_names_of_similarities <- function(d1_d2_jac_sim, threshold = 0.3, data) {
  similarity_matrix <- as.matrix(d1_d2_jac_sim)
  diag(similarity_matrix) <- 0
  tuplat    <- which(similarity_matrix > threshold, arr.ind = TRUE) %>% as.data.frame()
  tuplattuna <- left_join(tuplat, data, by = c("row" = "id"))
  tuplattuna
}


# Suorittaa täyden plagiaatintarkistuksen hakemistosta luetuille teksteille.
#
# path_used  : hakemiston polku (luetaan read_directory_files_essay_files_in_separate_directories:lla)
# threshold  : samankaltaisuusraja (oletus: 0.3)
# file_name  : luettava tiedosto kustakin hakemistosta (oletus: "verkkoteksti.html")
#
# Palauttaa data framen samankaltaisista pareista
do_analysis <- function(path_used, threshold = 0.3, file_name = "verkkoteksti.html") {
  assignm      <- read_directory_files_essay_files_in_separate_directories(path_used, file_name)
  prepped      <- prep_files(assignm)
  get_names_of_similarities(prepped, threshold = threshold, data = assignm)
}
