#' lees een csv2-bestand in
#'
#' Hulpfunctie om csv-bestanden (met ';' als kolomscheidingsteken) met read_csv2
#' in te lezen. Er wordt gecontroleerd of het bestand bestaat en van het juiste
#' type is.
#' @param map De naam van de map waarin het bestand zit
#' @param bestand De naam van het csv-bestand
#' @param foutboodschap De boodschap die vermeld wordt,
#' als het bestand ontbreekt.
#' @inheritParams readr::read_delim
#' @importFrom readr read_csv2
#' @importFrom base file.path

lees_csv2 <- function(map, bestand,
                      foutboodschap = NULL, col_types = NULL) {
  pad <- file.path(map, bestand)
  # 1. Controle op bestaan van bestand
  if (is.null(foutboodschap)) {
    foutboodschap <- paste("Kritieke fout: Bestand", bestand, "ontbreekt in map", map)
  }
  if (!file.exists(pad)) {
    stop(foutboodschap)
  }

  # 2. Controle op extensie (.csv)
  if (tolower(tools::file_ext(pad)) != "csv") {
    stop(paste("Fout: Het bestand", bestand, "heeft geen .csv extensie."))
  }

  # 3. Controle op scheidingsteken (';')
  # We lezen alleen de eerste regel in om de structuur te checken
  eerste_regel <- readLines(pad, n = 1)
  if (!grepl(";", eerste_regel)) {
    warning(paste(
      "Waarschuwing: Geen ';' gevonden in de eerste regel van",
      bestand, ". Is dit wel een csv2 bestand?"
    ))
  }

  # Inlezen van het bestand
  if (is.null(col_types)) {
    return(read_csv2(pad, show_col_types = FALSE))
  } else {
    return(read_csv2(pad, col_types = col_types))
  }
}
