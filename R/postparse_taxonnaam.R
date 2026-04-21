#' @title parse_taxonnaam deel 3
#'
#' @description Deze functie bevat het derde deel van de functie
#' parse_taxonnaam, namelijk de omzettingen van de naam die nog moeten gebeuren
#' na het doorlopen van de gbif-service.
#'
#' @inheritParams parse_taxonnaam
#'
#' @param resultaat_parser Dataframe dat een resultaat is dat teruggegeven is
#' door de gbif-service
#'
#' @return Deze functie geeft de licht aangepaste naam of namen terug (als
#' string of vector van strings)
#'
#' @noRd
#'

postparse_taxonnaam <-
  function(resultaat_parser, parse_type) {
    if ("sensu" %in% colnames(resultaat_parser)) {
      resultaat_parser[, c(parse_type)] <-
        trimws(
          paste(
            resultaat_parser[, c(parse_type)],
            ifelse(
              is.na(resultaat_parser$sensu) | resultaat_parser$sensu != "s.l.",
              "", "groep"
            )
          )
        )
    }
    resultaat <- resultaat_parser[, c(parse_type)]
    resultaat <- gsub("\U00D7", "x ", resultaat)
    resultaat <- gsub("^NA$", NA, resultaat)

    return(resultaat)
  }
