#' @title parse_taxonnaam deel 1
#'
#' @description Deze functie bevat het eerste deel van de functie
#' parse_taxonnaam, namelijk de omzettingen van de naam die moeten gebeuren
#' voor het parsen door de gbif-service. Ze is overgenomen uit het package LSVI.
#'
#' @inheritParams parse_taxonnaam
#'
#' @return Deze functie geeft de licht aangepaste naam of namen terug (als
#' string of vector van strings)
#'
#' @noRd
#'

preparse_taxonnaam <- function(taxonnaam) {
  taxonnaam <- gsub("v\\.d\\.", "v. d.", taxonnaam)
  taxonnaam <- gsub("((\\s|\\()v)\\.", "\\1an", taxonnaam)
  taxonnaam <- gsub(" auct\\. non ", " ", taxonnaam)
  taxonnaam <- gsub(" non ", " Non", taxonnaam)
  taxonnaam <- gsub(" auct\\. ", " Auct. ", taxonnaam)
  taxonnaam <- gsub(" auct\\.$", " Auct.", taxonnaam)
  taxonnaam <- gsub(" auct\\., ", " ", taxonnaam)
  taxonnaam <- gsub(" den ", " Den ", taxonnaam)
  taxonnaam <- gsub(" an ", " An ", taxonnaam)
  taxonnaam <- gsub(" anon ", " Anon ", taxonnaam)
  taxonnaam <- gsub("\\sf\\.$", "", taxonnaam)
  taxonnaam <- gsub("\\sf\\.\\)", ")", taxonnaam)
  taxonnaam <- gsub(" nom\\. illegit\\.$", " nom. illeg.", taxonnaam)
  taxonnaam <- gsub(" nom\\. superfl\\.$", "", taxonnaam)
  taxonnaam <- gsub(" nom\\. conf\\.$", "", taxonnaam)
  taxonnaam <- gsub(" sensu lato$", " s.l.", taxonnaam)
  taxonnaam <- gsub(" pro parte$", "", taxonnaam)
  taxonnaam <-
    gsub("^([A-Z][a-z]+\\s[a-z]+)(\\/[a-z]+)+$", "\\1 s.l.", taxonnaam)
  taxonnaam <-
    gsub(
      "^([A-Z][a-z]+)\\s([A-Z][a-z]*\\.?)\\s(subg.(\\s[A-Z][a-z]+\\.?)(\\s?[A-Z]?[a-z]*\\.?)*)$", # nolint
      "\\1 \\3", taxonnaam
    )
  taxonnaam <-
    gsub("\\scv\\.\\s([A-Z][a-z]+(\\s[a-z]+)?)$", " '\\1'", taxonnaam)

  return(taxonnaam)
}
