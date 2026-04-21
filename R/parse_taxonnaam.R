#' @title vereenvoudig de soortnaam door de auteurs te verwijderen
#'
#' @description Deze functie vereenvoudigt de opgegeven taxonnaam/taxonnamen
#' door de auteursnaam te verwijderen. Ze is overgenomen uit het package LSVI en
#' is gebaseerd op de functie name_parse uit het package rgbif.
#'
#' @param taxonnaam String. Wetenschappelijke naam of namen die vereenvoudigd
#' moeten worden
#'
#' @param parse_type String. Welk type vereenvoudiging teruggegeven moet worden.
#' Standaard is dit 'canonicalnamewithmarker', andere opties zijn
#' 'canonicalname' en 'canonicalnamecomplete'
#'
#' @return Deze functie geeft de vereenvoudigde naam of namen terug (als string
#' of vector van strings)
#'
#' @importFrom rgbif name_parse
#'

parse_taxonnaam <- function(taxonnaam, parse_type = "canonicalnamewithmarker") {
  if (length(taxonnaam) == 0) {
    return(as.character("geen soort opgegeven (lege vector)"))
  }
  if (all(is.na(taxonnaam))) {
    return(rep(NA, length(taxonnaam)))
  }
  taxonnaam <- preparse_taxonnaam(taxonnaam)

  resultaat_parser <- name_parse(taxonnaam)
  resultaat <- postparse_taxonnaam(resultaat_parser, parse_type)

  return(resultaat)
}
